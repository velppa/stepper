(ns stepper.resource
  "Task resources, addressed by ARN (arn:localhost:stepper:::<handler>:<command>).

  A resource takes the Task's arguments (a map) and returns the task
  result, or throws ex-info with :error/:cause for a task failure."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn arn-client
  "Client segment of a Task ARN - who executes it."
  [arn]
  (second (str/split arn #":")))

(defn- arn-handler-command
  "\"<handler>:<command>\" tail of a Task ARN - what to execute."
  [arn]
  (->> (str/split arn #":") (take-last 2) (str/join ":")))

(defmulti invoke
  "Invoke resource ARN with ARGUMENTS.  Dispatches on the ARN's
  <handler>:<command> tail, so the same resource serves any client
  name.  CTX carries {:on-event :state-name} so a resource can report
  progress into the execution history."
  (fn [arn _arguments _ctx] (arn-handler-command arn)))

(defmethod invoke :default [arn _arguments _ctx]
  (throw (ex-info (str "Unknown resource: " arn)
                  {:error "States.TaskFailed"
                   :cause (str "no resource registered for " arn)})))

(defn- run-process
  "Run ARGV, honoring cwd/env/timeout_seconds.
  Result: {stdout, stderr, exit_code}; non-zero exit or timeout throws."
  [argv {:strs [cwd env timeout_seconds]}]
  (let [builder (ProcessBuilder. ^java.util.List argv)]
    ;; no stdin: commands must not wait on input that never comes
    (.redirectInput builder (io/file "/dev/null"))
    (when cwd (.directory builder (io/file cwd)))
    (when env (.putAll (.environment builder) (update-keys env str)))
    (let [process (.start builder)
          stdout (future (slurp (.getInputStream process)))
          stderr (future (slurp (.getErrorStream process)))]
      (when-not (try
                  (if timeout_seconds
                    (.waitFor process (long timeout_seconds) java.util.concurrent.TimeUnit/SECONDS)
                    (do (.waitFor process) true))
                  ;; interrupt = the task was stopped: take the process down with it
                  (catch InterruptedException _
                    (.destroyForcibly process)
                    (throw (ex-info "command stopped"
                                    {:error "States.Aborted"
                                     :cause (str (first argv) " stopped")}))))
        (.destroyForcibly process)
        (throw (ex-info "command timed out"
                        {:error "States.Timeout"
                         :cause (str (first argv) " exceeded " timeout_seconds "s")})))
      (let [exit (.exitValue process)
            result {"stdout" @stdout "stderr" @stderr "exit_code" exit}]
        (when-not (zero? exit)
          (throw (ex-info "command failed"
                          {:error "States.TaskFailed"
                           :cause (str "exit " exit ": " @stderr)
                           :result result})))
        result))))

;; Shell command runner.  Arguments:
;;   command         - string, run via /bin/sh -c
;;   cwd             - working directory
;;   env             - extra environment variables
;;   timeout_seconds - kill and fail with States.Timeout when exceeded
(defmethod invoke "shell:runCommand"
  [_ {:strs [command] :as arguments} _ctx]
  (run-process ["/bin/sh" "-c" (str command)] arguments))

(defn- unanswered-question?
  "True when TEXT, trimmed, ends with a question mark.  claude -p runs
  with stdin on /dev/null (see run-process) - there is no channel back
  for an answer, so a trailing question means the run stopped short of
  finishing rather than genuinely needing input no one can give it."
  [text]
  (and (string? text) (not (str/blank? text)) (str/ends-with? (str/trimr text) "?")))

;; Claude Code prompt runner: claude -p <prompt> --output-format json.
;; Arguments:
;;   prompt - the prompt text
;;   claude - path to the claude binary, default "claude" from PATH
;;   args   - list of extra CLI flags, e.g. ["--permission-mode" "bypassPermissions"]
;;   plus cwd/env/timeout_seconds as in runCommand.
;; The session id is generated here and passed via --session-id, and a
;; TaskStarted event carries it into the history before the run - so it
;; is known while the task runs and survives a failed run.
;; Result is the CLI's JSON object - the reply under "result" plus
;; session information (session_id, total_cost_usd, num_turns,
;; duration_ms, usage, ...) - with stderr/exit_code added; when stdout
;; is not a JSON object the raw {stdout, stderr, exit_code} is
;; returned, session_id added.  A reply ending in a question fails the
;; task with Claude.UnansweredQuestion instead of succeeding, since
;; nothing will ever answer it.
(defmethod invoke "claude:runPrompt"
  [_ {:strs [prompt claude args] :as arguments} {:keys [on-event state-name]}]
  (when-not (and (string? prompt) (not (str/blank? prompt)))
    (throw (ex-info "prompt required"
                    {:error "States.TaskFailed"
                     :cause "claude:runPrompt needs a non-empty prompt argument"})))
  (let [session-id (str (random-uuid))]
    (when on-event
      (on-event {:type "TaskStarted" :state-name state-name
                 :detail {"session_id" session-id}}))
    (let [result (run-process (into [(or claude "claude") "-p" prompt
                                     "--output-format" "json"
                                     "--session-id" session-id]
                                    (map str args))
                              arguments)
          parsed (try (json/parse-string (get result "stdout"))
                      (catch Exception _ nil))
          output (if (map? parsed)
                   (merge {"session_id" session-id} parsed
                          (select-keys result ["stderr" "exit_code"]))
                   (assoc result "session_id" session-id))
          reply (if (map? parsed) (get parsed "result") (get result "stdout"))]
      (if (unanswered-question? reply)
        (throw (ex-info "claude -p ended with a question"
                        {:error "Claude.UnansweredQuestion"
                         :cause (str "no one can answer it, so the run did not finish: " reply)
                         :result output}))
        output))))

;; Script file runner, via bash.  Arguments:
;;   file - path to a script file
;;   args - list of arguments
;;   plus cwd/env/timeout_seconds as in runCommand.
(defmethod invoke "shell:runFile"
  [_ {:strs [file args] :as arguments} _ctx]
  (let [f (io/file file)]
    (when-not (.isFile f)
      (throw (ex-info "file not found"
                      {:error "States.TaskFailed"
                       :cause (str file " does not exist")})))
    (run-process (into ["bash" (.getAbsolutePath f)] (map str args)) arguments)))
