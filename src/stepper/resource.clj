(ns stepper.resource
  "Task resources, addressed by SRN (Stepper Resource Name).

  A resource takes the Task's arguments (a map) and returns the task
  result, or throws ex-info with :error/:cause for a task failure."
  (:require [clojure.java.io :as io]))

(defmulti invoke (fn [srn _arguments] srn))

(defmethod invoke :default [srn _arguments]
  (throw (ex-info (str "Unknown resource: " srn)
                  {:error "States.TaskFailed"
                   :cause (str "no resource registered for " srn)})))

(defn- run-process
  "Run ARGV, honoring cwd/env/timeout_seconds.
  Result: {stdout, stderr, exit_code}; non-zero exit or timeout throws."
  [argv {:strs [cwd env timeout_seconds]}]
  (let [builder (ProcessBuilder. ^java.util.List argv)]
    (when cwd (.directory builder (io/file cwd)))
    (when env (.putAll (.environment builder) (update-keys env str)))
    (let [process (.start builder)
          stdout (future (slurp (.getInputStream process)))
          stderr (future (slurp (.getErrorStream process)))]
      (when-not (if timeout_seconds
                  (.waitFor process (long timeout_seconds) java.util.concurrent.TimeUnit/SECONDS)
                  (do (.waitFor process) true))
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
(defmethod invoke "srn:local:shell:::shell:runCommand"
  [_ {:strs [command] :as arguments}]
  (run-process ["/bin/sh" "-c" (str command)] arguments))

;; Executable file runner.  Arguments:
;;   file - path to an executable file
;;   args - list of arguments
;;   plus cwd/env/timeout_seconds as in runCommand.
(defmethod invoke "srn:local:shell:::shell:runFile"
  [_ {:strs [file args] :as arguments}]
  (let [f (io/file file)]
    (when-not (.isFile f)
      (throw (ex-info "file not found"
                      {:error "States.TaskFailed"
                       :cause (str file " does not exist")})))
    (when-not (.canExecute f)
      (throw (ex-info "file not executable"
                      {:error "States.TaskFailed"
                       :cause (str file " is not executable")})))
    (run-process (into [(.getAbsolutePath f)] (map str args)) arguments)))
