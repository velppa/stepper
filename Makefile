# Stepper: server on hotter, client on m4pro.

.PHONY: start-server start-client

# SSO via home-auth, bearer token for clients; STEPPER_BASE_URL turns SSO
# on. Run under dtach: dtach -n .dtach/stepper.sock make start-server
start-server:
	PATH="$$HOME/.local/share/mise/shims:/opt/local/bin:/usr/local/bin:/usr/bin:/bin" \
	STEPPER_BASE_URL="https://stepper.hotter.myaddr.dev" \
	clojure -M:run serve 8323

start-client:
	STEPPER_API_TOKEN="$$(TMPDIR=$$(getconf DARWIN_USER_TEMP_DIR) emacsclient -e '(cadr (auth-source-user-and-password "stepper.hotter.myaddr.dev" "velppa^m4pro"))' | sed -e 's/^"//' -e 's/"$$//')" \
	clojure -M:client https://stepper.hotter.myaddr.dev m4pro
