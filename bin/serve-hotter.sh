#!/bin/sh
# Stepper server on hotter: SSO via home-auth, bearer token for clients.
# Run under dtach: dtach -n /tmp/stepper.sock ./bin/serve-hotter.sh
export PATH="$HOME/.local/share/mise/shims:/opt/local/bin:/usr/local/bin:/usr/bin:/bin"
# Issuer, OIDC credentials and API tokens come from home-auth over
# loopback at startup; STEPPER_BASE_URL is what turns SSO on.
export STEPPER_BASE_URL="https://stepper.hotter.myaddr.dev"
cd "$(dirname "$0")/.."
exec clojure -M:run serve 8323
