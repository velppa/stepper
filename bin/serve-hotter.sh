#!/bin/sh
# Stepper server on hotter: SSO via home-auth, bearer token for clients.
# Run under dtach: dtach -n /tmp/stepper.sock ./bin/serve-hotter.sh
export PATH="$HOME/.local/share/mise/shims:/opt/local/bin:/usr/local/bin:/usr/bin:/bin"
export STEPPER_OIDC_ISSUER="https://auth.hotter.myaddr.dev/oidc"
export STEPPER_OIDC_CLIENT_ID="stepper"
export STEPPER_OIDC_CLIENT_SECRET="189b3a51939f1860810909dcc6f0ec0d976b7a3bc206e0dd"
export STEPPER_BASE_URL="https://stepper.hotter.myaddr.dev"
export STEPPER_API_TOKEN="269f3d44c2bd1f1012f884319583a618972d1be8a6209dc3"
cd "$(dirname "$0")/.."
exec clojure -M:run serve 8323
