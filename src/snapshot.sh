# Source code of the `snapshot` function, injected into the wrapped script

jq -n -c '$ARGS.positional' --args -- snapshot "$@" >&${_taskrunner_request_pipe}
read -u ${_taskrunner_response_pipe} reply
eval "$reply"
