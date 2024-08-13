# Source code of the `snapshot` function, injected into the wrapped script

args_to_json() {
  json_array="["

  for arg in "${@}"; do
      json_array+=$(jq -Rn --arg str "$arg" '$str')
      json_array+=","
  done

  json_array="${json_array%,}]"

  echo "$json_array"
}

args_to_json snapshot "$@" >&${_taskrunner_request_pipe}
read -u ${_taskrunner_response_pipe} reply
eval "$reply"
