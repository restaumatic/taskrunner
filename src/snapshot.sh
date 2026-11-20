# Source code of the `snapshot` function, injected into the wrapped script

jq -n -c '$ARGS.positional' --args -- snapshot "$@" >&${_taskrunner_request_pipe}
read -t "${TASKRUNNER_SNAPSHOT_TIMEOUT:-3600}" -u ${_taskrunner_response_pipe} reply || {
  if (( $? > 128 )); then
    echo "taskrunner: snapshot in job ${_taskrunner_job_name} got no reply within ${TASKRUNNER_SNAPSHOT_TIMEOUT:-3600}s" >&2
  else
    echo "taskrunner: snapshot in job ${_taskrunner_job_name} got no reply, taskrunner closed the response pipe" >&2
  fi
  exit 1
}
eval "$reply"
