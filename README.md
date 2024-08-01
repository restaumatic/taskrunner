# taskrunner

## Features

- Runs tasks (which are defined as shell scripts)
- Ensures only one instance of a task runs at a time (globally in the whole system) - using file-based locks
- Tasks can run in parallel
- Output from parallel tasks is correctly annotated with original task name
- Output lines are timestamped
- Parallelism can be nested
  - But task name annotations are flattened, i.e. are not added repeatedly at each level
- Output from each task is also collected to a separate file, to easier debug that particular task
- Tasks can have specified inputs
  - Input can be a file, an environment variable, or result of an arbitrary command
- If a task is invoked again for the same input (on a given machine), it is not re-run
- Tasks can have specified outputs (files)
- Task outputs can be cached in an object store
- If a task is invoked with inputs for which it was already computed on CI, the result is fetched from remote cache and task is not re-run
- If a task is called multiple times in the same execution, it is only executed once.
- When running on CI:
   - Task output (stdout and stderr) is uploaded to an object store after task is finished
   - task status is reported as a GitHub _check_ for a commit
     - _pending_ while it's running
     - _success_ or _failed_ when finished
   - GitHub check details link points to the uploaded task output

- Unstable inputs (i.e. inputs that have changed during the job execution) are detected
  - TODO: do something about input-output files like package-lock.json

- Tasks can have command-line arguments
  - But some options are meant for the task runner, such as `-f`

- Can force reexecution of a specific task (excl. dependencies) (`-f`)
  - Note: in previous implementation `-f` forced reexecution of all tasks. This seems less useful, will be under another option.

- Task can be cancelled using SIGINT or SIGTERM, and state is maintained appropriately

- Fast - if there's nothing to do, returns quickly (<1s, ideally <300ms)

## Possible features

- Support stdin? For now redirected from `/dev/null`
- Quiet output like `gradle`, only report what is running and progress, not full output, and no output if nothing to do
- Marker files - additional hash file in `.stack-work`, `node_modules` etc., so that if that dir is cleared, we redo the action
  - Or: remember hash of some of the output files and check they're still there
  - Only some because there can be benign changes
- Can dump enough info to reproduce failures
  - For example: hashes of inputs, caches etc.

## Things we should do better than previous version

- less confusing output for cache miss (no "error")
- ??? Something, can't recall now

## Things to handle

- Task leaks stdin/out/err handle - have a timeout on draining output
- Parallel task failed and we're killed - report status correctly
- `snapshot` - how to communicate with controller process?
  - pipe and pass fd to child process?
  - named pipe and pass name to child process via env?
- Nested tasks - each should write to original stdout
