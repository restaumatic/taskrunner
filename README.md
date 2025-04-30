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

## "Prime cache" mode

When migrating from another system behind a flag, it is sometimes desirable to build on the old system but still fill remote cache one the new one. For that occasion, a special "prime cache mode" is there.

It modifies the behavior in the following way:
- `snapshot` never downloads remote cache (incl. fuzzy) - to avoid overwriting stuff (which we assume is already built via another mechanism)
- `snapshot` always skips the job
- remote cache is uploaded, despite job being skipped

To use it, first build using another system, and the run `taskrunner` with `TASKRUNNER_PRIME_CACHE_MODE=1`

## Directory structure

- `$TASKRUNNER_STATE_DIRECTORY` (default: `/tmp/taskrunner`)
  - `locks` - global locks per job
    - `${jobName}.lock` - job lock file, job takes the lock when running
  - `hash` - hashes of inputs of already-done jobs
    - `${jobName}.hash` - first line is hash, rest is hash input (for debugging)
  - `builds` - build state directory for each build. Each toplevel invocation creates a subdirectory here.
    - `${buildId}` - state dir of a specific build. `buildId` is derived from the invocation time.
      - `logs` - logs produced by jobs in that build.
        - `${jobName}.log` - log, without ANSI sequences stripped
      - `results` - per-build cache of job results (we don't re-run jobs twice inside a build, even without `snapshot`)
        - `${jobName}` - file with status code of the job

## Configuration variables

- `TASKRUNNER_DEBUG` - whether to output debug messages to toplevel output. Note that debug messages are always written to per-task logs, regardless of this setting.
- `TASKRUNNER_LOG_INFO` - whether to output "info" messages to toplevel output. They are minimal messages, produced only when there's actually something to be done (including fetching from cache).
- more...

## Possible features

- Support stdin? For now redirected from `/dev/null`
- Quiet output like `gradle`, only report what is running and progress, not full output, and no output if nothing to do
- Marker files - additional hash file in `.stack-work`, `node_modules` etc., so that if that dir is cleared, we redo the action
  - Or: remember hash of some of the output files and check they're still there
  - Only some because there can be benign changes
- Can dump enough info to reproduce failures
  - For example: hashes of inputs, caches etc.
- Generate a trace (otlp for analysis, or render to a gantt chart)

## Things we should do better than previous version

- less confusing output for cache miss (no "error")
- ??? Something, can't recall now
- `--cmd` replaced with `--raw`, since we can't really execute in the context of the original script

## Things to handle

- Task leaks stdin/out/err handle - have a timeout on draining output
- Parallel task failed and we're killed - report status correctly
- `snapshot` - how to communicate with controller process?
  - pipe and pass fd to child process?
  - named pipe and pass name to child process via env?
- Nested tasks - each should write to original stdout
- Unmerged files when hashing
- bad usage of `snapshot` - e.g. called twice
- why `ls-tree -r` is needed - git option of quoting
- save cache tar error

## Misc TODO

- Better output of error messages (to normal streams)
- String/Text unification
- Debugging - show hash input
- More specialized tests for input handling
- In parent task's log, add reference to nested log file
- Debugging aid: when replacing saved hash, show diff between old and new hash input (or save old hash input to compare)
- Bug: pendolino sometimes rebuilds randomly with scripts/UPDATE
  - Probable cause: helper generation races with its input hashing
    - nope, it generates in another directory
- More tests for interaction between remote cache and local hash, especially:
  - restoring remote cache should also store local hash, but not store remote cache again
- test for root dir != cwd
- test for commit status
- Somehow test content-type in log upload?

## Output principles (output generated by runner itself)

- "quiet" operation - no output except when an error happens
- standard operation ("info" mode):
  - when job does nothing (already done locally), no output
  - when resuming from cache, output one line for start (so that we know something's happening), one for done
  - when running, output one line for start, one for done - only for snapshottable jobs
- debug: log everything (maybe later categorize)

## Performance goals

- Previous impl no-op tests/scripts/UPDATE: ~1.6s
- Current impl no-op tests/scripts/UPDATE: ~2.3s

## Snapshot Command Flags

The `snapshot` command supports the following flags:

- `--outputs`: Specifies files to be cached in remote cache.
- `--cache-success`: Use remote cache even when no outputs are specified. The task is not rerun if it succeeded previously with the same inputs. Useful e.g. for test suites.
- `--raw`: Specifies raw input strings that are used to compute the task's hash.
- `--fuzzy-cache`: Enables the use of a fuzzy cache, which attempts to restore from a cache of a similar task if the exact cache is not available.
- `--cache-root`: Specifies the root directory for caching. Use when caching things outside of the repository, e.g. `~/.stack`.
- `--cache-version`: Specifies a version string for the cache. `--fuzzy-cache` will not download cache from another version, allowing clean breaks when making big changes, e.g. upgrading a compiler.
- `--commit-status`: Enables reporting of the task's status to a commit status system, such as GitHub checks.
- `--long-running`: Indicates that the task is expected to run for a long time (e.g. a server). Currently doens't have any effect though, TODO: can we remove it?
