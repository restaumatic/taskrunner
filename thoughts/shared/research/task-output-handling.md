---
date: 2025-09-30T04:59:12.021Z
researcher: Claude
git_commit: 0492ba8e3b81af1fea412709e010a4e3f0f355ae
branch: quietmode
repository: taskrunner
topic: "Task Output Handling in taskrunner"
tags: [research, codebase, output-handling, logging, quiet-mode, agentic-workflows]
status: complete
last_updated: 2025-09-30
last_updated_by: Claude
---

# Research: Task Output Handling in taskrunner

**Date**: 2025-09-30T04:59:12.021Z
**Researcher**: Claude
**Git Commit**: 0492ba8e3b81af1fea412709e010a4e3f0f355ae
**Branch**: quietmode
**Repository**: taskrunner

## Overview

This document details how the taskrunner application handles output from tasks, including logging, streaming, output annotation mechanisms, and the quiet mode feature for agentic workflows.

## Architecture

The taskrunner uses a sophisticated multi-stream output handling system that:
1. Captures task stdout/stderr in real-time
2. Annotates output with task names and timestamps
3. Writes to both terminal and persistent log files
4. Supports nested task execution with proper output routing
5. Provides quiet mode for agentic workflows with conditional output display

## Core Components

### AppState Structure (`src/Types.hs`)

The `AppState` contains four key output handles:
- `toplevelStderr :: Handle` - Terminal output for the main process
- `subprocessStderr :: Handle` - Error output for subprocess operations
- `logOutput :: Handle` - File handle for persistent logging
- `quietBuffer :: IORef [ByteString]` - Buffer for quiet mode output collection

### Output Processing Flow

#### 1. Process Creation (`src/App.hs:147-155`)

When executing a task, taskrunner creates a subprocess with:
```haskell
(Nothing, Just stdoutPipe, Just stderrPipe, processHandle) <- Process.createProcess
  (proc args.cmd args.args) {
    std_in = UseHandle devnull,
    std_out = CreatePipe,
    std_err = CreatePipe
  }
```

#### 2. Async Stream Handlers (`src/App.hs:164-168`)

Three async handlers process different output streams:
```haskell
stdoutHandler <- async $ outputStreamHandler appState toplevelStdout "stdout" stdoutPipe
stderrHandler <- async $ outputStreamHandler appState toplevelStderr "stderr" stderrPipe
subprocessStderrHandler <- async $ outputStreamHandler appState toplevelStderr "stderr" subprocessStderrRead
```

#### 3. Stream Processing (`src/App.hs:353-357`)

The `outputStreamHandler` reads lines and delegates to `outputLine`:
```haskell
outputStreamHandler :: AppState -> Handle -> ByteString -> Handle -> IO ()
outputStreamHandler appState toplevelOutput streamName stream = do
  handle ignoreEOF $ forever do
    line <- B8.hGetLine stream
    outputLine appState toplevelOutput streamName line
```

### Output Annotation (`src/Utils.hs:18-38`)

The `outputLine` function is the core of output handling:

#### Features:
1. **Timestamp Addition**: Optional timestamps with format `%T` (HH:MM:SS)
2. **Dual Logging**: Writes to both log file and terminal
3. **Task Name Annotation**: Prefixes output with `[jobName]`
4. **Stream Identification**: Labels output as `stdout`, `stderr`, `debug`, `info`, etc.
5. **Conditional Output**: Respects debug/info logging settings
6. **Quiet Mode Buffering**: Conditionally buffers output based on task success/failure

#### Format:
- **Log file**: `{timestamp} {streamName} | {line}`
- **Terminal**: `{timestamp} [{jobName}] {streamName} | {line}`

#### Example Output:
```
14:23:42 [build-frontend] stdout | Building React components...
14:23:43 [build-frontend] stderr | Warning: Deprecated API usage
```

## Log File Management

### Location (`src/Utils.hs:122-123`)
```haskell
logFileName :: Settings -> BuildId -> JobName -> FilePath
logFileName settings buildId jobName =
  settings.stateDirectory </> "builds" </> toString buildId </> "logs" </> (jobName <> ".log")
```

### Structure
```
$TASKRUNNER_STATE_DIRECTORY/
  builds/
    {buildId}/
      logs/
        {jobName}.log
      results/
        {jobName}          # Exit code
```

### File Properties
- **Line buffering** enabled for real-time writes
- **Binary mode** for proper encoding handling
- **Automatic closure** when task completes

## Quiet Mode Feature

### Overview
Quiet mode suppresses task output from the terminal unless the task fails, designed for agentic workflows to reduce noise and save tokens.

### Behavior
- **Success**: Task output buffered and discarded, no terminal output
- **Failure**: Task output buffered and flushed to terminal for debugging
- **Always**: All output still written to log files regardless of mode

### Implementation (`src/Utils.hs:39-45`)
```haskell
if appState.settings.quietMode
  then do
    -- In quiet mode, add to buffer instead of outputting immediately
    modifyIORef appState.quietBuffer (formattedLine :)
  else
    -- Normal mode: output immediately
    B8.hPutStrLn toplevelOutput formattedLine
```

### Buffer Management (`src/Utils.hs:133-143`)
- `flushQuietBuffer`: Outputs buffered content to terminal (on failure)
- `discardQuietBuffer`: Clears buffer without output (on success)
- Buffer stored in reverse order, flushed in correct chronological order

### Exit Code Integration (`src/App.hs:176-180`)
```haskell
when appState.settings.quietMode do
  if exitCode == ExitSuccess
    then discardQuietBuffer appState  -- Success: discard buffered output
    else flushQuietBuffer appState toplevelStderr  -- Failure: show buffered output
```

### Nested Task Behavior
- Each taskrunner process maintains its own quiet buffer
- Only the failing process flushes its buffer to terminal
- Successful nested tasks remain quiet even when parent fails
- Provides targeted debugging - shows output from exactly the failing component

## Output Control Settings

### Environment Variables
- `TASKRUNNER_DEBUG=1` - Include debug messages in terminal output
- `TASKRUNNER_LOG_INFO=1` - Include info messages in terminal output
- `TASKRUNNER_DISABLE_TIMESTAMPS=1` - Disable timestamp prefixes
- `TASKRUNNER_OUTPUT_STREAM_TIMEOUT=N` - Timeout for reading output streams
- `TASKRUNNER_QUIET=1` - Enable quiet mode (suppress output unless task fails)

### Filtering Logic (`src/Utils.hs:32-38`)
```haskell
let shouldOutputToToplevel
      | streamName == "debug" = appState.settings.logDebug
      | streamName == "info" = appState.settings.logInfo
      | otherwise = True
```

## Parallel Task Support

### Task Name Flattening
- Nested tasks don't add repeated annotations
- All output maintains original task context
- Parent task logs reference nested task log files

### Async Processing
- Each stream gets dedicated async handler
- Prevents blocking on individual stream delays
- Proper exception handling with `ignoreEOF`

## Remote Cache Integration

### Log Upload
When `uploadLogs` is enabled:
- Task logs uploaded to object store after completion
- GitHub check details link to uploaded logs
- Content-type preserved for proper rendering

### Structure
- Logs uploaded with task hash as key
- Retrievable for debugging failed cache hits
- Integrated with commit status reporting

## Error Handling

### Stream Failures
- `ignoreEOF` handles normal stream closure
- Timeouts prevent hanging on unresponsive processes
- Graceful degradation when log files unavailable

### Process Management
- Output handlers cancelled when main process exits
- Proper cleanup of file handles and pipes
- Exception propagation for critical failures

## Key Design Principles

1. **Real-time Output**: No buffering delays for user feedback (except in quiet mode)
2. **Comprehensive Logging**: Everything logged for debugging
3. **Nested Task Support**: Proper routing for complex workflows
4. **Configurable Verbosity**: Users control output detail level
5. **Parallel Safety**: Concurrent tasks don't interfere
6. **Remote Integration**: Logs available for CI/CD analysis
7. **Agentic Workflow Support**: Quiet mode for clean, token-efficient automation

## Implementation Notes

### Code Locations
- Main logic: `src/App.hs` (lines 147-168, 353-357)
- Output formatting: `src/Utils.hs` (lines 18-45)
- Quiet mode buffer management: `src/Utils.hs` (lines 133-143)
- Exit code integration: `src/App.hs` (lines 176-180)
- Type definitions: `src/Types.hs` (lines 41-57)
- Log utilities: `src/Utils.hs` (lines 122-123)

### Dependencies
- `async` for concurrent stream processing
- `System.Process` for subprocess management
- `System.IO` for handle operations
- `Data.ByteString.Char8` for efficient line processing

### Performance Considerations
- Line-by-line processing minimizes memory usage
- Binary mode avoids encoding overhead
- Async handlers prevent stream blocking
- Buffering disabled for real-time output (except quiet mode)
- Quiet mode buffer size naturally limited by task output volume
- Buffer memory freed immediately after task completion