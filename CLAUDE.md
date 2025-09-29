# Claude Code Workflow Instructions for taskrunner

## Project Overview
This is a Haskell project that implements a task runner with caching, parallel execution, and remote storage capabilities. It uses Stack for build management and tasty-golden for snapshot testing.

## Project Structure
- `src/` - Haskell source code (main library)
- `app/` - Executable main entry point
- `test/` - Test suite
  - `test/t/` - Golden test files (`.txt` input, `.out` expected output)
  - `test/Spec.hs` - Main test runner
  - `test/FakeGithubApi.hs` - Mock GitHub API for testing
- `package.yaml` - Haskell package configuration (Stack format)
- `stack.yaml` - Stack resolver and build configuration
- `taskrunner.cabal` - Generated cabal file (don't edit directly)

## Build and Development Workflow

### Building the Project
```bash
stack build
```

### Running Tests
```bash
# Run all tests (may be slow)
stack test

# Run tests, skipping slow ones
export SKIP_SLOW_TESTS=1
stack test

# Run specific test by pattern
stack test --test-arguments "--pattern hello"

# List all available tests
stack test --test-arguments "--list-tests"
```

### Accepting Golden Test Changes
When golden tests fail due to expected output changes:
```bash
stack test --test-arguments --accept
```

### Test Structure
- Test files are in `test/t/` directory
- Each test has:
  - `.txt` file - shell script to execute
  - `.out` file - expected output (golden file)
- Tests run through the taskrunner executable
- Special comments in `.txt` files control test behavior:
  - `# check output` - check stdout/stderr
  - `# check github` - check GitHub API calls
  - `# no toplevel` - don't wrap in taskrunner
  - `# s3` - enable S3 testing
  - `# github keys` - provide GitHub credentials

## Key Commands for Development

### Building
- `stack build` - Build the project
- `stack build --fast` - Fast build (less optimization)
- `stack clean` - Clean build artifacts

### Testing
- `stack test` - Run all tests
- `stack test --test-arguments --accept` - Accept golden test changes
- `SKIP_SLOW_TESTS=1 stack test` - Skip slow tests

### Running the executable
- `stack exec taskrunner -- [args]` - Run the built executable
- `stack run -- [args]` - Build and run in one command

## Notes
- This project uses tasty-golden for snapshot/golden file testing
- The test suite includes integration tests that verify taskrunner behavior
- Some tests require S3 credentials and GitHub API tokens (set via environment variables)
- The project uses Universum as an alternative Prelude
- Build output and temporary files are in `.stack-work/`