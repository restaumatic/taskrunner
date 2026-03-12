# Release a new version of taskrunner

Argument: $ARGUMENTS (the new version number, e.g. "0.18.0.6")

## Instructions

1. Validate the version argument:
   - If no version is provided, read the current version from `package.yaml` and suggest the next patch version (increment the last number). Ask the user to confirm before proceeding.
   - The version should be in the format `X.Y.Z.W` (four dot-separated numbers).

2. Check for a clean working tree (`git status`). If there are uncommitted changes, stop and inform the user.

3. Update the version in both files:
   - `package.yaml`: the `version:` field (line 2)
   - `taskrunner.cabal`: the `version:` field (line 8)

4. Commit with message `v<VERSION>` (e.g. `v0.18.0.6`).

5. Create a lightweight git tag `v<VERSION>`.

6. Ask the user if they want to push the commit and tag to trigger the release. If yes, run:
   ```
   git push && git push --tags
   ```
   The push triggers the GitHub Actions release workflow (`.github/workflows/release.yml`) which builds, tests, and publishes the release on GitHub.
