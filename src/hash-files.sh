# Script called from main executable to compute hash of git pathspecs

set -euo pipefail

GIT_INDEX_FILE="$(mktemp)"
trap 'rm -f "${GIT_INDEX_FILE}"' 0 1 2 3 15
export GIT_INDEX_FILE
cp "$(git rev-parse --git-dir)/index" "${GIT_INDEX_FILE}"
tree="$(git add "$@" && git write-tree)" || exit 1
git ls-tree -r --full-name "$tree" "$@" | sha1sum - | awk '{print $1}'
