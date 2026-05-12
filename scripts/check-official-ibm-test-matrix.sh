#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MATRIX="$ROOT_DIR/docs/OFFICIAL_IBM_SIMULATION_TEST_MATRIX.md"

if [[ ! -f "$MATRIX" ]]; then
  echo "missing matrix: $MATRIX" >&2
  exit 1
fi

missing=0

while IFS= read -r manifest; do
  crate="$(basename "$(dirname "$manifest")")"
  if ! grep -q "\`$crate\`" "$MATRIX"; then
    echo "missing crate in matrix: $crate" >&2
    missing=1
  fi
done < <(find "$ROOT_DIR/crates" -mindepth 2 -maxdepth 2 -name Cargo.toml | sort)

ibm_links="$(grep -Eo 'https://(www\.)?ibm\.com/(docs|support|products)[^)]+' "$MATRIX" | sort -u | wc -l | tr -d ' ')"
if [[ "$ibm_links" -lt 10 ]]; then
  echo "expected at least 10 official IBM links, found $ibm_links" >&2
  missing=1
fi

if ! grep -q '## Crate Matrix' "$MATRIX"; then
  echo "matrix is missing the crate matrix section" >&2
  missing=1
fi

if [[ "$missing" -ne 0 ]]; then
  exit 1
fi

echo "Official IBM simulation test matrix covers all crates and has $ibm_links IBM links."
