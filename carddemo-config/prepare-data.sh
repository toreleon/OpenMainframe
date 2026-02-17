#!/usr/bin/env bash
# Converts CardDemo ASCII text data files (newline-delimited) into
# binary fixed-length record files that load_vsam_data can consume.
# Each line is padded (or truncated) to the exact COBOL record length.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SRC_DIR="${SCRIPT_DIR}/../aws-mainframe-modernization-carddemo/app/data/ASCII"
DST_DIR="${SCRIPT_DIR}/data"

mkdir -p "$DST_DIR"

# file_base:record_length (record lengths verified against EBCDIC file sizes)
declare -a FILES=(
    "acctdata:300"
    "carddata:150"
    "custdata:500"
    "cardxref:50"
    "dailytran:350"
    "discgrp:50"
    "tcatbal:50"
    "trancatg:60"
    "trantype:60"
)

for entry in "${FILES[@]}"; do
    base="${entry%%:*}"
    reclen="${entry##*:}"
    src="$SRC_DIR/${base}.txt"
    dst="$DST_DIR/${base}.dat"

    if [ ! -f "$src" ]; then
        echo "SKIP: $src not found"
        continue
    fi

    python3 -c "
import sys
reclen = int(sys.argv[1])
with open(sys.argv[2], 'r') as f, open(sys.argv[3], 'wb') as out:
    count = 0
    for line in f:
        line = line.rstrip('\n').rstrip('\r')
        if not line:
            continue
        # Pad or truncate to exact record length
        rec = line.ljust(reclen)[:reclen]
        out.write(rec.encode('ascii'))
        count += 1
    print(f'Converted: {sys.argv[4]}.txt -> {sys.argv[4]}.dat ({count} records x {reclen} bytes = {count*reclen} bytes)')
" "$reclen" "$src" "$dst" "$base"
done

echo ""
echo "All data files converted to $DST_DIR"
