#!/usr/bin/env bash
# Launch the AWS CardDemo application on OpenMainframe CICS emulator.
#
# Usage: ./carddemo-config/run-carddemo.sh
#
# Test credentials:
#   User ID:  USER0001    Password: PASSWORD  (regular user)
#   User ID:  ADMIN001    Password: PASSWORD  (admin user)
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

BINARY="$ROOT_DIR/target/release/open-mainframe"
CARDDEMO="$ROOT_DIR/aws-mainframe-modernization-carddemo"
CONFIG="$SCRIPT_DIR"

# Build if needed
if [ ! -f "$BINARY" ]; then
    echo "Building open-mainframe..."
    cargo build --release --manifest-path "$ROOT_DIR/Cargo.toml"
fi

# Prepare data files if needed
if [ ! -f "$CONFIG/data/acctdata.dat" ]; then
    echo "Preparing data files..."
    "$CONFIG/prepare-data.sh"
fi

exec "$BINARY" cics \
  "$CARDDEMO/app/cbl/COSGN00C.cbl" \
  -I "$CARDDEMO/app/cpy" \
  -I "$CONFIG/copybooks" \
  -I "$ROOT_DIR/crates/open-mainframe-cics/copybooks" \
  --bms-dir "$CARDDEMO/app/bms" \
  --data "ACCTDAT=$CONFIG/data/acctdata.dat:11:300" \
  --data "CARDDAT=$CONFIG/data/carddata.dat:16:150" \
  --data "CUSTDAT=$CONFIG/data/custdata.dat:9:500" \
  --data "CARDXREF=$CONFIG/data/cardxref.dat:16:50" \
  --data "CXACAIX=$CONFIG/data/cardxref.dat:11:50:25" \
  --data "CARDAIX=$CONFIG/data/carddata.dat:11:150:16" \
  --data "USRSEC=$CONFIG/data/usrsec.dat:8:80" \
  --data "TRANSACT=$CONFIG/data/dailytran.dat:16:350" \
  --transid CC00=COSGN00C \
  --transid CM00=COMEN01C \
  --transid CA00=COADM01C \
  --transid CAVW=COACTVWC \
  --transid CAUP=COACTUPC \
  --transid CCLI=COCRDLIC \
  --transid CCDL=COCRDSLC \
  --transid CCUP=COCRDUPC \
  --transid CT00=COTRN00C \
  --transid CT01=COTRN01C \
  --transid CT02=COTRN02C \
  --transid CB00=COBIL00C \
  --transid CR00=CORPT00C \
  --transid CU00=COUSR00C \
  --transid CU01=COUSR01C \
  --transid CU02=COUSR02C \
  --transid CU03=COUSR03C \
  --theme classic
