#!/usr/bin/env bash
#
# evalPointerE2E.sh
#
# This script gets target and GT binary list path and output directory path
# for evaluating PointerAnalyzer. This script generate GT json file from GT
# binary list, and processes PointerAnalyzer each file. This script tracks
# result metric and logs per file and merge them after entire analysis.
#

set -u

# Construct base paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
CALLER_CWD="$PWD"

usage() {
  cat <<'EOF'
Usage:
  evalPointerE2E.sh \
    --targets <target-list> \
    --gt-binaries <ground-truth-binary-list> \
    --output <result-root> \
    [--gt-dir <dir>] \
    [--analysis-dir <dir>] \
    [--evaluation-dir <dir>] \
    [--log-dir <dir>] \
    [--merged-output <file>] \
    [--interRelation <0|1>] \
    [--skip-build]

Input lists contain one binary path per line. Empty lines and lines beginning
with '#' are ignored. Target and ground-truth binaries are paired by exact
basename.

--interRelation controls callee-summary application: 1 enables it and 0
disables it. The default is 1.
EOF
}

# Transform relative path to absolute path
absolute_path() {
  case "$1" in
    # Given path is already absolute path
    /*) printf '%s\n' "$1" ;;
    # If given path is relative path, combine with current pwd
    *) printf '%s/%s\n' "$CALLER_CWD" "$1" ;;
  esac
}

TARGET_LIST=""
GT_BINARY_LIST=""
OUTPUT_ROOT=""
GT_DIR=""
ANALYSIS_DIR=""
EVALUATION_DIR=""
LOG_DIR=""
MERGED_OUTPUT=""
INTER_RELATION=1
SKIP_BUILD=0

# Parse the arguments
while (($# > 0)); do
  case "$1" in
    --targets)
      TARGET_LIST="${2:-}"
      shift 2
      ;;
    --gt-binaries)
      GT_BINARY_LIST="${2:-}"
      shift 2
      ;;
    --output)
      OUTPUT_ROOT="${2:-}"
      shift 2
      ;;
    --gt-dir)
      GT_DIR="${2:-}"
      shift 2
      ;;
    --analysis-dir)
      ANALYSIS_DIR="${2:-}"
      shift 2
      ;;
    --evaluation-dir)
      EVALUATION_DIR="${2:-}"
      shift 2
      ;;
    --log-dir)
      LOG_DIR="${2:-}"
      shift 2
      ;;
    --merged-output)
      MERGED_OUTPUT="${2:-}"
      shift 2
      ;;
    --interRelation)
      INTER_RELATION="${2:-}"
      shift 2
      ;;
    --skip-build)
      SKIP_BUILD=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      printf 'Unknown option: %s\n' "$1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

# Check target and GT binary list path and output directory path are given
if [[ -z "$TARGET_LIST" || -z "$GT_BINARY_LIST" || -z "$OUTPUT_ROOT" ]]; then
  usage >&2
  exit 2
fi

# Check whether caller-callee relationship option is valid
if [[ "$INTER_RELATION" != 0 && "$INTER_RELATION" != 1 ]]; then
  printf \
    'Invalid --interRelation value: %s. Use 0 or 1.\n' \
    "$INTER_RELATION" >&2
  exit 2
fi

# Construct paths
TARGET_LIST="$(absolute_path "$TARGET_LIST")"
GT_BINARY_LIST="$(absolute_path "$GT_BINARY_LIST")"
OUTPUT_ROOT="$(absolute_path "$OUTPUT_ROOT")"

GT_DIR="$(absolute_path "${GT_DIR:-"$OUTPUT_ROOT/ground-truth"}")"
ANALYSIS_DIR="$(absolute_path "${ANALYSIS_DIR:-"$OUTPUT_ROOT/pointer"}")"
LOG_DIR="$(absolute_path "${LOG_DIR:-"$OUTPUT_ROOT/logs"}")"

EVALUATION_DIR="$(
  absolute_path "${EVALUATION_DIR:-"$OUTPUT_ROOT/pointer-evaluation"}"
)"
MERGED_OUTPUT="$(
  absolute_path \
    "${MERGED_OUTPUT:-"$EVALUATION_DIR/merged_evalResult.json"}"
)"

# Generate output directory and log file
mkdir -p "$GT_DIR" "$ANALYSIS_DIR" "$EVALUATION_DIR" "$LOG_DIR"
MASTER_LOG="$LOG_DIR/pointer_e2e.log"
: > "$MASTER_LOG"

# Print and log given input
# status <log>
status() {
  printf '%s\n' "$*"
  printf '%s\n' "$*" >> "$MASTER_LOG"
}

# Print and log fail case with given input
# fail <fail-log>
fail() {
  status "[FAILED] $*"
}

status "[CONFIG] interRelation=$INTER_RELATION"

# Check given json file fits with Json format
validate_json() {
  python3 -m json.tool "$1" >/dev/null 2>&1
}

# Execute given command and log the output into given log file.
# The name of log file is given by first argument.
# run_logged <log-file-path> <command>
run_logged() {
  local log_file="$1"
  shift # Remove first argument: $1
  {
    printf '$'
    printf ' %q' "$@"
    printf '\n'
    "$@"
  } >> "$log_file" 2>&1
}

# Declare Associative Array for tracking mapping from base name to path
declare -A TARGETS=()
declare -A GT_BINARIES=()
INPUT_ERRORS=0

# Read given binary list and construct mapping from base name to path
# load_list <binary-list-pah> <Associative-Array-name> <target|gt>
load_list() {
  local list_path="$1"
  local map_name="$2"
  local kind="$3"
  # paths refers given Associative Array. It can update given array
  local -n paths="$map_name"

  # Check file existency
  if [[ ! -f "$list_path" ]]; then
    fail "$kind list does not exist: $list_path"
    INPUT_ERRORS=$((INPUT_ERRORS + 1))
    return
  fi

  # Read each line of given list and construct given array(mapping)
  local line
  local name
  while IFS= read -r line || [[ -n "$line" ]]; do
    # Remove blanks
    line="${line%$'\r'}"
    line="${line#"${line%%[![:space:]]*}"}"
    line="${line%"${line##*[![:space:]]}"}"

    # Skip empty line and comment
    [[ -z "$line" || "$line" == \#* ]] && continue

    # Parse binary path and its base name
    line="$(absolute_path "$line")"
    name="$(basename "$line")"
    
    # Check duplicate binary exist or not
    if [[ -v "paths[$name]" ]]; then
      fail "duplicate $kind basename '$name': ${paths[$name]} and $line"
      INPUT_ERRORS=$((INPUT_ERRORS + 1))
    else
      # If unique binary, update mapping
      paths["$name"]="$line"
    fi
  done < "$list_path"
}

# Check dotnet exists
if ! command -v dotnet >/dev/null 2>&1; then
  fail "dotnet is not installed or not in PATH"
  exit 1
fi

# Check python3 used for post-processing exists
if ! command -v python3 >/dev/null 2>&1; then
  fail "python3 is not installed or not in PATH"
  exit 1
fi

# Load binary list and construct mapping
load_list "$TARGET_LIST" TARGETS "target"
load_list "$GT_BINARY_LIST" GT_BINARIES "gt"

# Check empty binary list
if ((${#TARGETS[@]} == 0)); then
  fail "target list contains no binary paths: $TARGET_LIST"
  INPUT_ERRORS=$((INPUT_ERRORS + 1))
fi

if ((${#GT_BINARIES[@]} == 0)); then
  fail "ground-truth binary list contains no binary paths: $GT_BINARY_LIST"
  INPUT_ERRORS=$((INPUT_ERRORS + 1))
fi

# Check all GT binaries have corresponding target binary
for name in "${!GT_BINARIES[@]}"; do
  if [[ ! -v "TARGETS[$name]" ]]; then
    fail "ground-truth binary has no target with basename '$name'"
    INPUT_ERRORS=$((INPUT_ERRORS + 1))
  fi
done

# Target or GT binary are not given. Stop processing.
if ((${#TARGETS[@]} == 0 || ${#GT_BINARIES[@]} == 0)); then
  status "[SUMMARY] successful=0 failed=$INPUT_ERRORS"
  exit 1
fi

# Build PointerAnalyzer and Checker. Checker is used for extracting GT.
if ((SKIP_BUILD == 0)); then
  status "[BUILD] PointerAnalyzer"
  if ! dotnet build "$REPO_ROOT/src/PointerAnalyzer.fsproj" \
      >> "$MASTER_LOG" 2>&1; then
    fail "PointerAnalyzer build failed"
    exit 1
  fi

  status "[BUILD] Checker"
  if ! dotnet build "$REPO_ROOT/checker/Checker.fsproj" \
      >> "$MASTER_LOG" 2>&1; then
    fail "Checker build failed"
    exit 1
  fi
fi

cd "$REPO_ROOT"

# Extract all binary name in NAMES as array
mapfile -t NAMES < <(printf '%s\n' "${!TARGETS[@]}" | LC_ALL=C sort)
METRIC_FILES=()
FAILURES=$INPUT_ERRORS
SUCCESSES=0

for name in "${NAMES[@]}"; do
  # Extract target binary
  target="${TARGETS[$name]}"
  # Generate log per binary
  binary_log="$LOG_DIR/${name}_pointer.log"
  : > "$binary_log"

  # Check binary existency
  if [[ ! -f "$target" ]]; then
    fail "$name: target binary does not exist: $target"
    FAILURES=$((FAILURES + 1))
    continue
  fi

  # Check GT binary existency and extract GT
  if [[ ! -v "GT_BINARIES[$name]" ]]; then
    fail "$name: matching ground-truth binary was not listed"
    FAILURES=$((FAILURES + 1))
    continue
  fi

  gt_binary="${GT_BINARIES[$name]}"
  if [[ ! -f "$gt_binary" ]]; then
    fail "$name: ground-truth binary does not exist: $gt_binary"
    FAILURES=$((FAILURES + 1))
    continue
  fi

  # Construct GT
  gt_json="$GT_DIR/${name}_GT.json"
  if [[ -f "$gt_json" ]]; then
    # If valid GT json exist, use previous one: Do not overwrite
    status "[GT EXISTS] $name: preserving $gt_json"
    if ! validate_json "$gt_json"; then
      fail "$name: existing GT is not valid JSON: $gt_json"
      FAILURES=$((FAILURES + 1))
      continue
    fi
  else
    # GT json does not exist. Construct GT json.
    status "[GT CREATE] $name"
    if ! run_logged \
      "$binary_log" \
      dotnet run --no-build --project "$REPO_ROOT/checker/Checker.fsproj" -- \
      -m 2 -b "$gt_binary" -o "$GT_DIR" -on "$name"; then
      fail "$name: ground-truth extraction failed; see $binary_log"
      FAILURES=$((FAILURES + 1))
      continue
    fi

    # Check GTExtractor generate valid GT json
    if [[ ! -f "$gt_json" ]] || ! validate_json "$gt_json"; then
      fail "$name: extractor did not produce valid GT JSON: $gt_json"
      FAILURES=$((FAILURES + 1))
      continue
    fi
    status "[GT CREATED] $name: $gt_json"
  fi

  # Execute PointerAnalyzer
  status "[ANALYZE] $name"
  if ! run_logged \
    "$binary_log" \
    dotnet run --no-build \
    --project "$REPO_ROOT/src/PointerAnalyzer.fsproj" -- \
    -b "$target" -o "$ANALYSIS_DIR" -fa "$INTER_RELATION"; then
    fail "$name: PointerAnalyzer failed; see $binary_log"
    FAILURES=$((FAILURES + 1))
    continue
  fi

  # Check PointerAnalyzer produces valid results
  inferred="$ANALYSIS_DIR/$name/inferredTypes.json"
  config="$ANALYSIS_DIR/$name/analysisConfig.json"
  if [[ ! -f "$inferred" ]] || ! validate_json "$inferred"; then
    # PointerAnalyzer did not generate valud inferred type
    fail "$name: missing or invalid inferred result: $inferred"
    FAILURES=$((FAILURES + 1))
    continue
  fi
  if [[ ! -f "$config" ]] || ! validate_json "$config"; then
    # PointerAnalyzer did not generate config file containing WordSize
    fail "$name: missing or invalid analysis configuration: $config"
    FAILURES=$((FAILURES + 1))
    continue
  fi
  status "[ANALYSIS OK] $name"

  # Evaluate the result per binary
  status "[EVALUATE] $name"
  if ! run_logged \
    "$binary_log" \
    dotnet run --no-build --project "$REPO_ROOT/checker/Checker.fsproj" -- \
    -m 3 -gt "$gt_json" -i "$inferred" \
    -o "$EVALUATION_DIR" -on "$name"; then
    fail "$name: PointerAnalyzer evaluation failed; see $binary_log"
    FAILURES=$((FAILURES + 1))
    continue
  fi

  # Check evaluator generate valid evaluate result json file
  metric="$EVALUATION_DIR/${name}_evalResult.json"
  if [[ ! -f "$metric" ]] || ! validate_json "$metric"; then
    fail "$name: missing or invalid evaluation metric: $metric"
    FAILURES=$((FAILURES + 1))
    continue
  fi

  METRIC_FILES+=("$metric")
  SUCCESSES=$((SUCCESSES + 1))
  status "[EVALUATION OK] $name"
done

# If at least one binary is successed to evaluate, merge all evaluation result
if ((${#METRIC_FILES[@]} > 0)); then
  if python3 "$SCRIPT_DIR/merge_pointer_metrics.py" \
      --interRelation "$INTER_RELATION" \
      --output "$MERGED_OUTPUT" "${METRIC_FILES[@]}" \
      >> "$MASTER_LOG" 2>&1; then
    status "[MERGED] $SUCCESSES result(s): $MERGED_OUTPUT"
  else
    fail "failed to merge PointerAnalyzer metrics"
    FAILURES=$((FAILURES + 1))
  fi
else
  fail "no successful PointerAnalyzer evaluations to merge"
  FAILURES=$((FAILURES + 1))
fi

status "[SUMMARY] successful=$SUCCESSES failed=$FAILURES"
if ((FAILURES > 0)); then
  exit 1
fi
