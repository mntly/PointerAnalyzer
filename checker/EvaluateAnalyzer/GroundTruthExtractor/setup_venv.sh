#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
VENV_DIR="${SCRIPT_DIR}/.venv"
PYTHON_BIN="${PYTHON_BIN:-python3}"

# Check python is installed
if ! command -v "${PYTHON_BIN}" >/dev/null 2>&1; then
  echo "python3 is not installed or not in PATH." >&2
  exit 1
fi

# Generate python venv at GroundTruthExtractor
"${PYTHON_BIN}" -m venv "${VENV_DIR}"

# Install pyelftools for parsing DWARF information
"${VENV_DIR}/bin/python" -m pip install --upgrade pip
"${VENV_DIR}/bin/python" -m pip install pyelftools

echo "GroundTruthExtractor Python venv is ready: ${VENV_DIR}"
echo "Python: ${VENV_DIR}/bin/python"
