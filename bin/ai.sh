#!/bin/bash

# ai.sh
#
# Author: Masaei Sato <masaei@affrc.go.jp>
# Date: 2024-2-19
# Last update: 2024-2-19
#
# Overview: This script is a wrapper script to run an R script (ai.R)
#   that uses openai's GPT-3.  The R script parses the text given as
#   arguments and generates a response using openai's GPT-3.
#
# Requirements:
#   - R (Confirmed to work with R version 4.3.2)
#   - R packages: jsonlite, httr
#   - openai's API key
#   - R script: ai.R
#
# Note:
#   - The R script (ai.R) and this script (ai.sh) must be located in the same directory.
#   - The R script must be executable.
#   - The API key must be set as an environment variable OPENAI_API_KEY.
#
# Usage:
#   ./ai.sh <user_text>
#   ./ai.sh -h
#
# Example usage:
#  ./ai.sh "What is agricultural informatics?"

# Help message
usage() {
  echo "Usage: $(basename "$0") [options] <user_text>"
  echo "Options:"
  echo "  -h, --help      Display this help message"
}

# Check if help is displayed
if [[ "$1" == "-h" || "$1" == "--help" ]]; then
  usage
  exit 0
fi

# Check the number of arguments
if [[ $# -ne 1 ]]; then
  echo "Error: Incorrect number of arguments."
  usage
  exit 1
fi

# Path to R script
# [[./ai.R][ai.R]]
# RSCRIPT_PATH="/home/$USER/bin/ai.R"

# Get the absolute path of the directory where the script resides
SCRIPT_DIR=$(cd $(dirname "$0") && pwd)

# Set path to R script (in same directory)
RSCRIPT_PATH="$SCRIPT_DIR/ai.R"

# Execute R script with user's text as argument
# Without option --vanilla (Because the effect of --no-environ
# prevents it from being executed.)
Rscript --slave $RSCRIPT_PATH "$1"
