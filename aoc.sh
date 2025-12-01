#!/bin/bash

MIN_YEAR=2015
AOC_LIMIT_YEAR=2025
AOC_MAX_DAYS_PRE_LIMIT=25
AOC_MAX_DAYS_POST_LIMIT=12

PNAME=$0
YEAR=$1
DAY=$2
AOC_ARG_KEY=$3

CURRENT_YEAR=$(date +%Y)
SCRIPT_PATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
ENV_FILE="$SCRIPT_PATH/.env"
AOC_SESSION=""

help() {
  cat << EOF >&2
Usage: $PNAME <year> <day> [aoc-session-key]

Arguments:
  <year>             The Advent of Code year (>= $MIN_YEAR).
  <day>              The puzzle day (1 to $AOC_MAX_DAYS_PRE_LIMIT).

  [aoc-session-key]  (Optional) The session key for the AoC API.

Session Key Priority:
The script searches for the session key in this order:
1. Optional 3rd command-line argument: [aoc-session-key]
2. Local .env file: $ENV_FILE
3. Shell environment variable: \$AOC_SESSION

Note on Puzzle Days:
For year $AOC_LIMIT_YEAR and onwards, Advent of Code only provides 
${AOC_MAX_DAYS_POST_LIMIT} puzzles per year (Day 1 to 12).
For years prior to $AOC_LIMIT_YEAR, the maximum day is $AOC_MAX_DAYS_PRE_LIMIT.
EOF

  if [[ -n "$1" ]]; then
    echo -e "\nERROR: $1" >&2
    exit 1
  fi
  exit 0
}

error() {
  help "$1"
}

get_session_key() {
  # Check argument
  if [[ -n "$ARG_SESSION_KEY" ]]; then
    AOC_SESSION="$ARG_SESSION_KEY"
    return 0
  fi

  # check .env file
  if [[ -f "$ENV_FILE" ]]; then
    LOCAL_KEY=$(cat "$ENV_FILE" | tr -d '\n')
    if [[ -n "$LOCAL_KEY" ]]; then
      AOC_SESSION="$LOCAL_KEY"
      return 0
    fi
  fi

  # check OS environment
  if [[ -n "$AOC_SESSION" ]]; then
    return 0
  fi

  return 1
}

if [[ "$1" = "--help" || "$1" = "-h" ]]; then
  help
fi

if [[ -z "$YEAR" || -z "$DAY" ]]; then
  error "Missing <year> or <day> parameter."
fi

# validate session key
get_session_key
if [[ -z "$AOC_SESSION" ]]; then
  error "No AoC Session Key found. Please provide it via environment (\$AOC_SESSION), .env file, or as a parameter."
fi

# validate year
if ! [[ "$YEAR" =~ ^[0-9]+$ ]]; then
  error "'$YEAR' is not a valid numeric year."
elif [[ "$YEAR" -lt $MIN_YEAR ]]; then
  error "'$YEAR' is an invalid year: Less than $MIN_YEAR."
elif [[ "$YEAR" -gt $CURRENT_YEAR ]]; then
  error "'$YEAR' is an invalid year: Greater than $CURRENT_YEAR."
fi

# validate day
MAX_DAY=$AOC_MAX_DAYS_PRE_LIMIT
if [[ "$YEAR" -ge $AOC_LIMIT_YEAR ]]; then
  MAX_DAY=$AOC_MAX_DAYS_POST_LIMIT
fi

if ! [[ "$DAY" =~ ^[0-9]+$ ]]; then
  error "'$DAY' is not a valid numeric day."
elif [[ "$DAY" -lt 1 ]]; then
  error "'$DAY' is an invalid day: Less than 1."
elif [[ "$DAY" -gt $MAX_DAY ]]; then
  error "'$DAY' is an invalid day: Greater than $MAX_DAY. (Note: $YEAR has a limit of $MAX_DAY days.)"
fi

if [[ "$DAY" -gt 9 ]]; then
  DAY_NAME="$DAY"
else
  DAY_NAME="0$DAY"
fi

if [[ ! -d "$SCRIPT_PATH/$YEAR" ]]; then
  echo "Creating directory: $SCRIPT_PATH/$YEAR"
  mkdir -p "$SCRIPT_PATH/$YEAR"
fi

DAY_PATH="$SCRIPT_PATH/$YEAR/$DAY_NAME"
if [[ ! -d "$DAY_PATH" ]]; then
  echo "Creating directory: $DAY_PATH"
  mkdir -p "$DAY_PATH"
fi

