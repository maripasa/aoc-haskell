YEAR=$1
DAY=$2
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
AOC_SESSION=$(tr -d '\n' < "$SCRIPT_DIR/../.env")
# Help
if [ -z "$YEAR" ] || [ -z "$DAY" ]; then
  echo "Usage: $0 <year> <day>" >&2
  exit 1
fi
# Does .env exist?
if [ -z "$AOC_SESSION" ]; then
  echo ".env does not exist" >&2
  exit 2
fi

input=$(curl --cookie "session=$AOC_SESSION" "https://adventofcode.com/$YEAR/day/$DAY/input")
padded_day=$(printf "%02d" "$DAY")

if [ -d "$SCRIPT_DIR/day$padded_day" ]; then
  echo "Day$padded_day already exists, doing nothing"
  exit 3
fi

mkdir -p "$SCRIPT_DIR/day$padded_day"
touch "$SCRIPT_DIR/day$padded_day/Day$padded_day.hs"
echo "$input" > "$SCRIPT_DIR/day$padded_day/input"
cp "$SCRIPT_DIR/build" "$SCRIPT_DIR/day$padded_day/"

echo "done"
