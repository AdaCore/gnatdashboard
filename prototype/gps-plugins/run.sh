#! /bin/sh

#
#                  Q U A L I M E T R I C S
#
# Helper script to run the qmt.py script through GPS interpreter.
#
# Usage:
#    $ ./run.sh [-d] path/to/root.gpr
#
# The --debug option removes the --hide switch to GPS command line to display
# the python console if there is an issue with the qmt.py script (e.g. syntax
# error).
#

# Displays the usage string and exits with ecode 1.
function usage() {
  echo 'usage: run.sh [-d|--debug] PROJECT_FILE' 1>&2
  exit 1
}

# If no argument given, exits directly.
if [ $# -eq 0 ]; then
  usage
fi

PROJECT_FILE=""
GPS_HIDE_SWITCH="--hide"

# Parses the command line.
for arg in "$@"; do
  case $1 in
    "-d" | "--debug")
        GPS_HIDE_SWITCH=""
      ;;
    *)
        PROJECT_FILE="$1"
      ;;
  esac

  shift
done

# If no .gpr file provided, exists with ecode 1.
if [ -z "$PROJECT_FILE" ]; then
  usage
fi

# Spawn gps with the correct command line arguments.
gps $GPS_HIDE_SWITCH -P $PROJECT_FILE --load=python:qmt/qmt.py

# Exits the script with GPS exit code.
exit $?
