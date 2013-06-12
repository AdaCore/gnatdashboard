#!/bin/bash

# USAGE
#
#  reporter.sh test_name status diff_file [comment [time]]
#
#  where:
#    test_name is the name of a test
#    status    is a status: OK, DIFF, XFAIL, or DEAD
#    diff_file is a file containing a diff (you can use /dev/null if you have no diff)
#    comment   is a comment, for instance "test disabled on windows"
#    time      is the time it took to run the test, in seconds, for instance "2.127"
#
# This scripts adds the given test to the current report dir.

test_name=`echo $1 | sed -e "s/'/_/g"`
status=$2
diff_file=
if [ "$3" != "" ]; then
   if [ -s $3 ]; then
      diff_file=$3
   fi
fi
comment=$4
time=$5

test_name=`echo $test_name | sed -e 's/:/=/g' | sed -e 's/\//./g' | sed -e 's/\\\\/./g'`

test_result="$REPORTS_DIR/$test_name.result"
test_diff="$REPORTS_DIR/$test_name.diff"
test_time="$REPORTS_DIR/$test_name.time"

# Write TEST_NAME.result
echo $status:$comment > "$test_result"

# Write TEST_NAME.diff if needed
if [ "$diff_file" != "" ]
then
   cp $diff_file $test_diff
fi

# Write TEST_NAME.time if needed
if [ "$time" != "" ]
then
   echo $time > "$test_time"
fi
