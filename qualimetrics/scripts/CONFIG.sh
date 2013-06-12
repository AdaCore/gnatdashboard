#!/bin/bash

# This script provides a number of shell functions used by our various
# testsuites

## The directory where the test driver exists
## This directory is always set as the current directory, so that testsuites can
## assume they know the current directory

if [ "`uname`" = "Darwin" ]; then
  OS=Darwin
  PWDCMD="/bin/pwd -P"
  SED=perl
  SED_E=-pe
else
  SED=sed
  SED_E=-e
  PWDCMD=/bin/pwd
fi

export PWDCMD

testpwd=`dirname $0`
testpwd=`cd $testpwd; $PWDCMD`
cd "$testpwd"

if [ -d $testpwd/../../kernel ]; then
  rootdir=$testpwd/../..
elif [ -d $testpwd/../kernel ]; then
  rootdir=$testpwd/..
else
  rootdir=$testpwd/../../..
fi


# Set the VALGRIND environment variable to "yes" to activate support for
# valgrind. Set it to "leaks" to check for memory leaks.
# Always export valgrind_leak and valgrind_memcheck, so that tests can always
# force the use of valgrind if needed

which valgrind >/dev/null 2>/dev/null
if [ $? = 0 ]; then
  #  --gen-suppressions=all
  valgrind_leak="valgrind --tool=memcheck --num-callers=20 \
    --leak-resolution=high $EXTRA_VALGRIND_OPTS \
    --suppressions=$rootdir/gps/valgrind-python.supp \
    --suppressions=$rootdir/gps/opensuse11.3.supp \
    --suppressions=$rootdir/gps/valgrind-python-leaks.supp \
    --suppressions=$rootdir/gps/gps.supp"
  valgrind_leak_reachable="valgrind --tool=memcheck --num-callers=40 \
    --leak-resolution=high --show-reachable=yes $EXTRA_VALGRIND_OPTS \
    --suppressions=$rootdir/gps/valgrind-python.supp \
    --suppressions=$rootdir/gps/valgrind-python-leaks.supp \
    --suppressions=$rootdir/gps/gps.supp"
  valgrind_memcheck="valgrind --tool=memcheck $EXTRA_VALGRIND_OPTS \
    --suppressions=$rootdir/gps/valgrind-python.supp \
    --suppressions=$rootdir/gps/gps.supp --num-callers=20 -q "
  valgrind_callgrind="valgrind --tool=callgrind $EXTRA_VALGRIND_OPTS \
    --trace-jump=yes --dump-instr=yes \
    --suppressions=$rootdir/gps/valgrind-python.supp \
    --suppressions=$rootdir/gps/gps.supp --num-callers=20 -q "
else
  valgrind_leak=""
  valgrind_leak_reachable=""
  valgrind_memcheck=""
  valgrind_callgrind=""
fi

export valgrind_leak valgrind_memcheck valgrind_leak_reachable

if [ "$VALGRIND" = "yes" ]; then
  valgrind=$valgrind_memcheck
elif [ "$VALGRIND" = "leak" ]; then
  export G_SLICE=always-malloc
  export G_DEBUG=gc-friendly,resident-modules
  valgrind="$valgrind_leak --leak-check=full"
elif [ "$VALGRIND" = "leak_summary" ]; then
  export G_SLICE=always-malloc
  export G_DEBUG=gc-friendly,resident-modules
  valgrind="$valgrind_leak --leak-check=summary"
elif [ "$VALGRIND" = "leak_full" ]; then
  export G_SLICE=always-malloc
  export G_DEBUG=gc-friendly,resident-modules
  valgrind="$valgrind_leak_reachable --leak-check=full"
elif [ "$VALGRIND" = "callgrind" ]; then
  export G_SLICE=always-malloc
  export G_DEBUG=gc-friendly,resident-modules
  valgrind="$valgrind_callgrind"
else
  valgrind=""
fi

export valgrind

## The diff command to use
## We use "diff -u" since it won't output the > and < characters that sometimes
## confuse mailers. This is a GNU diff option, so GNU diff is required to
## run the test suites

diff="diff -u"

if [ "$OS" = "Windows_NT" ]; then
   exe_suffix=".exe"
   DIRSEP=";"
else
   exe_suffix=""
   DIRSEP=":"
fi

export exe_suffix DIRSEP


###################
## Parse the standard switches
##    -s (silent): Do not display any output for tests that pass. Only tests
##                 that fail or are ignored will get any output
##    -d (diff)  : For tests that fail, do not display a diff, only display its
##                 status
##    -r (rlimit): Command to execute to run each test. This is typically a
##                 "rlimit" command to limit the execution time of the command
##    -g (gdb):    Run the test under gdb (set $GDB to use another debugger)
##    -p (gps):    Specify the gps executable to use
## It also sets the variable $tests to the remaining command line arguments.
## This function must be called as "parse_opts "$@""
###################

silent=0
clean=0
coverage=0
showdiff=1
rlimit=""
tests=""
gdb=${gdb:-""}
GDB=${GDB:-gdb}
GPS=${GPS:-gps}
export GPS

parse_opts()
{
   while getopts "sdgr:p:cC" opt
   do
     case $opt in
       s) silent=1;;
       d) showdiff=0;;
       r) rlimit=${OPTARG:-""};;
       g) gdb="${GDB} -args";;
       p) GPS=${OPTARG:-"gps"};;
       c) clean=1;;
       C) coverage=1
          EXTRA_GPRBUILD_OPT="-cargs $COVERAGE_CARGS -largs $COVERAGE_LARGS $EXTRA_GPRBUILD_OPT"
          ;;
       *) echo_with_status 0 "Invalid switch: $opt"; exit 1;;
     esac
   done
   shift `expr $OPTIND - 1`

   tests="$@"
   export GPS
}

###################
## If the current directory contains a .gnatdebug file, setup the environment
## to use it. Otherwise, setup the environment to use a gnatdebug file in the
## test driver directory (to be shared by all the tests)
###################

setup_traces()
{
   if [ -f .gnatdebug ]; then
      ADA_DEBUG_FILE=`pwd`/.gnatdebug
   else
      ADA_DEBUG_FILE=$testpwd/gnatdebug$GNATDEBUG_VARIANT
   fi
   export ADA_DEBUG_FILE
}

###################
## Print the header of the test
###################

header_line_terminated=1
start_time=0
print_header()
{
   header="$1"

   if [ "$header" != "" ]; then
      echo -n "====== $header: "
   fi
   header_line_terminated=0
   start_time=$(date +%s)
}

###################
## Echo the first line in a paragraph. If needed, this also terminates the
## header line with a status for the test
## A testsuite should always terminate with 'echo_with_status 1 ""', which will
## have no effect if some previous output exists, and will displays a SUCCESS
## status if no previous output was done
###################

echo_with_status()
{
   end_time=$(date +%s)

   status="$1"
   shift
   output="$@"
   if [ $header_line_terminated -eq 0 ]; then
      if [ $status -eq 1 ]; then
         if [ "$GPS_PERFORMANCE" != "true" ]; then
            echo -n "SUCCESS"
         fi
      elif [ $status -eq 2 ]; then
         echo -n "NOT RUN"
      elif [ $status -eq 3 ]; then
         echo -n "XFAIL"
      else
         echo -n "FAILURE"
      fi

      if [ "$GPS_MONITOR" = "true" ]; then
         diff_time=$(( $end_time - $start_time ))
         echo " ($diff_time seconds)"
      else
         echo
      fi

      header_line_terminated=1
   fi
   if [ "$output" != "" ]; then
      echo "$output"
   fi
}

###################
## Run a command and test its output. In case of error, display the
## output of the command. In case of error, exit the script
###################

run_and_exit()
{
   command="$@"
   out=/tmp/gps_test$$
   eval "$command" > $out 2>&1
   result=$?
   if [ $result != 0 ]; then
      if [ $header_line_terminated = 0 ]; then echo "FAILS"; fi
      echo "$command"
      cat $out
      rm $out
      exit 1
   fi
   rm $out
}

###################
## Handles Control-C
###################

pipe_error()
{
  echo_with_status 0 "!!!! Interrupted !!!!"
  exit 1
}
trap pipe_error PIPE INT QUIT TERM > /dev/null 2>&1

###################
## Create a dummy .gps directory in "$1"
## Sets up a few default preferences.
## In particular, this ensures we are not loading any existing properties
## Sets up default perspectives layout.
###################

create_gps_home()
{
   localdir=${1:-$pwd}
   rm -rf $localdir/.gps
   mkdir -p $localdir/.gps
   mkdir $localdir/.gps/plug-ins
   mkdir $localdir/.gps/log_files

   cat >$localdir/.gps/preferences <<EOF
<?xml version="1.0"?>
<GPS>
  <pref name="General-Splash-Screen">False</pref>
  <pref name="Smart-Completion-Mode" > 0</pref>
  <pref name="Default-VCS"></pref>
  <pref name="General/Display-Tip-Of-The-Day">FALSE</pref>
</GPS>
EOF

   cat >$localdir/.gps/gnatinspect_traces.cfg <<EOF
>gnatinspect.log
EOF

   cp $rootdir/design/tests/perspectives.xml $localdir/.gps/perspectives.xml
   GPS_HOME=$localdir
   export GPS_HOME
}


###################
## substitute some environment-dependent paths by TEST, in order to allow
## system-independent checks
###################

substitute_pwd () {

   input_file="$1"
   output_file="$2"
   substitute_pwd=${3:-$testpwd}
   extra_flags=""

   if [ "$OS" = "Windows_NT" ]; then
      testpwd_os=`cygpath -w $substitute_pwd`
      obj=`cygpath -w $substitute_pwd/../obj`
      # We only need to do a case insensitive search on Windows. On top of that,
      # this flag can't be used in the general case since not all versions of
      # sed implement it.
      extra_flags="i"
   elif [ "$OS" = "Darwin" ]; then
      testpwd_os=$substitute_pwd
      obj=`cd $substitute_pwd/../obj 2>/dev/null; $PWDCMD`

      # Case insensitive search on Darwin as well
      extra_flags="i"
   else
      testpwd_os=$substitute_pwd
      obj=`cd $substitute_pwd/../obj 2>/dev/null; $PWDCMD`
   fi

   # we will do all the substitutions assuming that the directory separator is
   # /.

   testpwd_esc=`echo $testpwd_os | sed -e "s/\\\\\\\\/\\//g"`
   obj_esc=`echo $obj | sed -e "s/\\\\\\\\/\\//g"`

   $SED $SED_E "s,\\\\,/,g" $input_file | $SED $SED_E "s,$testpwd_esc,TESTS,g$extra_flags" | $SED $SED_E "s,$obj_esc,OBJ,g$extra_flags" | $SED $SED_E  "s,\\\\,/,g" > $output_file

}

###################
## substitute TESTS with PWD
## system-independent checks
###################

substitute_TESTS_with_pwd () {

   input_file="$1"
   output_file="$2"
   extra_flags=""

   if [ "$OS" = "Windows_NT" ]; then
      testpwd_os=`cygpath -w $testpwd`
      # We only need to do a case insensitive search on Windows. On top of that,
      # this flag can't be used in the general case since not all versions of
      # sed implement it.
      extra_flags="i"
   elif [ "$OS" = "Darwin" ]; then
      testpwd_os=$testpwd

      # Do a case insensitive search on Darwin as well
      extra_flags="i"
   else
      testpwd_os=$testpwd
   fi

   # we will do all the substitutions assuming that the directory separator is
   # /.

   testpwd_esc=`echo $testpwd_os | sed -e "s/\\\\\\\\/\\//g"`

   $SED $SED_E  "s,\\\\,/,g" $input_file | $SED $SED_E "s,TESTS,$testpwd_esc,g$extra_flags" | $SED $SED_E "s,\\\\,/,g" > $output_file

}

###################
## Compare two files, but substitute some environment-dependent strings in the
## second one
###################

compare_files()
{
   expected_output="$1"
   output_file="$2"
   output_stripped=$output_file.$$

   substitute_pwd $output_file $output_stripped

   $diff $expected_output $output_stripped
   localstatus=$?
   rm $output_stripped
   return $localstatus
}

###################
## Compare two files that contain the description of a canvas
###################

compare_canvas()
{
   expected_output="$1"
   output_file="$2"
   output_stripped=$output_file.$$
   output_stripped2=$output_file.$$_2

   substitute_pwd $output_file $output_stripped
   sed -e 's/ at[ -][0-9]*,[ -][0-9]*/ at x, y/g' $output_stripped >$output_stripped2

   $diff $expected_output $output_stripped2
   localstatus=$?
   rm $output_stripped $output_stripped2
   return $localstatus
}

###################
## Restart the xserver used by nightly builds to run the GPS test suite
###################

restart_x_server()
{
   if [ "$XSERVER" != "" -a "$XDISPLAY" != "" ]; then
      # Wait a bit before restarting the X server, so that sockets connections
      # are properly released. This is just a test for now
      sleep 20
      ssh $XSERVER "script/gps-utils $GNATMAIL_DIR x_start $XDISPLAY $MACHINE"
   fi
}

###################
## Remove occurences of .exe found in the file
###################

remove_exe_suffix()
{
   file=$1
   sed -e "s/\.exe//" $file > $file.tmp
   mv $file.tmp $file
}

