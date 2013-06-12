#!/bin/bash
## Driver to run the automatic tests. Do not call this directly,
## use run.sh instead

. `dirname $0`/../scripts/CONFIG.sh

export REPORTS_DIR=`pwd`/../scripts/reports
mkdir -p $REPORTS_DIR

export REPORTER=`pwd`/../scripts/reporter.sh

print_header "QUALIMETRICS"
parse_opts "$@"

orig_tests=${tests}
tests=${tests:-*}
setup_traces

QUALIMETRICS=qualimetrics
QUALIMETRICS="$valgrind$gdb $QUALIMETRICS"

if [ "$XREF" = "yes" ]; then
   GNATINSPECT="gnatinspect --exit"
else
   GNATINSPECT="true"
fi
export QUALIMETRICS NUMPROCS valgrind gdb GNATINSPECT

## If the environment variable CLEANUP is set to 0, then we keep the
## .gps directory as is. In particular, this can be used to preserve the desktop
## (show the Python console) while debugging tests

cleanup_before_tests=${CLEANUP:-1}

export PATH=$testpwd/../bin/$OBJ_SUBDIR:"$PATH"

GPS_HOME=$testpwd/tmp_home

export PYTHONPATH=$testpwd${DIRSEP}$testpwd/../../share/library${DIRSEP}$testpwd/../../examples/python${DIRSEP}$PYTHONPATH

run_test ()
{
   dir=$1
   num=$2

   (
   ## Create the home directory, in order to avoid pop-up dialogs in the
   ## first launch of GPS. We recreate it from scratch so that each test
   ## really is independent from the others

   create_gps_home "$GPS_HOME.$num/"

   cd $testpwd/$dir

   # Drop per project messages files
   find . -name '*-msg.xml' -exec rm -f {} \;

   setup_traces

   out=out.$$.$num

   rm -f gnatinspect.*

   if [ -f test.cmd -o -f default.gpr ]; then
      if [ $silent -eq 0 ]; then
         print_header "$dir"
      fi

      if [ -f test.cmd ]; then
         if [ "$gdb" != "" ]; then
            $rlimit bash ./test.cmd
         else
            $rlimit bash ./test.cmd > $out 2>&1
         fi

         status=$?

         # Restore permissions in case test.cmd modified them (see
         # e.g. F502-040) and timed out
         chmod -R +w .

      else
         if [ "$gdb" != "" ] ; then
            $rlimit $QUALIMETRICS -Pdefault
         elif [ "$valgrind" != "" ]; then
            $rlimit $QUALIMETRICS -Pdefault >$out 2>&1
         else
            $rlimit $QUALIMETRICS -Pdefault >$out 2>&1
         fi

         status=$?
      fi

      # check for the presence of a "time.out" file, or "perf.out"
      if [ -f time.out ]; then
          time=`cat time.out`
          rm time.out
          if [ $silent -eq 0 ]; then
             echo "time:" $time s
          fi
      elif [ -f perf.out ]; then
          time=`cat perf.out`
          rm perf.out
          if [ $silent -eq 0 ]; then
             echo $time
          fi
          time=`echo $time | cut -d' ' -f2`
      else
          time=""
      fi

      if [ -f expected ]; then
         tr -d '\r' < $out > $out.stripped
         diff -u expected $out.stripped > $out
      fi

      if [ $status -eq 99 ]; then
         ## Not run
         if [ $silent -eq 0 ]; then
            echo_with_status 99
         fi

         if [ "$REPORTER" != "" ]; then
            $REPORTER qualimetrics.$dir "DEAD" /dev/null "Test not run"
         fi

      elif [ $status -eq 100 ]; then
         ## XFAIL
         if [ $silent -eq 0 ]; then
            echo_with_status 100
         fi

         if [ "$REPORTER" != "" ]; then
            $REPORTER qualimetrics.$dir "XFAIL" $out "$time"
         fi

      elif [ $status -ne 0 -o -s $out ]; then
         if [ $silent -eq 1 ]; then
            print_header "$dir"
         fi
         echo_with_status 0

         if [ "$REPORTER" != "" ]; then
           $REPORTER qualimetrics.$dir "DIFF" $out
         fi

         cat $out
      else
         ## Success
         if [ $silent -eq 0 -a "$GPS_PERFORMANCE" != "true" ]; then
            echo_with_status 1
            cat $out  # To see the traces
         fi

         if [ "$REPORTER" != "" ]; then
            $REPORTER qualimetrics.$dir "OK" /dev/null "" "$time"
         fi
      fi

      rm -f $out $out.stripped

   elif [ $silent -eq 0 ]; then
      echo_with_status 1 "====== $dir: nothing to run"
   fi

   if [ $header_line_terminated -eq 1 ]; then
      touch $testpwd/.header_line_terminated
   fi

   return $retry
   )
}

# run all tests

rm -f $testpwd/.header_line_terminated

if [ $silent -eq 0 ]; then
   echo
fi

for dir in $tests; do
   cd $testpwd
   if [ -d $dir \
        -a "$GPS_TEST_CONTEXT" = "nightly" \
        -a -f $dir/DO_NOT_RUN_NIGHTLY ]
   then
      if [ $silent = 0 ]; then
         echo_with_status 1 "====== $dir: not run nightly"
      fi

   elif [ -f $dir/DO_NOT_RUN ]; then
      if [ $silent = 0 ]; then
        echo_with_status 1 "====== $dir: not run"
      fi

   elif [ -d $dir ]; then
      run_test $dir
   fi
done

wait

[ $cleanup_before_tests -eq 1 ] && rm -rf tmp_home*

if [ -f $testpwd/.header_line_terminated ]; then
  header_line_terminated=1
  rm -f $testpwd/.header_line_terminated
fi

echo_with_status 1 ""
