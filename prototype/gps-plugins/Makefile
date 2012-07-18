###
# Execute the script python create_gc_report to generate gnatcheck report for the given project
###

GPR_PATH=/Users/martin/Projects/qualimetrics/prototype/examples/gps_tutorial/sdc.gpr
SCRIPT=run.sh
TARGET_PATH=/Users/martin/Projects/qualimetrics/prototype/examples/gps_tutorial/report/
LOG_PATH=/Users/martin/Projects/qualimetrics/prototype/examples/tutorial/gnatcheck.log
INTERPRETER=sh


all:
	python create_gc_report.py -P$(GPR_PATH) -S$(SCRIPT) -I$(INTERPRETER) --target=$(TARGET_PATH) --log=$(LOG_PATH)
