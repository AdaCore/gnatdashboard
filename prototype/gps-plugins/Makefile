###
# tree: execute qmt.py script to generate a file that contains the logical
# 			project tree
# report: execute the script python create_gc_report to generate gnatcheck
# 				report for the given project
###

GPR_PATH=$(PWD)/../../share/examples/sdc/sdc.gpr
SCRIPT=run.sh
REPORT_DIR_PATH=$(PWD)/../../share/examples/sdc/report/

GC_OUTPUT_PATH=$(PWD)/../../share/examples/sdc/gnatcheck.out
GM_OUTPUT_PATH=`pwd`/../../share/examples/sdc/metrix.xml
CP_OUTPUT_PATH=$(PWD)/../../share/examples/sdc/codepeer.out
TREE_FILE_PATH=$(PWD)/../../share/examples/sdc/project_tree.json

all: tree report

tree:
	sh $(SCRIPT) -d $(GPR_PATH) 1> $(TREE_FILE_PATH)

report: gc cp gm

gc: 
	python create_gc_report.py --json-tree=$(TREE_FILE_PATH) --target=$(REPORT_DIR_PATH) --log=$(GC_OUTPUT_PATH)

cp:
	python create_cp_report.py --json-tree=$(TREE_FILE_PATH) --output=$(CP_OUTPUT_PATH) --report-dir=$(REPORT_DIR_PATH)

gm:
	python create_gm_report.py --json-tree=$(TREE_FILE_PATH) --target=$(REPORT_DIR_PATH) --gm_output=$(GM_OUTPUT_PATH)

clean:
	rm *.pyc
