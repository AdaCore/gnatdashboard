###
# tree: execute qmt.py script to generate a file that contains the logical project tree
# report: execute the script python create_gc_report to generate gnatcheck report for the given project
###

GPR_PATH=$(PWD)/../../share/examples/sdc/sdc.gpr
SCRIPT=run.sh
TARGET_PATH=$(PWD)/../../share/examples/sdc/report/
LOG_PATH=$(PWD)/../../share/examples/sdc/gnatcheck.out
INTERPRETER=sh
TREE_FILE_PATH=$(PWD)/../../share/examples/sdc/project_tree.json

all: tree report

tree:
	sh $(SCRIPT) -d $(GPR_PATH) 1> $(TREE_FILE_PATH)

report:
	python create_gc_report.py -P$(GPR_PATH) -S$(SCRIPT) -I$(INTERPRETER) --target=$(TARGET_PATH) --log=$(LOG_PATH)
	
