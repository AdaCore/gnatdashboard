# Create package for basic qualimetrics driver, doesn't includ GNATcheck
all: package

package:
	mkdir -p basic_qm_driver
	cp -pf basic_driver/qualimetrics.py basic_qm_driver/
	# Lib
	mkdir -p basic_qm_driver/lib
	cp -pf run.sh basic_qm_driver/lib/
	cp -pf create_cp_report.py basic_qm_driver/lib/
	cp -pf create_gcov_report.py basic_qm_driver/lib/
	cp -pf utils.py basic_qm_driver/lib/
	cp -pf qmt/qmt.py basic_qm_driver/lib/

clean:
	rm -rf basic_qm_driver
