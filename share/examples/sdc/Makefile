sdc:
	gprbuild -p -P sdc

metric:
	gnatmetric -P sdc

check:
	-gnatcheck -P sdc --show-rule

prove:
	gnatprove -P sdc

codepeer:
	codepeer -P sdc -update-scil -level 1
	odepeer_msg_reader -csv obj/codepeer/sdc.output

clean:
	gprclean -P sdc
