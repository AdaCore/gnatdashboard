CP_DOC_PATH=/Users/kiwi/code/codepeer/release/install-common/doc/users_guide/messages_and_annotations.rst
GC_DOC_PATH=/Users/kiwi/tools/gnatpro/gnat/gnat-last/share/doc/gnat/txt/gnatcheck_rm.txt
all:
	python GNATtools_create_resource.py --cp_doc=$(CP_DOC_PATH) --gc_doc=$(GC_DOC_PATH)
	make clean

clean:
	rm *.pyc
