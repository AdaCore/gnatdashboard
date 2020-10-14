#!/bin/bash

rm -rf prj_simple/obj
gnathub -P "prj_simple/simple.gpr" --plugins gnatstack
echo $?

