# launch GNAThub on a trivial project
gnathub -P default

# verify that the object directory contains the expected files
# at the expected locations
find obj/ | grep -v ".svn" | sort
