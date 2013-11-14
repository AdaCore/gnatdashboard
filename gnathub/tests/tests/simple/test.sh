# launch GNAThub on a trivial project
gnathub -q -P default --plugins gnatmetric,gnatcheck,sonarconfig

# verify that the object directory contains the expected files
# at the expected locations
find obj/ | grep -v auto.cgpr |  sort
