# for idempotence

gprclean -q -Pdefault

# build
gprbuild -q -Pdefault

cd obj

# run the executable
./hello

# compute converage information
gcov hello f 2>&1 > /dev/null

cd ..

# launch gnathub
gnathub -q -Pdefault

# launch the print script
gnathub -q -Pdefault --exec script.py
