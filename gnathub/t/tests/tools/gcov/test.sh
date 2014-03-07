# build
gprbuild -q -p -Pdefault

# run the executable & compute coverage information
(cd obj && ./hello && gcov hello f > /dev/null 2>&1)

# launch gnathub
gnathub -q -Pdefault

# launch the print script
gnathub -q -Pdefault --exec script.py
