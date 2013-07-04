# launch qualimetrics on a trivial project
qualimetrics -Pdefault

# verify that gnatmetric has saved metric in db
if [ `sqlite3 obj/qualimetrics.db "select count(id) from rules;"` -gt 0 ]
then
    echo "OK - rules saved"
else
    echo "NOK - no rules saved"
fi

# verify that gnatmetric tool has been saved in db
if [ `sqlite3 obj/qualimetrics.db "select count(id) from tools;"` -gt 0 ]
then
    echo "OK - tool saved"
else
    echo "NOK - tool not saved"
fi

