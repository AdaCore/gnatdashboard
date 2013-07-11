# launch qualimetrics on a trivial project
qualimetrics -Pdefault

tool_name="GNAT Metric"

# verify that gnatmetric has saved metric in db
gm_rules=`sqlite3 obj/qualimetrics/qualimetrics.db "select count(r.id) from rules r, tools t where t.id = r.tool_id and t.name = \"GNAT Metric\";"`
if [ "$gm_rules" -lt "1" ]
then
    echo "No rule saved for GNAT Metric"
fi

# verify that gnatmetric tool has been saved in db
sqlite3 obj/qualimetrics/qualimetrics.db "select name from tools;" | grep "$tool_name"

