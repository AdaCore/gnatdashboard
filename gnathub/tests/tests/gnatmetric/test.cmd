# launch gnathub on a trivial project
gnathub -q -P default --plugins gnatmetric

tool_name="GNATmetric"

# verify that gnatmetric has saved metric in db
gm_rules=`sqlite3 obj/gnathub/gnathub.db \
    "select count(r.id) from rules r, tools t where t.id = r.tool_id and t.name = \"$tool_name\";"`

if [ "$gm_rules" -lt "1" ]; then
    echo "No rule saved for GNATmetric"
fi

# verify that gnatmetric tool has been saved in db
sqlite3 obj/gnathub/gnathub.db "select name from tools;" | grep "$tool_name"
