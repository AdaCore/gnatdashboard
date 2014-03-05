# Fetch our own version of Python (which has SQLAlchemy installed)
BASE="$(dirname $(dirname `which gnathub`))"
PYTHON="$BASE/share/gnathub/python/bin/python"

# Launch gnathub on a trivial project
gnathub -q -P default --plugins gnatmetric

tool_name="GNATmetric"

# Verify that gnatmetric has saved metric in db
GNATmetric_count=`"$PYTHON" gnathub_query.py obj/gnathub/gnathub.db \
    "select count(r.id) from rules r, tools t where t.id = r.tool_id and t.name = \"$tool_name\";"`

if [ $((GNATmetric_count)) -lt 1 ]; then
    echo "No rule saved for GNATmetric"
fi

# Verify that gnatmetric tool has been saved in db
"$PYTHON" gnathub_query.py \
    obj/gnathub/gnathub.db "select name from tools;" | grep "$tool_name"
