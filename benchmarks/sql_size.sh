RESULTS=$1
DLSIZE=dl_size.txt
SQLSIZE=sql_size.txt

# echo "" > $RESULTS/$DLSIZE
echo "" > $RESULTS/$SQLSIZE

for FILE in $(cat dl_list)
do
    # wc -c $FILE.dl >> $RESULTS/$DLSIZE
    wc -c $RESULTS/gensql/$FILE.inc.sql >> $RESULTS/$SQLSIZE
done
