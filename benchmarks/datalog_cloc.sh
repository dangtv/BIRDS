DLSIZE=datalog.csv

echo "language,filename,blank,comment,code" > $DLSIZE

for FILE in $(cat dl_list)
do
    cloc --force-lang=prolog --quiet --csv --by-file $FILE.dl | head -3 | tail -1 >> $DLSIZE
done

# cloc --force-lang=prolog --quiet /Users/dangtv/Projects/delta-datalog/examples/benchmark/stackexchange.2/purchaseview.2.dl 
# cloc --force-lang=prolog --quiet --csv --list-file=dl_list2 --by-file
# cloc --force-lang=prolog --quiet --csv --list-file=dl_list_ext --by-file
# cloc --force-lang=prolog --quiet --csv --list-file=dl_list_ext --by-file --report-file=datalog.csv