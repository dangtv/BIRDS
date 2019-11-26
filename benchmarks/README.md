## How to run the benchmark

### Validation time 

To run the validation for Datalog programs in the benchmark:

```bash
 bash benchmark.sh
```

The results are stored in the folder `results`
The validation time is stored in the file `putbx_time.csv`

To calculate the size of Datalog programs:

```bash
 bash datalog_cloc.sh
```

The result is in the file `datalog.csv`

To calculate the size of compiled SQL programs

```bash
 bash sql_size.sh <result-folder>
```

The result is in the file `<result-folder>/sql_size.txt`

## View updating time

For each view, to measure the running time of an SQL statement that modifies the view, we first need to generate data for the underlying base tables by

```bash
bash dbgen.sh
```

That randomly generates data for the base tables with various numbers of tuples. The result is many CSV files with different numbers of records (tuples) for each base table. This data generation process may take time.

Then, we can start benchmarking by

```bash
bash benchmark.sh
```
