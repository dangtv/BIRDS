# BIRDS

More detail at: [https://dangtv.github.io/BIRDS/](https://dangtv.github.io/BIRDS/)

## Compilation and Usage

### Compilation

* Dependencies:
  * ocaml >= 4.02.3: [installation guideline](https://ocaml.org/docs/install.html)
  * GNU Make >= 4.1

* Compiling:
  * Clean:
    ```bash
    make clean
    ```
  * Build:
    ```bash
    make all
    ```
  * Release:
    ```bash
    make release
    ```

### Usage

* The execution file is ./bin/birds:
    ```bash
    ./bin/birds [OPTIONS]
    -db         : print debugging information
    -f file     : read program from file, if not chosen, read from stdin
    -o file     : write program out file, if not chosen, print to stdout
    -s schema_name     : database schema name to connect to (default: public)
    -help  Display this list of options
    --help  Display this list of options
    ```

* For example:
    ```bash
    ./bin/birds -s public -f tests/test1_put_rule.dl -o tests/test1.sql
    ```

## Other Datalog systems

* Datalog source file: *.dl
* Datalog runtime environment: [Datalog Educational System (DES)](http://des.sourceforge.net), or [DLV System](http://www.dlvsystem.com/dlv/)

### Run a pair of put and get datalog programs by using [DES](http://des.sourceforge.net)

* Go to DES CLI :
    ```bash
    des
    ```
* put:
  * load the ground facts:
    ```bash
    /consult tests/test1_put_fact.dl
    ```
  * load the rules of put (you need to remove the first line from tests/test1_put_rule.dl):
    ```bash
    /[+ tests/test1_put_rule.dl ]
    ```
  * get the result of tracks1_prime:
    ```bash
    tracks1_prime(TRACK,DATE,RATING,ALBUM,QUANTITY)
    ```
* get:
  * load the ground facts:
    ```bash
    /consult tests/test1_get_fact.dl
    ```
  * load the rules of put:
    ```bash
    /[+ tests/test1_get_rule.dl ]
    ```
  * get the result of tracks2_prime:
    ```bash
    tracks2_prime(TRACK,RATING,ALBUM,QUANTITY)
    ```

### Run a pair of put and get datalog programs by using [DLV](http://www.dlvsystem.com/dlv/)

* put: (need to remove the first line in test2_put_rule.dl) (test2_put.db is the set of ground fact)
    ```bash
    dlv test2_put_rule.dl test2_put.db -filter=tracks2_prime
    ```

* get: (test2_get.db is the set of ground fact)
    ```bash
    dlv test2_get_rule.dl test2_get.db -filter=tracks3_prime
    ```