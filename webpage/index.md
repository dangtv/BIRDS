---
layout: default
---

# Bidirectional programing for view updating in relational database system

## Datalog enviroment: Installation, Configuration and Usage

* datalog program: *.dl
* datalog runtime environment: [Datalog Educational System (DES)](http://des.sourceforge.net)

## Datalog Transformation: Installation and Usage

### Installation

* Dependencies:
    * [ocaml](https://ocaml.org/docs/install.html#Ubuntu)
    * ocamlyacc: opam install ocamlyacc
    * ocamllex: opam install ocamllex
    * findlib (>= 1.3.1)
    * GNU Make

* Installing:
    * Clean:
    ```bash
        make clean
    ```
    * Build:
    ```bash
        make
    ```

### Usage

* The binary file is ./bin/birds which derive get datalog program from putback one:

    ```bash
    ./bin/birds [OPTIONS]
    -db         : print debugging information
    -f file     : read program from file, if not chosen, read from stdin
    -o file     : write program out file, if not chosen, print to stdout
    -help  Display this list of options
    --help  Display this list of options
    ```

* For example:
    ```base
        ./bin/birds -s public -f tests/test1_put_rule.dl -o tests/test1_get.sql
    ```

## Datalog for View Update: How to write?

* For example, there is a base relation tracks1 in a database:
    ```sql
        CREATE TABLE tracks1(
            track VARCHAR NOT NULL PRIMARY KEY,
            date int,
            rating int,
            album VARCHAR,
            quantity int NOT NULL);
    ```
    To define a view tracks2_prime, we use set of rules to describe how to update data from the view to the base relation, and put these rules in one file, for example /tests/test1_put_rule.dl. The first step is writing a simple statement to tell the schemma of the view and the source:
    ```datalog
        %s:tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY).
        %v:tracks2_prime(TRACK,RATING,ALBUM,QUANTITY).
    ```
    then writing a set of rules describing how to udpate data from the view tracks2_prime to base relation tracks1:

    ```datalog
        %s:tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY).
        %v:tracks2_prime(TRACK,RATING,ALBUM,QUANTITY).
        delta_insert_tracks1(TRACK,2018,RATING,ALBUM,QUANTITY) :- tracks2_prime(TRACK,RATING,ALBUM,QUANTITY), not tracks1(TRACK,_,RATING,ALBUM,QUANTITY).
        delta_delete_tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY) :- tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY), not tracks2_prime(TRACK,RATING,ALBUM,QUANTITY),not tracks2_prime(TRACK,RATING,ALBUM,QUANTITY).
        tracks1_prime(TRACK,DATE,RATING,ALBUM,QUANTITY) :- tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY), not delta_delete_tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY).
        tracks1_prime(TRACK,DATE,RATING,ALBUM,QUANTITY) :- delta_insert_tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY).
    ```

* Derive the get datalog program (definition of the view):
    ```base
    ./bin/birds -h localhost -p 5432 -U guestuser -w 12345678 -d datalogdb -f tests/test1_put_rule.dl -o tests/test1_get_rule.dl
    ```
    result:
    ```datalog
    delta_insert_tracks1(TRACK,2018,RATING,ALBUM,QUANTITY) :- tracks2_prime_med(TRACK,RATING,ALBUM,QUANTITY) , not tracks1(TRACK,_,RATING,ALBUM,QUANTITY).
    tracks2_prime_med(TRACK,RATING,ALBUM,QUANTITY) :- tracks1(TRACK,_,RATING,ALBUM,QUANTITY).
    tracks2_prime(TRACKS2_PRIME_A4_COL0,TRACKS2_PRIME_A4_COL1,TRACKS2_PRIME_A4_COL2,TRACKS2_PRIME_A4_COL3) :- tracks2_prime_med(TRACKS2_PRIME_A4_COL0,TRACKS2_PRIME_A4_COL1,TRACKS2_PRIME_A4_COL2,TRACKS2_PRIME_A4_COL3).
    ```

### Run the pair of put and get datalog programs by using [DES](http://des.sourceforge.net)

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

### Run the pair of put and get datalog programs by using [DLV](http://www.dlvsystem.com/dlv/)

Because DLV do not allow to write anonimos variable (under score) in the negated predicates, so DLV can not work for the test 1. But it works for the test 2 (test 2 is for the view Tracks3 --> Tracks2):

* put: (need to remove the first line in test2_put_rule.dl) (test2_put.db is the set of ground fact)

```bash
    dlv test2_put_rule.dl test2_put.db -filter=tracks2_prime
```

* get: (test2_get.db is the set of ground fact)

```bash
    dlv test2_get_rule.dl test2_get.db -filter=tracks3_prime
```