---
layout: default
---

# The basics

## Schema

Example: Two base tables `s1(X,Y)` and `s2(X,Y)` and a view `v(X,Y)`

s1

| X | Y |
|---|---|
| 1 | 2 |
| 2 | 3 |

s2 
 
| X | Y |
|---|---|
| 1 | 2 |
| 2 | 3 |


v 

| X | Y |
|---|---|
| 1 | 2 |
| 2 | 3 |


```prolog
source s1(X:int, Y:int).
source s2(X:int, Y:int).
view v(X:int, Y:int).
```
* Use the keywords `source` and `view` to distinguish source tables and view
* Each column is assigned a data type
* Supported data types are: integer, float, string.

### Rules for update strategy:
* Delta predicate: a delta predicate is a normal predicate following a symbol `+` or `-`
  * The predicate `+R` corresponds to the delta relation of tuples being inserted into source relation `R`
  * The predicate `−R` corresponds to the delta relation of tuples being deleted from `R`
* Delta rule: a delta rule is a rule whose head is a delta predicate of a base table. For example, the following rule means any tuple `(X,Y)`, which is in `s1` but not in `v`, will be deleted from `s1`:

    ```prolog
    -s1(X,Y) :- s1(X,Y), not v(X,Y).
    ```

## Update strategy

The following is a full Datalog program for an update strategy on the view `v(X,Y)` ([basic_sample.dl]({{site.github.repository_url}}/tree/master/examples/basic_sample.dl)):

```prolog
% schema:
source s1(X:int,Y:int).
source s2(X:int,Y:int).
view v(X:int,Y:int).

% rules for update strategy:
-s1(X,Y) :- s1(X,Y), not v(X,Y).
-s2(X,Y) :- s2(X,Y), not v(X,Y).
+s1(X,Y) :- v(X,Y), not s1(X,Y), not s2(X,Y).
```
    
The first two rules say that if a tuple `(X,Y)` is in `s1` or `s2` but not in `v`, it will be deleted from `s1` or `s2`, respectively. The last rule says that if a tuple `(X,Y)` is in `v` but in neither `s1` nor `s2`, it will be inserted to `s1`.

## Verifying and Compiling into SQL

We verify the correctness of the Datalog program and compile it into SQL by using command line `birds`:

```bash
birds -v -f basic_sample.dl -o basic_sample.sql
```

Where the option `-v` enables verification process, `-f` is for the input Datalog program and `-o` is for the output SQL file.

![basic-compilation](assets/images/basic-compilation.png)

The generated SQL file can run directly in a PostgreSQL database to create the corresponding updatable view `v` in the database.


## Running in PostgreSQL

1. Load and run the generated SQL file in PostgreSQL:
    ```bash
    psql -U <db_user_name> -d <db_name> -f <path_to_sql_file>
    ```
    For example:
    ```bash
    psql -U postgres -d sample_db -f examples/basic_sample.sql
    ```

1. Run an UPDATE/INSERT/DELETE statement to modify data on view, for example:
    ```sql
    INSERT INTO v VALUES (6,7);
    ```
1. Request a bigger modification on view by combining multiple UPDATE/INSERT/DELETE statements in one transaction:
    ```sql
    BEGIN;
        INSERT INTO v VALUES (6,7);
        DELETE FROM v where x = 5;
        UPDATE v SET x = 10 where x = 4;
    END;
    ```