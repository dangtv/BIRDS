# BIRDS

More detail at: [https://dangtv.github.io/BIRDS/](https://dangtv.github.io/BIRDS/)

## Compilation and Usage

### Compilation

* Dependencies:
  * GNU Make >= 4.1
  * ocaml >= 4.07.0: [installation guideline](https://ocaml.org/docs/install.html)
  * Ocaml package dependencies:
    * num (>= 1.0): opam install num
    * postgresql-ocaml (>=4.0.1): opam install postgresql
  * Lean >= 3.4.1
  * Z3 >= 4.7.1

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

### Installation

* Macosx: 
  * Installing Lean
    ```bash 
    brew install lean
    ```
  * Compiling Lean package: go to folder verification: `cd verification`
    ```bash 
    leanpkg configure
    leanpkg build
    ```
  * Configure for Lean: add new file `/Users/<user_name>/.lean/leanpkg.path` with the following (replace `<path_to_this_folder>` with the path to this souce code)
  ```bash 
    builtin_path
    path <path_to_this_folder>/verification/_target/deps/mathlib/.
    path <path_to_this_folder>/verification/src
    path <path_to_this_folder>/verification/_target/deps/super/src
  ```
  * Installing z3: download at [https://github.com/Z3Prover/z3/releases](https://github.com/Z3Prover/z3/releases), rename the execution file to `z3` and copy it to `/usr/local/bin/` 

  * Installing BIRDS: copy execution file to `/usr/local/bin/`

* Ubuntu: 
  * Installing Lean
    ```bash 
    apt-get install lean
    ```
  * Compiling Lean package: go to folder verification: `cd verification`
    ```bash 
    leanpkg configure
    leanpkg build
    ```
  * Configure for Lean: add new file `/root/.lean/leanpkg.path` with the following (replace `<path_to_this_folder>` with the path to this souce code)
  ```bash 
    builtin_path
    path <path_to_this_folder>/verification/_target/deps/mathlib/.
    path <path_to_this_folder>/verification/src
    path <path_to_this_folder>/verification/_target/deps/super/src
  ```
  * Installing z3:  
    ```bash 
    apt-get install z3
    ```
  * Installing BIRDS: copy execution file to `/usr/local/bin/`

### Usage

* The execution file is ./bin/birds:
    ```bash
    ./bin/birds [OPTIONS]
    -db            print debugging information
    -f file        read program from file, if not chosen, read from stdin
    -o file        write program out file, if not chosen, print to stdout
    -l file        write lean verification out file
    -s schema_name database schema name to connect to (default: public)
    -h host        database server host (default: "localhost")
    -c             connect and run sql on database server
    -import        connect and import data schema from database server
    -v             verify the well-behavedness
    -i             optimize update propagation by incremental rewriting rules
    -e             optimize datalog rules
    -p port        database server port (default: "5432")
    -U user        database user (default: "postgres")
    -w password    database user password (default: 12345678)
    -d dbname      database name to connect to (default: "datalogdb")
    -m mode        {1: For putback view update datalog program, 2: For view update datalog program containing view definition, update strategy and integrity constraints, 3: For only view definition datalog program} (default: 1)
    -help          Display this list of options
    --help         Display this list of options
    ```

* For example:
    ```bash
    bin/birds -s public -f examples/basic_sample.dl -o examples/basic_sample.sql -v
    ```