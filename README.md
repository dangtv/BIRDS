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
  * Compiling Lean package
  Go to folder verification: `cd verification`
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
  * Compiling Lean package
  Go to folder verification: `cd verification`
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