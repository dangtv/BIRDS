# BIRDS

More information about BIRDS is available at: [https://dangtv.github.io/BIRDS/](https://dangtv.github.io/BIRDS/)

## Installation and Usage

### Compilation and installation

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
* Installing:
  ```bash 
  make install
  ```

### Usage

* The execution file is `birds`:
    ```bash
    birds [OPTIONS]
    -db         print debugging information
    -f file     input program file, if not chosen, read from stdin
    -b file     shell script file specifying the action, which will be executed when there is an update on the view, if not chosen, execute nothing
    -o file     output SQL file, if not chosen, print to stdout
    -l file     output verification file (optional)
    -s schema   database schema name to connect to (default: public)
    -h host     database server host (default: "localhost")
    -c          connect and run the generated SQL on the database server
    -import     connect and import the data schema from database server
    -v          enable verifications
    -i          optimize the update propagation by incremental rewriting rules
    -e          optimize datalog rules
    -u          speed up the verifications
    -p port     database server port (default: "5432")
    -U user     database user (default: "postgres")
    -g user     the special user for global dejima synchronization (default: "dejima")
    -dejima     detect updates on dejima views to perform pre-defined actions in the shell script file
    -w password database user password (default: 12345678)
    -d dbname   database name to connect to (default: "datalogdb")
    -t timeout  timeout (second) (default: 120s)
    -help       Display this list of options
    --help      Display this list of options
    ```

* For example:
    ```bash
    birds -s public -f examples/basic_sample.dl -o examples/basic_sample.sql -v
    ```

### Installing underlying systems

* BIRDS is integrated with underlying systems including:
  * Lean >= 3.4.2: Download at [https://github.com/leanprover/lean/releases](https://github.com/leanprover/lean/releases)
    * Configure for Lean: add a new file `/Users/<user_name>/.lean/leanpkg.path` on Macos or `/root/.lean/leanpkg.path` on Linux with the following content(replace `<path_to_this_folder>` with the path to this souce code):
      ```bash 
        builtin_path
        path <path_to_this_folder>/verification/_target/deps/mathlib/src
        path <path_to_this_folder>/verification/src
        path <path_to_this_folder>/verification/_target/deps/super/src
      ```
    * Compiling Lean package of BIRDS: go to the folder `verification`: `cd verification`, and run:
      ```bash 
      leanpkg configure
      leanpkg build
      ```
  * Z3 >= 4.7.1: Download at [https://github.com/Z3Prover/z3/releases](https://github.com/Z3Prover/z3/releases)
  * Postgresql database >= 9.6: [https://www.postgresql.org/download/](https://www.postgresql.org/download/)
    * PL/sh extension for postgresql: available at [https://github.com/petere/plsh](https://github.com/petere/plsh)
  * coreutils package: For ubuntu: `apt-get install coreutils`. For Macos: `brew install coreutils` and then create a symbolic link `ln -s /usr/local/bin/gtimeout /usr/local/bin/timeout`

## Docker 

* Build a docker image:
  ```bash 
    docker build -t "birds" .
  ```

* Run a docker container based on this image
  ```bash 
    docker run --name "birds1" -ti -p 5432:5432 -p 3010:3010 -v <host_folder>:<container_folder> birds
  ```

* For example:
  ```bash 
    docker run --name "birds1" -ti -p 5432:5432 -p 3010:3010 -v $(pwd)/examples:/root/examples birds
  ```

* The Docker image for the entire BIRDS system is available at: [https://hub.docker.com/r/dangtv/birds](https://hub.docker.com/r/dangtv/birds), it can be downloaded by running:
  ```bash
    docker pull dangtv/birds
  ```
