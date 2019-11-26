# BIRDS

More information about BIRDS is available at [https://dangtv.github.io/BIRDS/](https://dangtv.github.io/BIRDS/)

## CLI Installation

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

The BIRDS command line tool is `birds`. See the usage of this command by using the `-h` option:
  ```bash 
  birds -h
  ```

## Integrated systems

BIRDS is integrated with other systems to enable some features of the `birds` command. These systems can be installed as follows:

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
* Z3 >= 4.8.7: Download at [https://github.com/Z3Prover/z3/releases](https://github.com/Z3Prover/z3/releases)
* Postgresql database >= 9.6: [https://www.postgresql.org/download/](https://www.postgresql.org/download/)
  * PL/sh extension for postgresql: available at [https://github.com/petere/plsh](https://github.com/petere/plsh)
* coreutils package: For ubuntu: `apt-get install coreutils`. For Macos: `brew install coreutils` and then create a symbolic link `ln -s /usr/local/bin/gtimeout /usr/local/bin/timeout`

## Docker 

The docker image that contains all the features and the integrated systems of BIRDS can be built by:
  ```bash 
    docker build -t "birds" .
  ```

In this docker image, the PostgreSQL database runs on port 5432 and the BIRDS WebUI runs on port 3010
