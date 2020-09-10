# BIRDS

More information about BIRDS is available at [https://dangtv.github.io/BIRDS/](https://dangtv.github.io/BIRDS/)

## Installation

* To build the `birds` command line tool, the following softwares must be installed:
  * GNU Make >= 4.1
  * ocaml >= 4.07.0: see OCaml's [installation guidance](https://ocaml.org/docs/install.html)
  * Ocaml packages:
    * num (>= 1.0): `opam install num`
    * postgresql-ocaml (>=4.0.1): `opam install postgresql`

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
* Installing: by using the generated binary file `release/birds` or typing
  ```bash 
  make install
  ```

See the usage of the `birds` command by typing:
  ```bash 
  birds --help
  ```

## Integrated systems

BIRDS is integrated with other systems to enable some features of the `birds` command. These systems can be installed as follows:

* Z3 >= 4.8.7: The binary files can be downloaded at [https://github.com/Z3Prover/z3/releases](https://github.com/Z3Prover/z3/releases). To install the `z3` command, create a symbolic link in `/usr/bin` to the `z3` binary file:
  ```bash 
  ln -s <path-to-the-z3-binary-file> /usr/bin/z3
  ```
* Lean >= 3.4.2: The binary files can be downloaded at [https://github.com/leanprover/lean/releases](https://github.com/leanprover/lean/releases). To install Lean commands, create symbolic links in `/usr/bin` as follows:
  ```bash 
  ln -s <path-to-the-lean-binary-file> /usr/bin/lean 
  ln -s <path-to-the-leanpkg-binary-file> /usr/bin/leanpkg 
  ln -s <path-to-the-leanchecker-binary-file> /usr/bin/leanchecker 
  ```

  The Lean package of BIRDS must be configured and compiled as follows:
  * Configuration: add a new file `/Users/<user_name>/.lean/leanpkg.path` on MacOS or `/root/.lean/leanpkg.path` on Linux with the following content(replace `<path_to_BIRDS>` with the absolute path to the BIRDS source code folder):
    ```bash 
      builtin_path
      path <path_to_BIRDS>/verification/_target/deps/mathlib/src
      path <path_to_BIRDS>/verification/src
      path <path_to_BIRDS>/verification/_target/deps/super/src
    ```
  * Compilation: To compile the Lean package of BIRDS, type:
    ```bash 
    cd BIRDS/verification
    leanpkg configure
    leanpkg build
    ```
* Rosette: After installing [Racket](https://racket-lang.org) (Minimal Racket can work well), [Rosette](https://github.com/emina/rosette) can be installed by `raco pkg install rosette`.
* PostgreSQL database >= 9.6: [https://www.postgresql.org/download/](https://www.postgresql.org/download/)
  * PL/sh extension for PostgreSQL: available at [https://github.com/petere/plsh](https://github.com/petere/plsh)
* coreutils: 
  * For Ubuntu: `apt-get install coreutils`. 
  * For MacOS: `brew install coreutils` and then create a symbolic link `ln -s /usr/local/bin/gtimeout /usr/local/bin/timeout`

## Docker 

The docker image that contains all the features and the integrated systems of BIRDS can be built by:
  ```bash 
    docker build -t "birds" .
  ```

In this docker image, the PostgreSQL database runs on port 5432 and the BIRDS WebUI runs on port 3010
