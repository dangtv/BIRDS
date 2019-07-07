---
layout: default
---

# BIRDS Command Line Tool

## Installation

* It is necessary to install external tools including Lean >= 3.4.1 and Z3 >= 4.7.1 before installing the command line tool `birds`. 
* The tool `birds` can be downloaded at:
  * [For Macosx]({{ site.github.mac_exe }})
  * [For Ubuntu]({{ site.github.ubuntu_exe }})
  <!-- * [Older versions](https://github.com/dangtv/BIRDS/releases) -->
  * Or build an executable file from the source code available at [https://github.com/dangtv/BIRDS](https://github.com/dangtv/BIRDS) (along with instructions)

## Usage

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
-v          enable verifications
-i          optimize the update propagation by incremental rewriting rules
-e          optimize datalog rules
-p port     database server port (default: "5432")
-U user     database user (default: "postgres")
-g user     the special user for global dejima synchronization (default: "dejima")
-dejima     detect updates on dejima views to perform pre-defined actions in the shell script file
-w password database user password (default: 12345678)
-d dbname   database name to connect to (default: "datalogdb")
-m mode     1: put, 2: get & put, 3: get (default: 1)
-help       Display this list of options
--help      Display this list of options
```

For example, this command will transform the update strategy described in `test1.dl` to sql:
```bash
birds -s public -f test1.dl -o test1.sql
```
