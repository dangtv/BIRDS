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
-f file     read program from file, if not chosen, read from stdin
-b file     shell script file specifying the action, which will be executed when there is any update on the view, if not chosen, execute nothing
-o file     write program out file, if not chosen, print to stdout
-l file     write lean verification out file
-s schema   database schema name to connect to (default: public)
-h host     database server host (default: "localhost")
-c          connect and run sql on database server
-import     connect and import data schema from database server
-v          verify the well-behavedness
-i          optimize update propagation by incremental rewriting rules
-e          optimize datalog rules
-p port     database server port (default: "5432")
-U user     database user (default: "postgres")
-g user     special user for global dejima synchronization (default: "dejima")
-w password database user password (default: 12345678)
-d dbname   database name to connect to (default: "datalogdb")
-m mode     {1: For putback view update datalog program, 2: For view update datalog program containing view definition, update strategy and integrity constraints, 3: For only view definition datalog program} (default: 1)
-help       Display this list of options
--help      Display this list of options
```

For example, this command will transform the update strategy described in `test1.dl` to sql:
```bash
birds -s public -f test1.dl -o test1.sql
```
