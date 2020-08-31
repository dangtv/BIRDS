---
layout: default
---

# BIRDS Command Line Tool

## Installation

* It is necessary to install external tools including Lean >= 3.4.1 and Z3 >= 4.7.1 before installing the command line tool `birds`. 
* The tool `birds` can be downloaded at [https://github.com/dangtv/BIRDS/releases](https://github.com/dangtv/BIRDS/releases),
  <!-- * [For Macosx]({{ site.github.mac_exe }}) -->
  <!-- * [For Ubuntu]({{ site.github.ubuntu_exe }}) -->
  <!-- * [Older versions](https://github.com/dangtv/BIRDS/releases) -->
or build an executable file from the source code available at [https://github.com/dangtv/BIRDS](https://github.com/dangtv/BIRDS) (along with instructions)

## Usage

```bash
birds [OPTIONS]
--version               Print version
--log                   Print running information
--debug                 Enable debugging mode
--explain               Show only explanations in the debugging mode
-f <file>               Input program file, if not chosen, read from stdin
-b <file>               Shell script file specifying the action, which will be executed when there is an update on the view, if not chosen, execute nothing
-o <file>               Output SQL file, if not chosen, print to stdout
-l <file>               Output verification file (optional)
-s <schema>             Database schema name to connect to (default: public)
-h <host>               Database server host (default: "localhost")
-c                      Connect and run the generated SQL on the database server
--import                Connect and import the data schema from the database server
-v                      Enable verifications
-x <size>               Get a counterexample with the maximum size if the program is not well-behaved
--counterexample <size> The same as -x
-i                      Optimize the update propagation by incremental rewriting rules
--incrementalization    The same as -i
-e                      Optimize datalog rules
--optimization          The same as -e
-u                      Speed up the verifications
--speedup               The same as -u
-p <port>               Database server port (default: "5432")
-U <user>               Database user (default: "postgres")
-g <user>               The special user for global dejima synchronization (default: "dejima")
--dejima                Detect updates on dejima views to perform pre-defined actions in the shell script file
-w <password>           Database user password (default: 12345678)
-d <dbname>             Database name to connect to (default: "datalogdb")
-t <timeout>            Timeout (second) (default: 120s)
-help                   Display this list of options
--help                  Display this list of options
```

For example, this command transforms the update strategy described in `test1.dl` to `test1.sql`:
```bash
birds -s public -f test1.dl -o test1.sql
```
