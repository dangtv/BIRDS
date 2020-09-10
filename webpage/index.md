---
layout: index
---

# Overview

BIRDS (**BI**directional transformation for **R**elational view update **D**atalog-based **S**trategies) is a framework for programming view update strategies on relations in Datalog. 
The framework is based on the theory of (putback-based) bidirectional transformations (BX) but specifically developed for relational database management systems (RDBMSs).
BIRDS frees programmers from the burden of manually:

* Checking the well-behavedness of view update strategies,
* Optimizing view update strategies,
* Translating update strategies into triggers and trigger procedures for views in PostgreSQL to handle SQL DML statements of UPDATE/INSERT/DELETE on the views. 

# View update strategy: How to write?
We are given schemas of tables and a view. Traditionally, over the base tables, we write a query that defines the view. However, this defining query is not enough to determine how view updates are propagated to the base tables.

BIRDS allows developers to focus on the reverse direction, i.e., the view update strategy, which is more essential for updatable views. An update strategy can be completely described by a Datalog program over the base tables and the view that results in updates (insertions/deletions) on the base tables. In fact, this update strategy captures both the view update propagation and the defining query of the view. 

A well-behaved update strategy should make full use of data on the view in updating the source tables. We must ensure that no information on the view is lost in the updated source. BIRDS supports us to verify the well-behavedness of the written update strategy.

Writing view update strategies by example: [The basics](basic-tutorial.html)

The Datalog core Syntax: BIRDS currently accepts extensions of Datalog including negation and built-in predicates but no recursion in the Datalog program. See more details at [Syntax](syntax.html)

# From Datalog to SQL triggers

BIRDS is integrated with PostgreSQL as a backend SQL database. BIRDS generates an SQL program that implements the described view update strategy in the PostgreSQL database by using triggers.
See the [trigger generation](triggers.html) for details.

# Installation and Usage

BIRDS can be used via the Command line interface (CLI) or the Web-based interface (WebUI).

### Command line tool

The instructions of building and installing the command line tool `birds` can be found at [https://github.com/dangtv/BIRDS](https://github.com/dangtv/BIRDS).

To use the `birds` command:

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

For example, this command will transform the update strategy described in `test.dl` to sql:
```bash
birds -f test.dl -o test.sql
```

### WebUI 

Learn to use this WebUI at [the instruction for the WebUI editor](webui-installation.html)

### Docker

The easiest way to use both the command line tool `birds` and the web-based interface is creating a docker container from [BIRDS's docker image](https://hub.docker.com/r/dangtv/birds):

```bash 
docker run --name "birds" -ti -p 5432:5432 -p 3010:3010 dangtv/birds
```

In this docker image, the PostgreSQL database runs on port 5432 with a default user whose name is `postgres` and password is `12345678`. The BIRDS WebUI runs on port 3010 without a default user (an administrator user can be created at the first time).

# Case studies

* [The basics](basic-tutorial.html)
* [Customers database](customer.html)
* [Employees database](employee.html)
* [Ride-sharing system](ridesharing-tutorial.html)
* [Music database](music-tutorial.html)

# Publications

1. Van-Dang Tran, Hiroyuki Kato, Zhenjiang Hu. **Programmable View Update Strategies on Relations**. *46th International Conference on Very Large Data Bases ([VLDB 2020](https://vldb2020.org))*, Tokyo, Japan, 2020. [[BibTeX](assets/bib/vldb2020.bib), [DOI](https://doi.org/10.14778/3377369.3377380), [extended version](https://arxiv.org/abs/1911.05921)] 

2. Van-Dang Tran, Hiroyuki Kato, Zhenjiang Hu. **BIRDS: Programming view update strategies in Datalog**. *46th International Conference on Very Large Data Bases ([VLDB 2020](https://vldb2020.org))*, Demonstration, Tokyo, Japan, 2020. [[BibTeX](assets/bib/vldb2020demo.bib), [DOI](https://doi.org/10.14778/3415478.3415503)]

# Acknowledgments

This work is partially supported by the Japan Society for the Promotion of Science (JSPS) Grant-in-Aid for Scientific Research (S) No. 17H06099 ([the BISCUITS project](http://www.biscuits.work/)). We thank the authors of the open source software projects ([Z3](https://github.com/Z3Prover/), [Lean](https://github.com/leanprover/), [SQLPad](https://github.com/rickbergfalk/sqlpad), [Rosette](https://github.com/emina/rosette), [datalog](https://github.com/c-cube/datalog), and so forth) that are used and extended in BIRDS.

# Contact

[Van-Dang Tran](https://dangtv.github.io/) (The Graduate University for Advanced Studies, SOKENDAI, Japan),

[Hiroyuki Kato](https://researchmap.jp/katohiroyuki?lang=en) (National Institute of Informatics (NII), Japan),

[Zhenjiang Hu](http://research.nii.ac.jp/~hu/) (Peking University, China).
