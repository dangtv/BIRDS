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

# View update strategy: How to write ?
We are given schemas of tables and a view. Traditionally, over the base tables, we write a query that defines the view. However, this defining query is not enough to determine how view updates are propagated to the base tables.

BIRDS allows developers to focus on the reverse direction, view update strategy, which is more essential for updatable views. An update strategy can be completely described by a Datalog program over the base tables and the view that results in updates (insertions/deletions) on the base tables. In fact, this update strategy captures both the view update propagation and the defining query of the view. 

A well-behaved update strategy should make full use of data on the view in updating the source tables. We must ensure that no information on the view is lost in the updated source. BIRDS supports us to verify the well-behavedness of the written update strategy.

Writing view update strategies by example: [The basics](basic-tutorial.html)

The Datalog core Syntax: [Syntax](syntax.html)

# From Datalog to SQL triggers

BIRDS is integrated with PostgreSQL as a backend SQL database. BIRDS generates an SQL program that implements the described view update strategy in the PostgreSQL database by using triggers.
See the [trigger generation](triggers.html) for details.

# Installation and Usage

BIRDS can be used via Command line interface (CLI) or Web-based interface (WebUI).

### Command line tool

See [https://github.com/dangtv/BIRDS](https://github.com/dangtv/BIRDS) to install the command line tool `birds`.

To use the `birds` command:

```bash
birds [OPTIONS]
-db         print debugging information
-f file     input program file, if not chosen, read from stdin
-b file     shell script file specifying the action, which will be executed when 
            there is an update on the view, if not chosen, execute nothing
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
-dejima     detect updates on dejima views to perform pre-defined actions in the
            shell script file
-w password database user password (default: 12345678)
-d dbname   database name to connect to (default: "datalogdb")
-t timeout  timeout (second) (default: 120s)
-help       Display this list of options
--help      Display this list of options
```

For example, this command will transform the update strategy described in `test1.dl` to sql:
```bash
birds -s public -f test.dl -o test.sql
```

### WebUI 

Learn to use this WebUI at [instruction for WebUI editor](webui-installation.html)

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


