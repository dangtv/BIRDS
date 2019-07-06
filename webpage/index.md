---
layout: index
---

## Overview

BIRDS (**BI**directional transformation for **R**elational view update **D**atalog-based **S**trategies) is a declarative framework that allows developers to program update strategies on relational views, hence, making views updatable. 
The framework is based on the theory of (putback-based) bidirectional transformation (BX) but specifically developed for relational database management systems (RDBMSs).
BIRDS supports Datalog, a highly declarative language, thus frees programmers from the burden of manually

* Checking the correctness of view update strategies with respect to the consistency of views and base relations,
* Maintaining bidirectional (backward & forward) transformations. In fact, a correct update strategy (backward direction) also captures the view definition (forward direction). BIRDS supports users to automatically derive the corresponding view definition for a given update strategy in which there is no self-join or projection on the view.
* Creating triggers and trigger procedures on updatable views for handling SQL DML statements: UPDATE/INSERT/DELETE.

## View update strategy: How to write ?
We are given schemas of tables and a view. Traditionally, over the base tables, we write a query that defines the view. However, this defining query is not enough to determine how view updates are propagated to the base tables.

BIRDS allows developers to focus on the reverse direction, view update strategy, which is more essential for updatable views. An update strategy is no more than a Datalog program over the base tables and the view that results in updates (insertions/deletions) on the base tables. In fact, this update strategy captures both the view update propagation as well as the defining query of the view. 

Note that an update strategy should make full use of data on the view in updating the source tables. We must ensure that no information on the view is lost in the updated source.
BIRDS supports us to verify the correctness of the written update strategy.

Update strategies by example: [The basics](basic-tutorial.html)

The Datalog core Syntax: [Syntax](syntax.html)

## Installation and Usage

There are two ways to use BIRDS to write and compile Datalog programs of view update strategies
    
* Command line tool: See the [instruction for CLI](cli-installation.html)
* Web-based interface: See the [instruction for WebUI editor](webui-installation.html)

You can also build from the [source code](https://github.com/dangtv/BIRDS) or download the [BIRDS's docker image](https://hub.docker.com/r/dangtv/birds) for using both the command line tool and WebUI editor of BIRDS.

## Case studies

* [The basics](basic-tutorial.html)
* [Customers database](customer.html)
* [Employees database](employee.html)
* [Ride-sharing system](ridesharing-tutorial.html)
* [Music database](music-tutorial.html)


