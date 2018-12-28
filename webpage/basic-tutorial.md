---
layout: default
---

# BIRDS tutorial: basics

## Writing view update strategies in Datalog

* Declare the view and the source relations in Datalog by using two special symbols: `%s:` for source relations, `%v:` for views. For example, this statement is for a source table `people` which has two columns `id` and `name`:

    ```prolog
    %s: people(ID, NAME).
    ```
* Database modifications (Insertion, Deletion, Updates): we can use Datalog for describing data modifications on a source relation by writing rules for delta relations of this source relation:
  * Delta predicate: a delta predicate is a normal predicate following a symbol `+` or `-`
    * The predicate `+R` corresponds to the delta relation of tuples being inserted into source relation `R`
    * The predicate `−R` corresponds to the delta relation of tuples being deleted from `R`
    * Updates on `R` can be represented by using both `+R` and `-R`
  * Delta rule: a delta rule is a rule for modifying data in a source relation, it is a Datalog rule with a delta predicate in its head. For example, the following rule means any tuple `X`, which is in `s1` but not in `v`, will be deleted from `s1`:

    ```prolog
    -s1(X) :- s1(X), not v(X).
    ```

Suppose that a database has two tables `s1` and `s2`, both have a single column `X`, a view `v` over these two tables can be defined with the following steps:

1. Write an update strategy on a view v(X) to tables s1(X) and s2(X) by using Datalog ([basic_sample.dl]({{site.github.repository_url}}/tree/master/examples/basic_sample.dl)):

    ```prolog
    % describe the schema of sources and views
    %s: s1(X).
    %s: s2(X).
    %v: v(X).

    % rule for deletion from sources
    -s1(X) :- s1(X), not v(X).
    -s2(X) :- s2(X), not v(X).
    % rule for insertion to sources
    +s1(X) :- v(X), not s1(X), not s2(X).
    ```

1. Derive view definition and transform it with the update strategy to SQL statements ([basic_sample.sql]({{site.github.repository_url}}/tree/master/examples/basic_sample.sql)):
    ```bash
    birds -s public -f examples/basic_sample.dl -o examples/basic_sample.sql
    ```

1. The result contains PostgreSQL SQL statements for creating this view `v` in the database schema `public` and triggers on `v`, which make `v` updatable with the written update strategy. The SQL result can run directly in a PostgreSQL database:

    ```sql

    CREATE OR REPLACE VIEW public.v AS SELECT __dummy__.col0 AS X FROM (...) AS __dummy__;

    CREATE OR REPLACE FUNCTION public.v_delta_action() RETURNS TRIGGER LANGUAGE plpgsql 
    SECURITY DEFINER AS $$
    DECLARE
    ... 
    BEGIN
        CREATE TEMPORARY TABLE v_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
    
        CREATE TEMPORARY TABLE __dummy__delta__delete__s1 WITH OIDS ON COMMIT DROP AS 
        SELECT ... FROM (...) ;
        ...
        CREATE TEMPORARY TABLE __dummy__delta__insert__s1 WITH OIDS ON COMMIT DROP AS 
        SELECT ... FROM (...);

        FOR temprec IN ( SELECT * FROM __dummy__delta__delete__s1) LOOP 
                DELETE FROM public.s1 WHERE ROW(X) IS NOT DISTINCT FROM  temprec;
                END LOOP;
        DROP TABLE __dummy__delta__delete__s1;
        ...
        INSERT INTO public.s1 SELECT * FROM  __dummy__delta__insert__s1; 
        DROP TABLE __dummy__delta__insert__s1;
        RETURN NULL;
    EXCEPTION
        ...
    END;
    $$;

    CREATE OR REPLACE FUNCTION public.v_materialization() RETURNS TRIGGER LANGUAGE plpgsql 
    SECURITY DEFINER AS $$
    DECLARE
    ...
    BEGIN
        CREATE TEMPORARY TABLE __temp__v WITH OIDS ON COMMIT DROP AS SELECT * FROM public.v;
        CREATE CONSTRAINT TRIGGER __temp__peer1_public_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__v DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.v_delta_action();
        RETURN NULL;
    EXCEPTION
        ...
    END;
    $$;

    DROP TRIGGER IF EXISTS v_trigger_materialization ON public.v;
    CREATE TRIGGER v_trigger_materialization
        BEFORE INSERT OR UPDATE OR DELETE ON
        public.v FOR EACH STATEMENT EXECUTE PROCEDURE public.v_materialization();

    CREATE OR REPLACE FUNCTION public.v_update() RETURNS TRIGGER LANGUAGE plpgsql 
    SECURITY DEFINER AS $$
    DECLARE
    ...
    BEGIN
        IF TG_OP = 'INSERT' THEN
        INSERT INTO __temp__v SELECT (NEW).*; 
        ELSIF TG_OP = 'UPDATE' THEN
        DELETE FROM __temp__v WHERE ROW(X) IS NOT DISTINCT FROM OLD;
        INSERT INTO __temp__v SELECT (NEW).*; 
        ELSIF TG_OP = 'DELETE' THEN
        DELETE FROM __temp__v WHERE ROW(X) IS NOT DISTINCT FROM OLD;
        END IF;
        RETURN NULL;
    EXCEPTION
        ... 
    END;
    $$;

    DROP TRIGGER IF EXISTS v_trigger_update ON public.v;
    CREATE TRIGGER v_trigger_update
        INSTEAD OF INSERT OR UPDATE OR DELETE ON
        public.v FOR EACH ROW EXECUTE PROCEDURE public.v_update();
    ```

## Running in PostgreSQL as a backend database system

1. Load and run the SQL generated by BIRDS in PostgreSQL:
    ```bash
    sudo -u <db_user_name> psql -d <db_name> -f <path_to_sql_file>
    ```
    For example:
    ```bash
    sudo -u postgres psql -d my_db -f examples/basic_sample.sql
    ```

1. Run a UPDATE/INSERT/DELETE statement to modify data on view, for example:
    ```sql
    INSERT INTO v VALUES (6);
    ```
1. Request a complex modification on view by combining all UPDATE/INSERT/DELETE statements in one transaction:
    ```sql
    BEGIN;
        INSERT INTO v VALUES (6);
        DELETE FROM v where x = 5;
        UPDATE v SET x = 10 where x = 4;
    END;
    ```