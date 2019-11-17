---
layout: default
---

# Trigger generation

BIRDS compiles a Datalog program of view update strategy into an SQL program that creates the corresponding view with associated triggers in PostgreSQL.
Suppose the view schema is `v(X)`, the first SQL statement that defines the view is:
```sql
CREATE OR REPLACE VIEW public.v <SQL-query-defining-the-view>;
```

The created view `v` is a virtual view. After that, BIRDS creates a trigger to initialize some temporary tables and triggers:

```sql 
CREATE OR REPLACE FUNCTION public.v_materialization()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    CREATE TEMPORARY TABLE __temp__Δ_ins_v ( LIKE public.v INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
    CREATE CONSTRAINT TRIGGER __temp__v_trigger_delta_action
    AFTER INSERT OR UPDATE OR DELETE ON 
        __temp__Δ_ins_v DEFERRABLE INITIALLY DEFERRED 
        FOR EACH ROW EXECUTE PROCEDURE public.v_delta_action();

    CREATE TEMPORARY TABLE __temp__Δ_del_v ( LIKE public.v INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
    CREATE CONSTRAINT TRIGGER __temp__v_trigger_delta_action
    AFTER INSERT OR UPDATE OR DELETE ON 
        __temp__Δ_del_v DEFERRABLE INITIALLY DEFERRED 
        FOR EACH ROW EXECUTE PROCEDURE public.v_delta_action();
  EXCEPTION
    ...
  END;
$$;

DROP TRIGGER IF EXISTS v_trigger_materialization ON public.v;
CREATE TRIGGER v_trigger_materialization
    BEFORE INSERT OR UPDATE OR DELETE ON
      public.v FOR EACH STATEMENT EXECUTE PROCEDURE public.v_materialization();
```

Next, BIRDS creates a trigger that handles any SQL DML statements on the view `v` to derive the view deltas:

```sql
CREATE OR REPLACE FUNCTION public.v_update() RETURNS TRIGGER LANGUAGE plpgsql
SECURITY DEFINER AS $$
  DECLARE
  ...
  BEGIN
    IF TG_OP = 'INSERT' THEN
      DELETE FROM __temp__Δ_del_v WHERE ROW(X) = NEW;
      INSERT INTO __temp__Δ_ins_v SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM __temp__Δ_ins_v WHERE ROW(X) = OLD;
      INSERT INTO __temp__Δ_del_v SELECT (OLD).*;
      DELETE FROM __temp__Δ_del_v WHERE ROW(X) = NEW;
      INSERT INTO __temp__Δ_ins_v SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      DELETE FROM __temp__Δ_ins_v WHERE ROW(X) = OLD;
      INSERT INTO __temp__Δ_del_v SELECT (OLD).*;
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

Finally, BIRDS creates a trigger function that implements the view update strategy. The trigger checks all the constraints, then computes the source deltas and applies them to the source tables.

```sql
CREATE OR REPLACE FUNCTION public.v_delta_action() RETURNS TRIGGER LANGUAGE plpgsql 
SECURITY DEFINER AS $$
DECLARE
... 
BEGIN
    IF EXISTS (SELECT WHERE <SQL-of-the-view-constraints> )
    THEN 
        RAISE check_violation USING MESSAGE = 'Invalid view update: constraints on the view are violated';
    END IF;
    IF EXISTS (SELECT WHERE <SQL-of-the-source-constraints> )
    THEN 
        RAISE check_violation USING MESSAGE = 'Invalid view update: constraints on the source relations are violated';
    END IF;

    CREATE TEMPORARY TABLE __dummy__delta__delete__s1 WITH OIDS ON COMMIT DROP AS 
    SELECT ... FROM (...) ;
    ...
    CREATE TEMPORARY TABLE __dummy__delta__delete__sn WITH OIDS ON COMMIT DROP AS 
    SELECT ... FROM (...) ;

    CREATE TEMPORARY TABLE __dummy__delta__insert__s1 WITH OIDS ON COMMIT DROP AS 
    SELECT ... FROM (...);
    ...
    CREATE TEMPORARY TABLE __dummy__delta__insert__sn WITH OIDS ON COMMIT DROP AS 
    SELECT ... FROM (...);

    FOR temprec IN ( SELECT * FROM __dummy__delta__delete__s1) LOOP 
            DELETE FROM public.s1 WHERE ROW(X) IS NOT DISTINCT FROM  temprec;
            END LOOP;
    DROP TABLE __dummy__delta__delete__s1;
    ...
    FOR temprec IN ( SELECT * FROM __dummy__delta__delete__sn) LOOP 
            DELETE FROM public.s1 WHERE ROW(X) IS NOT DISTINCT FROM  temprec;
            END LOOP;
    DROP TABLE __dummy__delta__delete__sn;

    INSERT INTO public.s1 SELECT * FROM  __dummy__delta__insert__s1; 
    ...
    INSERT INTO public.s1 SELECT * FROM  __dummy__delta__insert__sn; 
    DROP TABLE __dummy__delta__insert__s1;

    RETURN NULL;
EXCEPTION
    ...
END;
$$;
```