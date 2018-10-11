/*_____get datalog program_______
?- v(X).

v(V_A1_X) :- v_med(V_A1_X) , not __dummy__delta__insert__s1(V_A1_X).

v_med(X) :- s1(X).

v_med(X) :- s2(X).

__dummy__delta__insert__s1(X) :- v_med(X) , not s1(X) , not s2(X).

______________*/

CREATE OR REPLACE VIEW public.v AS 
SELECT __dummy__.col0 AS X 
FROM (SELECT DISTINCT v_a1_0.col0 AS col0 
FROM (SELECT DISTINCT v_med_a1_0.col0 AS col0 
FROM (SELECT DISTINCT s1_a1_0.X AS col0 
FROM public.s1 AS s1_a1_0   UNION SELECT DISTINCT s2_a1_0.X AS col0 
FROM public.s2 AS s2_a1_0  ) AS v_med_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT v_med_a1_0.col0 AS col0 
FROM (SELECT DISTINCT s1_a1_0.X AS col0 
FROM public.s1 AS s1_a1_0   UNION SELECT DISTINCT s2_a1_0.X AS col0 
FROM public.s2 AS s2_a1_0  ) AS v_med_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.s1 AS s1_a1 
WHERE s1_a1.X IS NOT DISTINCT FROM v_med_a1_0.col0 ) AND NOT EXISTS ( SELECT * 
FROM public.s2 AS s2_a1 
WHERE s2_a1.X IS NOT DISTINCT FROM v_med_a1_0.col0 ) ) AS __dummy__delta__insert__s1_a1 
WHERE __dummy__delta__insert__s1_a1.col0 IS NOT DISTINCT FROM v_med_a1_0.col0 ) ) AS v_a1_0  ) AS __dummy__;

CREATE OR REPLACE FUNCTION public.v_delta_action()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  temprec__dummy__delta__delete__s1 public.s1%ROWTYPE;
temprec__dummy__delta__delete__s2 public.s2%ROWTYPE;
temprec__dummy__delta__insert__s1 public.s1%ROWTYPE;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'v_delta_action_flag') THEN
        -- RAISE NOTICE 'execute procedure v_delta_action';
        CREATE TEMPORARY TABLE v_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        CREATE TEMPORARY TABLE __dummy__delta__delete__s1 WITH OIDS ON COMMIT DROP AS SELECT (ROW(col0) :: public.s1).* 
            FROM (SELECT DISTINCT __dummy__delta__delete__s1_a1_0.col0 AS col0 
FROM (SELECT DISTINCT s1_a1_0.X AS col0 
FROM public.s1 AS s1_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__v_a1_0.X AS col0 
FROM __temp__v AS __temp__v_a1_0  ) AS v_a1 
WHERE v_a1.col0 IS NOT DISTINCT FROM s1_a1_0.X ) ) AS __dummy__delta__delete__s1_a1_0  ) AS __dummy__delta__delete__s1_extra_alias;

CREATE TEMPORARY TABLE __dummy__delta__delete__s2 WITH OIDS ON COMMIT DROP AS SELECT (ROW(col0) :: public.s2).* 
            FROM (SELECT DISTINCT __dummy__delta__delete__s2_a1_0.col0 AS col0 
FROM (SELECT DISTINCT s2_a1_0.X AS col0 
FROM public.s2 AS s2_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__v_a1_0.X AS col0 
FROM __temp__v AS __temp__v_a1_0  ) AS v_a1 
WHERE v_a1.col0 IS NOT DISTINCT FROM s2_a1_0.X ) ) AS __dummy__delta__delete__s2_a1_0  ) AS __dummy__delta__delete__s2_extra_alias;

CREATE TEMPORARY TABLE __dummy__delta__insert__s1 WITH OIDS ON COMMIT DROP AS SELECT (ROW(col0) :: public.s1).* 
            FROM (SELECT DISTINCT __dummy__delta__insert__s1_a1_0.col0 AS col0 
FROM (SELECT DISTINCT v_a1_0.col0 AS col0 
FROM (SELECT DISTINCT __temp__v_a1_0.X AS col0 
FROM __temp__v AS __temp__v_a1_0  ) AS v_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.s1 AS s1_a1 
WHERE s1_a1.X IS NOT DISTINCT FROM v_a1_0.col0 ) AND NOT EXISTS ( SELECT * 
FROM public.s2 AS s2_a1 
WHERE s2_a1.X IS NOT DISTINCT FROM v_a1_0.col0 ) ) AS __dummy__delta__insert__s1_a1_0  ) AS __dummy__delta__insert__s1_extra_alias; 

FOR temprec__dummy__delta__delete__s1 IN ( SELECT * FROM __dummy__delta__delete__s1) LOOP 
            DELETE FROM public.s1 WHERE ROW(X) IS NOT DISTINCT FROM  temprec__dummy__delta__delete__s1;
            END LOOP;
DROP TABLE __dummy__delta__delete__s1;

FOR temprec__dummy__delta__delete__s2 IN ( SELECT * FROM __dummy__delta__delete__s2) LOOP 
            DELETE FROM public.s2 WHERE ROW(X) IS NOT DISTINCT FROM  temprec__dummy__delta__delete__s2;
            END LOOP;
DROP TABLE __dummy__delta__delete__s2;

INSERT INTO public.s1 SELECT * FROM  __dummy__delta__insert__s1; 
DROP TABLE __dummy__delta__insert__s1;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.v';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.v ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

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
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '__temp__v')
    THEN
        -- RAISE NOTICE 'execute procedure v_materialization';
        CREATE TEMPORARY TABLE __temp__v WITH OIDS ON COMMIT DROP AS SELECT * FROM public.v;
        CREATE CONSTRAINT TRIGGER __temp__peer1_public_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__v DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.v_delta_action();
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.v';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.v ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS v_trigger_materialization ON public.v;
CREATE TRIGGER v_trigger_materialization
    BEFORE INSERT OR UPDATE OR DELETE ON
      public.v FOR EACH STATEMENT EXECUTE PROCEDURE public.v_materialization();

CREATE OR REPLACE FUNCTION public.v_update()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    -- RAISE NOTICE 'execute procedure v_update';
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
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.v';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.v ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS v_trigger_update ON public.v;
CREATE TRIGGER v_trigger_update
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.v FOR EACH ROW EXECUTE PROCEDURE public.v_update();

