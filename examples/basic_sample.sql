CREATE OR REPLACE VIEW public.v AS 
SELECT __dummy__.COL0 AS X 
FROM (SELECT DISTINCT v_a1_0.COL0 AS COL0 
FROM (SELECT DISTINCT v_med_a1_0.COL0 AS COL0 
FROM (SELECT DISTINCT s1_a1_0.X AS COL0 
FROM public.s1 AS s1_a1_0   UNION SELECT DISTINCT s2_a1_0.X AS COL0 
FROM public.s2 AS s2_a1_0  ) AS v_med_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT v_med_a1_0.COL0 AS COL0 
FROM (SELECT DISTINCT s1_a1_0.X AS COL0 
FROM public.s1 AS s1_a1_0   UNION SELECT DISTINCT s2_a1_0.X AS COL0 
FROM public.s2 AS s2_a1_0  ) AS v_med_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.s1 AS s1_a1 
WHERE s1_a1.X IS NOT DISTINCT FROM v_med_a1_0.COL0 ) AND NOT EXISTS ( SELECT * 
FROM public.s2 AS s2_a1 
WHERE s2_a1.X IS NOT DISTINCT FROM v_med_a1_0.COL0 ) ) AS _derived_Δ_ins_s1_a1 
WHERE _derived_Δ_ins_s1_a1.COL0 IS NOT DISTINCT FROM v_med_a1_0.COL0 ) ) AS v_a1_0  ) AS __dummy__;

DROP MATERIALIZED VIEW IF EXISTS public.__dummy__materialized_v;

CREATE  MATERIALIZED VIEW public.__dummy__materialized_v AS 
SELECT * FROM public.v;

CREATE OR REPLACE FUNCTION public.v_delta_action()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  temprecΔ_del_s1 public.s1%ROWTYPE;
temprecΔ_del_s2 public.s2%ROWTYPE;
temprecΔ_ins_s1 public.s1%ROWTYPE;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'v_delta_action_flag') THEN
        -- RAISE NOTICE 'execute procedure v_delta_action';
        CREATE TEMPORARY TABLE v_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        IF EXISTS (SELECT WHERE false )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid update on view';
        END IF;
        CREATE TEMPORARY TABLE Δ_del_s1 WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0) :: public.s1).* 
            FROM (SELECT DISTINCT Δ_del_s1_a1_0.COL0 AS COL0 
FROM (SELECT DISTINCT s1_a1_0.X AS COL0 
FROM public.s1 AS s1_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __dummy__materialized_v_a1_0.X AS COL0 
FROM public.__dummy__materialized_v AS __dummy__materialized_v_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_v AS __temp__Δ_del_v_a1 
WHERE __temp__Δ_del_v_a1.X IS NOT DISTINCT FROM __dummy__materialized_v_a1_0.X )  UNION SELECT DISTINCT __temp__Δ_ins_v_a1_0.X AS COL0 
FROM __temp__Δ_ins_v AS __temp__Δ_ins_v_a1_0  ) AS v_a1 
WHERE v_a1.COL0 IS NOT DISTINCT FROM s1_a1_0.X ) ) AS Δ_del_s1_a1_0  ) AS Δ_del_s1_extra_alias;

CREATE TEMPORARY TABLE Δ_del_s2 WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0) :: public.s2).* 
            FROM (SELECT DISTINCT Δ_del_s2_a1_0.COL0 AS COL0 
FROM (SELECT DISTINCT s2_a1_0.X AS COL0 
FROM public.s2 AS s2_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __dummy__materialized_v_a1_0.X AS COL0 
FROM public.__dummy__materialized_v AS __dummy__materialized_v_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_v AS __temp__Δ_del_v_a1 
WHERE __temp__Δ_del_v_a1.X IS NOT DISTINCT FROM __dummy__materialized_v_a1_0.X )  UNION SELECT DISTINCT __temp__Δ_ins_v_a1_0.X AS COL0 
FROM __temp__Δ_ins_v AS __temp__Δ_ins_v_a1_0  ) AS v_a1 
WHERE v_a1.COL0 IS NOT DISTINCT FROM s2_a1_0.X ) ) AS Δ_del_s2_a1_0  ) AS Δ_del_s2_extra_alias;

CREATE TEMPORARY TABLE Δ_ins_s1 WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0) :: public.s1).* 
            FROM (SELECT DISTINCT Δ_ins_s1_a1_0.COL0 AS COL0 
FROM (SELECT DISTINCT v_a1_0.COL0 AS COL0 
FROM (SELECT DISTINCT __dummy__materialized_v_a1_0.X AS COL0 
FROM public.__dummy__materialized_v AS __dummy__materialized_v_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_v AS __temp__Δ_del_v_a1 
WHERE __temp__Δ_del_v_a1.X IS NOT DISTINCT FROM __dummy__materialized_v_a1_0.X )  UNION SELECT DISTINCT __temp__Δ_ins_v_a1_0.X AS COL0 
FROM __temp__Δ_ins_v AS __temp__Δ_ins_v_a1_0  ) AS v_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.s1 AS s1_a1 
WHERE s1_a1.X IS NOT DISTINCT FROM v_a1_0.COL0 ) AND NOT EXISTS ( SELECT * 
FROM public.s2 AS s2_a1 
WHERE s2_a1.X IS NOT DISTINCT FROM v_a1_0.COL0 ) ) AS Δ_ins_s1_a1_0  ) AS Δ_ins_s1_extra_alias; 

FOR temprecΔ_del_s1 IN ( SELECT * FROM Δ_del_s1) LOOP 
            DELETE FROM public.s1 WHERE ROW(X) IS NOT DISTINCT FROM  temprecΔ_del_s1;
            END LOOP;
DROP TABLE Δ_del_s1;

FOR temprecΔ_del_s2 IN ( SELECT * FROM Δ_del_s2) LOOP 
            DELETE FROM public.s2 WHERE ROW(X) IS NOT DISTINCT FROM  temprecΔ_del_s2;
            END LOOP;
DROP TABLE Δ_del_s2;

INSERT INTO public.s1 SELECT * FROM  Δ_ins_s1; 
DROP TABLE Δ_ins_s1;
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
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '__temp__Δ_ins_v' OR table_name = '__temp__Δ_del_v')
    THEN
        -- RAISE NOTICE 'execute procedure v_materialization';
        REFRESH MATERIALIZED VIEW public.__dummy__materialized_v;
        CREATE TEMPORARY TABLE __temp__Δ_ins_v ( LIKE public.__dummy__materialized_v INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__v_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_ins_v DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.v_delta_action();

        CREATE TEMPORARY TABLE __temp__Δ_del_v ( LIKE public.__dummy__materialized_v INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__v_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_del_v DEFERRABLE INITIALLY DEFERRED 
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
      -- raise notice 'NEW: %', NEW;
      DELETE FROM __temp__Δ_del_v WHERE ROW(X) IS NOT DISTINCT FROM NEW;
      INSERT INTO __temp__Δ_ins_v SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM __temp__Δ_ins_v WHERE ROW(X) IS NOT DISTINCT FROM OLD;
      INSERT INTO __temp__Δ_del_v SELECT (OLD).*;
      DELETE FROM __temp__Δ_del_v WHERE ROW(X) IS NOT DISTINCT FROM NEW;
      INSERT INTO __temp__Δ_ins_v SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      -- raise notice 'OLD: %', OLD;
      DELETE FROM __temp__Δ_ins_v WHERE ROW(X) IS NOT DISTINCT FROM OLD;
      INSERT INTO __temp__Δ_del_v SELECT (OLD).*;
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

