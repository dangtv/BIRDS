CREATE OR REPLACE VIEW public.tracks2 AS 
SELECT __dummy__.COL0 AS TRACK,__dummy__.COL1 AS RATING,__dummy__.COL2 AS ALBUM,__dummy__.COL3 AS QUANTITY 
FROM (SELECT DISTINCT tracks2_a4_0.COL0 AS COL0, tracks2_a4_0.COL1 AS COL1, tracks2_a4_0.COL2 AS COL2, tracks2_a4_0.COL3 AS COL3 
FROM (SELECT DISTINCT tracks2_med_a4_0.COL0 AS COL0, tracks2_med_a4_0.COL1 AS COL1, tracks2_med_a4_0.COL2 AS COL2, tracks2_med_a4_0.COL3 AS COL3 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS COL0, tracks1_a5_0.RATING AS COL1, tracks1_a5_0.ALBUM AS COL2, tracks1_a5_0.QUANTITY AS COL3 
FROM public.tracks1 AS tracks1_a5_0  ) AS tracks2_med_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks2_med_a4_0.COL0 AS COL0, 2018 AS COL1, tracks2_med_a4_0.COL1 AS COL2, tracks2_med_a4_0.COL2 AS COL3, tracks2_med_a4_0.COL3 AS COL4 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS COL0, tracks1_a5_0.RATING AS COL1, tracks1_a5_0.ALBUM AS COL2, tracks1_a5_0.QUANTITY AS COL3 
FROM public.tracks1 AS tracks1_a5_0  ) AS tracks2_med_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_med_a4_0.COL3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks2_med_a4_0.COL2 AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_med_a4_0.COL1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_med_a4_0.COL0 ) ) AS _derived_Δ_ins_tracks1_a5 
WHERE _derived_Δ_ins_tracks1_a5.COL4 IS NOT DISTINCT FROM tracks2_med_a4_0.COL3 AND _derived_Δ_ins_tracks1_a5.COL3 IS NOT DISTINCT FROM tracks2_med_a4_0.COL2 AND _derived_Δ_ins_tracks1_a5.COL2 IS NOT DISTINCT FROM tracks2_med_a4_0.COL1 AND _derived_Δ_ins_tracks1_a5.COL0 IS NOT DISTINCT FROM tracks2_med_a4_0.COL0 ) ) AS tracks2_a4_0  ) AS __dummy__;

DROP MATERIALIZED VIEW IF EXISTS public.__dummy__materialized_tracks2;

CREATE  MATERIALIZED VIEW public.__dummy__materialized_tracks2 AS 
SELECT * FROM public.tracks2;

CREATE OR REPLACE FUNCTION public.tracks2_delta_action()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  temprecΔ_del_tracks1 public.tracks1%ROWTYPE;
temprecΔ_ins_tracks1 public.tracks1%ROWTYPE;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'tracks2_delta_action_flag') THEN
        -- RAISE NOTICE 'execute procedure tracks2_delta_action';
        CREATE TEMPORARY TABLE tracks2_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        IF EXISTS (SELECT WHERE false )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid update on view';
        END IF;
        CREATE TEMPORARY TABLE Δ_del_tracks1 WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2,COL3,COL4) :: public.tracks1).* 
            FROM (SELECT DISTINCT Δ_del_tracks1_a5_0.COL0 AS COL0, Δ_del_tracks1_a5_0.COL1 AS COL1, Δ_del_tracks1_a5_0.COL2 AS COL2, Δ_del_tracks1_a5_0.COL3 AS COL3, Δ_del_tracks1_a5_0.COL4 AS COL4 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS COL0, tracks1_a5_0.DATE AS COL1, tracks1_a5_0.RATING AS COL2, tracks1_a5_0.ALBUM AS COL3, tracks1_a5_0.QUANTITY AS COL4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __dummy__materialized_tracks2_a4_0.TRACK AS COL0, __dummy__materialized_tracks2_a4_0.RATING AS COL1, __dummy__materialized_tracks2_a4_0.ALBUM AS COL2, __dummy__materialized_tracks2_a4_0.QUANTITY AS COL3 
FROM public.__dummy__materialized_tracks2 AS __dummy__materialized_tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_tracks2 AS __temp__Δ_del_tracks2_a4 
WHERE __temp__Δ_del_tracks2_a4.QUANTITY IS NOT DISTINCT FROM __dummy__materialized_tracks2_a4_0.QUANTITY AND __temp__Δ_del_tracks2_a4.ALBUM IS NOT DISTINCT FROM __dummy__materialized_tracks2_a4_0.ALBUM AND __temp__Δ_del_tracks2_a4.RATING IS NOT DISTINCT FROM __dummy__materialized_tracks2_a4_0.RATING AND __temp__Δ_del_tracks2_a4.TRACK IS NOT DISTINCT FROM __dummy__materialized_tracks2_a4_0.TRACK )  UNION SELECT DISTINCT __temp__Δ_ins_tracks2_a4_0.TRACK AS COL0, __temp__Δ_ins_tracks2_a4_0.RATING AS COL1, __temp__Δ_ins_tracks2_a4_0.ALBUM AS COL2, __temp__Δ_ins_tracks2_a4_0.QUANTITY AS COL3 
FROM __temp__Δ_ins_tracks2 AS __temp__Δ_ins_tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.COL3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.COL2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.COL1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.COL0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS Δ_del_tracks1_a5_0  ) AS Δ_del_tracks1_extra_alias;

CREATE TEMPORARY TABLE Δ_ins_tracks1 WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2,COL3,COL4) :: public.tracks1).* 
            FROM (SELECT DISTINCT Δ_ins_tracks1_a5_0.COL0 AS COL0, Δ_ins_tracks1_a5_0.COL1 AS COL1, Δ_ins_tracks1_a5_0.COL2 AS COL2, Δ_ins_tracks1_a5_0.COL3 AS COL3, Δ_ins_tracks1_a5_0.COL4 AS COL4 
FROM (SELECT DISTINCT tracks2_a4_0.COL0 AS COL0, 2018 AS COL1, tracks2_a4_0.COL1 AS COL2, tracks2_a4_0.COL2 AS COL3, tracks2_a4_0.COL3 AS COL4 
FROM (SELECT DISTINCT __dummy__materialized_tracks2_a4_0.TRACK AS COL0, __dummy__materialized_tracks2_a4_0.RATING AS COL1, __dummy__materialized_tracks2_a4_0.ALBUM AS COL2, __dummy__materialized_tracks2_a4_0.QUANTITY AS COL3 
FROM public.__dummy__materialized_tracks2 AS __dummy__materialized_tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_tracks2 AS __temp__Δ_del_tracks2_a4 
WHERE __temp__Δ_del_tracks2_a4.QUANTITY IS NOT DISTINCT FROM __dummy__materialized_tracks2_a4_0.QUANTITY AND __temp__Δ_del_tracks2_a4.ALBUM IS NOT DISTINCT FROM __dummy__materialized_tracks2_a4_0.ALBUM AND __temp__Δ_del_tracks2_a4.RATING IS NOT DISTINCT FROM __dummy__materialized_tracks2_a4_0.RATING AND __temp__Δ_del_tracks2_a4.TRACK IS NOT DISTINCT FROM __dummy__materialized_tracks2_a4_0.TRACK )  UNION SELECT DISTINCT __temp__Δ_ins_tracks2_a4_0.TRACK AS COL0, __temp__Δ_ins_tracks2_a4_0.RATING AS COL1, __temp__Δ_ins_tracks2_a4_0.ALBUM AS COL2, __temp__Δ_ins_tracks2_a4_0.QUANTITY AS COL3 
FROM __temp__Δ_ins_tracks2 AS __temp__Δ_ins_tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.COL3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks2_a4_0.COL2 AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.COL1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.COL0 ) ) AS Δ_ins_tracks1_a5_0  ) AS Δ_ins_tracks1_extra_alias; 

FOR temprecΔ_del_tracks1 IN ( SELECT * FROM Δ_del_tracks1) LOOP 
            DELETE FROM public.tracks1 WHERE ROW(TRACK,DATE,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM  temprecΔ_del_tracks1;
            END LOOP;
DROP TABLE Δ_del_tracks1;

INSERT INTO public.tracks1 SELECT * FROM  Δ_ins_tracks1; 
DROP TABLE Δ_ins_tracks1;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.tracks2';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.tracks2 ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

CREATE OR REPLACE FUNCTION public.tracks2_materialization()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '__temp__Δ_ins_tracks2' OR table_name = '__temp__Δ_del_tracks2')
    THEN
        -- RAISE NOTICE 'execute procedure tracks2_materialization';
        REFRESH MATERIALIZED VIEW public.__dummy__materialized_tracks2;
        CREATE TEMPORARY TABLE __temp__Δ_ins_tracks2 ( LIKE public.__dummy__materialized_tracks2 INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__tracks2_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_ins_tracks2 DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.tracks2_delta_action();

        CREATE TEMPORARY TABLE __temp__Δ_del_tracks2 ( LIKE public.__dummy__materialized_tracks2 INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__tracks2_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_del_tracks2 DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.tracks2_delta_action();
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.tracks2';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.tracks2 ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS tracks2_trigger_materialization ON public.tracks2;
CREATE TRIGGER tracks2_trigger_materialization
    BEFORE INSERT OR UPDATE OR DELETE ON
      public.tracks2 FOR EACH STATEMENT EXECUTE PROCEDURE public.tracks2_materialization();

CREATE OR REPLACE FUNCTION public.tracks2_update()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    -- RAISE NOTICE 'execute procedure tracks2_update';
    IF TG_OP = 'INSERT' THEN
      -- raise notice 'NEW: %', NEW;
      DELETE FROM __temp__Δ_del_tracks2 WHERE ROW(TRACK,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM NEW;
      INSERT INTO __temp__Δ_ins_tracks2 SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM __temp__Δ_ins_tracks2 WHERE ROW(TRACK,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM OLD;
      INSERT INTO __temp__Δ_del_tracks2 SELECT (OLD).*;
      DELETE FROM __temp__Δ_del_tracks2 WHERE ROW(TRACK,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM NEW;
      INSERT INTO __temp__Δ_ins_tracks2 SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      -- raise notice 'OLD: %', OLD;
      DELETE FROM __temp__Δ_ins_tracks2 WHERE ROW(TRACK,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM OLD;
      INSERT INTO __temp__Δ_del_tracks2 SELECT (OLD).*;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.tracks2';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.tracks2 ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS tracks2_trigger_update ON public.tracks2;
CREATE TRIGGER tracks2_trigger_update
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.tracks2 FOR EACH ROW EXECUTE PROCEDURE public.tracks2_update();

