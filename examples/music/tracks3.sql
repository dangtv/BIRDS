CREATE OR REPLACE VIEW public.tracks3 AS 
SELECT __dummy__.COL0 AS TRACK,__dummy__.COL1 AS RATING,__dummy__.COL2 AS ALBUM,__dummy__.COL3 AS QUANTITY 
FROM (SELECT DISTINCT tracks3_a4_0.COL0 AS COL0, tracks3_a4_0.COL1 AS COL1, tracks3_a4_0.COL2 AS COL2, tracks3_a4_0.COL3 AS COL3 
FROM (SELECT DISTINCT tracks3_med_a4_0.COL0 AS COL0, tracks3_med_a4_0.COL1 AS COL1, tracks3_med_a4_0.COL2 AS COL2, tracks3_med_a4_0.COL3 AS COL3 
FROM (SELECT DISTINCT tracks2_a4_0.TRACK AS COL0, tracks2_a4_0.RATING AS COL1, tracks2_a4_0.ALBUM AS COL2, tracks2_a4_0.QUANTITY AS COL3 
FROM public.tracks2 AS tracks2_a4_0 
WHERE tracks2_a4_0.QUANTITY  >  2 ) AS tracks3_med_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks3_med_a4_0.COL0 AS COL0, tracks3_med_a4_0.COL1 AS COL1, tracks3_med_a4_0.COL2 AS COL2, tracks3_med_a4_0.COL3 AS COL3 
FROM (SELECT DISTINCT tracks2_a4_0.TRACK AS COL0, tracks2_a4_0.RATING AS COL1, tracks2_a4_0.ALBUM AS COL2, tracks2_a4_0.QUANTITY AS COL3 
FROM public.tracks2 AS tracks2_a4_0 
WHERE tracks2_a4_0.QUANTITY  >  2 ) AS tracks3_med_a4_0 
WHERE tracks3_med_a4_0.COL3  >  2 AND NOT EXISTS ( SELECT * 
FROM public.tracks2 AS tracks2_a4 
WHERE tracks2_a4.QUANTITY IS NOT DISTINCT FROM tracks3_med_a4_0.COL3 AND tracks2_a4.ALBUM IS NOT DISTINCT FROM tracks3_med_a4_0.COL2 AND tracks2_a4.RATING IS NOT DISTINCT FROM tracks3_med_a4_0.COL1 AND tracks2_a4.TRACK IS NOT DISTINCT FROM tracks3_med_a4_0.COL0 ) ) AS _derived_Δ_ins_tracks2_a4 
WHERE _derived_Δ_ins_tracks2_a4.COL3 IS NOT DISTINCT FROM tracks3_med_a4_0.COL3 AND _derived_Δ_ins_tracks2_a4.COL2 IS NOT DISTINCT FROM tracks3_med_a4_0.COL2 AND _derived_Δ_ins_tracks2_a4.COL1 IS NOT DISTINCT FROM tracks3_med_a4_0.COL1 AND _derived_Δ_ins_tracks2_a4.COL0 IS NOT DISTINCT FROM tracks3_med_a4_0.COL0 ) ) AS tracks3_a4_0  ) AS __dummy__;

DROP MATERIALIZED VIEW IF EXISTS public.__dummy__materialized_tracks3;

CREATE  MATERIALIZED VIEW public.__dummy__materialized_tracks3 AS 
SELECT * FROM public.tracks3;

CREATE OR REPLACE FUNCTION public.tracks3_delta_action()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  temprecΔ_del_tracks2 public.tracks2%ROWTYPE;
temprecΔ_ins_tracks2 public.tracks2%ROWTYPE;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'tracks3_delta_action_flag') THEN
        -- RAISE NOTICE 'execute procedure tracks3_delta_action';
        CREATE TEMPORARY TABLE tracks3_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        IF EXISTS (SELECT  
FROM (SELECT  
FROM (SELECT  
FROM (SELECT DISTINCT __dummy__materialized_tracks3_a4_0.TRACK AS COL0, __dummy__materialized_tracks3_a4_0.RATING AS COL1, __dummy__materialized_tracks3_a4_0.ALBUM AS COL2, __dummy__materialized_tracks3_a4_0.QUANTITY AS COL3 
FROM public.__dummy__materialized_tracks3 AS __dummy__materialized_tracks3_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_tracks3 AS __temp__Δ_del_tracks3_a4 
WHERE __temp__Δ_del_tracks3_a4.QUANTITY IS NOT DISTINCT FROM __dummy__materialized_tracks3_a4_0.QUANTITY AND __temp__Δ_del_tracks3_a4.ALBUM IS NOT DISTINCT FROM __dummy__materialized_tracks3_a4_0.ALBUM AND __temp__Δ_del_tracks3_a4.RATING IS NOT DISTINCT FROM __dummy__materialized_tracks3_a4_0.RATING AND __temp__Δ_del_tracks3_a4.TRACK IS NOT DISTINCT FROM __dummy__materialized_tracks3_a4_0.TRACK )  UNION SELECT DISTINCT __temp__Δ_ins_tracks3_a4_0.TRACK AS COL0, __temp__Δ_ins_tracks3_a4_0.RATING AS COL1, __temp__Δ_ins_tracks3_a4_0.ALBUM AS COL2, __temp__Δ_ins_tracks3_a4_0.QUANTITY AS COL3 
FROM __temp__Δ_ins_tracks3 AS __temp__Δ_ins_tracks3_a4_0  ) AS tracks3_a4_0 
WHERE tracks3_a4_0.COL3  <=  2 ) AS ⊥_a0_0  ) AS __dummy__ )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid update on view';
        END IF;
        CREATE TEMPORARY TABLE Δ_del_tracks2 WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2,COL3) :: public.tracks2).* 
            FROM (SELECT DISTINCT Δ_del_tracks2_a4_0.COL0 AS COL0, Δ_del_tracks2_a4_0.COL1 AS COL1, Δ_del_tracks2_a4_0.COL2 AS COL2, Δ_del_tracks2_a4_0.COL3 AS COL3 
FROM (SELECT DISTINCT tracks2_a4_0.TRACK AS COL0, tracks2_a4_0.RATING AS COL1, tracks2_a4_0.ALBUM AS COL2, tracks2_a4_0.QUANTITY AS COL3 
FROM public.tracks2 AS tracks2_a4_0 
WHERE tracks2_a4_0.QUANTITY  >  2 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __dummy__materialized_tracks3_a4_0.TRACK AS COL0, __dummy__materialized_tracks3_a4_0.RATING AS COL1, __dummy__materialized_tracks3_a4_0.ALBUM AS COL2, __dummy__materialized_tracks3_a4_0.QUANTITY AS COL3 
FROM public.__dummy__materialized_tracks3 AS __dummy__materialized_tracks3_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_tracks3 AS __temp__Δ_del_tracks3_a4 
WHERE __temp__Δ_del_tracks3_a4.QUANTITY IS NOT DISTINCT FROM __dummy__materialized_tracks3_a4_0.QUANTITY AND __temp__Δ_del_tracks3_a4.ALBUM IS NOT DISTINCT FROM __dummy__materialized_tracks3_a4_0.ALBUM AND __temp__Δ_del_tracks3_a4.RATING IS NOT DISTINCT FROM __dummy__materialized_tracks3_a4_0.RATING AND __temp__Δ_del_tracks3_a4.TRACK IS NOT DISTINCT FROM __dummy__materialized_tracks3_a4_0.TRACK )  UNION SELECT DISTINCT __temp__Δ_ins_tracks3_a4_0.TRACK AS COL0, __temp__Δ_ins_tracks3_a4_0.RATING AS COL1, __temp__Δ_ins_tracks3_a4_0.ALBUM AS COL2, __temp__Δ_ins_tracks3_a4_0.QUANTITY AS COL3 
FROM __temp__Δ_ins_tracks3 AS __temp__Δ_ins_tracks3_a4_0  ) AS tracks3_a4 
WHERE tracks3_a4.COL3 IS NOT DISTINCT FROM tracks2_a4_0.QUANTITY AND tracks3_a4.COL2 IS NOT DISTINCT FROM tracks2_a4_0.ALBUM AND tracks3_a4.COL1 IS NOT DISTINCT FROM tracks2_a4_0.RATING AND tracks3_a4.COL0 IS NOT DISTINCT FROM tracks2_a4_0.TRACK ) ) AS Δ_del_tracks2_a4_0  ) AS Δ_del_tracks2_extra_alias;

CREATE TEMPORARY TABLE Δ_ins_tracks2 WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2,COL3) :: public.tracks2).* 
            FROM (SELECT DISTINCT Δ_ins_tracks2_a4_0.COL0 AS COL0, Δ_ins_tracks2_a4_0.COL1 AS COL1, Δ_ins_tracks2_a4_0.COL2 AS COL2, Δ_ins_tracks2_a4_0.COL3 AS COL3 
FROM (SELECT DISTINCT tracks3_a4_0.COL0 AS COL0, tracks3_a4_0.COL1 AS COL1, tracks3_a4_0.COL2 AS COL2, tracks3_a4_0.COL3 AS COL3 
FROM (SELECT DISTINCT __dummy__materialized_tracks3_a4_0.TRACK AS COL0, __dummy__materialized_tracks3_a4_0.RATING AS COL1, __dummy__materialized_tracks3_a4_0.ALBUM AS COL2, __dummy__materialized_tracks3_a4_0.QUANTITY AS COL3 
FROM public.__dummy__materialized_tracks3 AS __dummy__materialized_tracks3_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_tracks3 AS __temp__Δ_del_tracks3_a4 
WHERE __temp__Δ_del_tracks3_a4.QUANTITY IS NOT DISTINCT FROM __dummy__materialized_tracks3_a4_0.QUANTITY AND __temp__Δ_del_tracks3_a4.ALBUM IS NOT DISTINCT FROM __dummy__materialized_tracks3_a4_0.ALBUM AND __temp__Δ_del_tracks3_a4.RATING IS NOT DISTINCT FROM __dummy__materialized_tracks3_a4_0.RATING AND __temp__Δ_del_tracks3_a4.TRACK IS NOT DISTINCT FROM __dummy__materialized_tracks3_a4_0.TRACK )  UNION SELECT DISTINCT __temp__Δ_ins_tracks3_a4_0.TRACK AS COL0, __temp__Δ_ins_tracks3_a4_0.RATING AS COL1, __temp__Δ_ins_tracks3_a4_0.ALBUM AS COL2, __temp__Δ_ins_tracks3_a4_0.QUANTITY AS COL3 
FROM __temp__Δ_ins_tracks3 AS __temp__Δ_ins_tracks3_a4_0  ) AS tracks3_a4_0 
WHERE tracks3_a4_0.COL3  >  2 AND NOT EXISTS ( SELECT * 
FROM public.tracks2 AS tracks2_a4 
WHERE tracks2_a4.QUANTITY IS NOT DISTINCT FROM tracks3_a4_0.COL3 AND tracks2_a4.ALBUM IS NOT DISTINCT FROM tracks3_a4_0.COL2 AND tracks2_a4.RATING IS NOT DISTINCT FROM tracks3_a4_0.COL1 AND tracks2_a4.TRACK IS NOT DISTINCT FROM tracks3_a4_0.COL0 ) ) AS Δ_ins_tracks2_a4_0  ) AS Δ_ins_tracks2_extra_alias; 

FOR temprecΔ_del_tracks2 IN ( SELECT * FROM Δ_del_tracks2) LOOP 
            DELETE FROM public.tracks2 WHERE ROW(TRACK,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM  temprecΔ_del_tracks2;
            END LOOP;
DROP TABLE Δ_del_tracks2;

INSERT INTO public.tracks2 SELECT * FROM  Δ_ins_tracks2; 
DROP TABLE Δ_ins_tracks2;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.tracks3';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.tracks3 ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

CREATE OR REPLACE FUNCTION public.tracks3_materialization()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '__temp__Δ_ins_tracks3' OR table_name = '__temp__Δ_del_tracks3')
    THEN
        -- RAISE NOTICE 'execute procedure tracks3_materialization';
        REFRESH MATERIALIZED VIEW public.__dummy__materialized_tracks3;
        CREATE TEMPORARY TABLE __temp__Δ_ins_tracks3 ( LIKE public.__dummy__materialized_tracks3 INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__tracks3_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_ins_tracks3 DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.tracks3_delta_action();

        CREATE TEMPORARY TABLE __temp__Δ_del_tracks3 ( LIKE public.__dummy__materialized_tracks3 INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__tracks3_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_del_tracks3 DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.tracks3_delta_action();
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.tracks3';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.tracks3 ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS tracks3_trigger_materialization ON public.tracks3;
CREATE TRIGGER tracks3_trigger_materialization
    BEFORE INSERT OR UPDATE OR DELETE ON
      public.tracks3 FOR EACH STATEMENT EXECUTE PROCEDURE public.tracks3_materialization();

CREATE OR REPLACE FUNCTION public.tracks3_update()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    -- RAISE NOTICE 'execute procedure tracks3_update';
    IF TG_OP = 'INSERT' THEN
      -- raise notice 'NEW: %', NEW;
      DELETE FROM __temp__Δ_del_tracks3 WHERE ROW(TRACK,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM NEW;
      INSERT INTO __temp__Δ_ins_tracks3 SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM __temp__Δ_ins_tracks3 WHERE ROW(TRACK,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM OLD;
      INSERT INTO __temp__Δ_del_tracks3 SELECT (OLD).*;
      DELETE FROM __temp__Δ_del_tracks3 WHERE ROW(TRACK,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM NEW;
      INSERT INTO __temp__Δ_ins_tracks3 SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      -- raise notice 'OLD: %', OLD;
      DELETE FROM __temp__Δ_ins_tracks3 WHERE ROW(TRACK,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM OLD;
      INSERT INTO __temp__Δ_del_tracks3 SELECT (OLD).*;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.tracks3';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.tracks3 ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS tracks3_trigger_update ON public.tracks3;
CREATE TRIGGER tracks3_trigger_update
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.tracks3 FOR EACH ROW EXECUTE PROCEDURE public.tracks3_update();

