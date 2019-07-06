/*view definition (get):
ced(EMP_NAME, DEPT_NAME) :- p_0(EMP_NAME, DEPT_NAME).
p_0(EMP_NAME, DEPT_NAME) :- ed(EMP_NAME, DEPT_NAME) , not eed(EMP_NAME, DEPT_NAME).
*/
CREATE OR REPLACE VIEW public.ced AS 
SELECT __dummy__.COL0 AS EMP_NAME,__dummy__.COL1 AS DEPT_NAME 
FROM (SELECT DISTINCT ced_a2_0.COL0 AS COL0, ced_a2_0.COL1 AS COL1 
FROM (SELECT DISTINCT p_0_a2_0.COL0 AS COL0, p_0_a2_0.COL1 AS COL1 
FROM (SELECT DISTINCT ed_a2_0.EMP_NAME AS COL0, ed_a2_0.DEPT_NAME AS COL1 
FROM public.ed AS ed_a2_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.eed AS eed_a2 
WHERE eed_a2.DEPT_NAME = ed_a2_0.DEPT_NAME AND eed_a2.EMP_NAME = ed_a2_0.EMP_NAME ) ) AS p_0_a2_0  ) AS ced_a2_0  ) AS __dummy__;


CREATE OR REPLACE FUNCTION public.ced_delta_action()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  deletion_data text;
  insertion_data text;
  json_data text;
  result text;
  user_name text;
  temprecΔ_del_eed public.eed%ROWTYPE;
temprecΔ_ins_ed public.ed%ROWTYPE;
temprecΔ_ins_eed public.eed%ROWTYPE;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'ced_delta_action_flag') THEN
        -- RAISE LOG 'execute procedure ced_delta_action';
        CREATE TEMPORARY TABLE ced_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        IF EXISTS (SELECT WHERE false )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid update on view';
        END IF;
        CREATE TEMPORARY TABLE Δ_del_eed WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1) :: public.eed).* 
            FROM (SELECT DISTINCT Δ_del_eed_a2_0.COL0 AS COL0, Δ_del_eed_a2_0.COL1 AS COL1 
FROM (SELECT DISTINCT eed_a2_1.EMP_NAME AS COL0, eed_a2_1.DEPT_NAME AS COL1 
FROM (SELECT DISTINCT ced_a2_0.EMP_NAME AS COL0, ced_a2_0.DEPT_NAME AS COL1 
FROM public.ced AS ced_a2_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_ced AS __temp__Δ_del_ced_a2 
WHERE __temp__Δ_del_ced_a2.DEPT_NAME = ced_a2_0.DEPT_NAME AND __temp__Δ_del_ced_a2.EMP_NAME = ced_a2_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_ced_a2_0.EMP_NAME AS COL0, __temp__Δ_ins_ced_a2_0.DEPT_NAME AS COL1 
FROM __temp__Δ_ins_ced AS __temp__Δ_ins_ced_a2_0  ) AS new_ced_a2_0, public.eed AS eed_a2_1 
WHERE eed_a2_1.DEPT_NAME = new_ced_a2_0.COL1 AND eed_a2_1.EMP_NAME = new_ced_a2_0.COL0 ) AS Δ_del_eed_a2_0  ) AS Δ_del_eed_extra_alias;

CREATE TEMPORARY TABLE Δ_ins_ed WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1) :: public.ed).* 
            FROM (SELECT DISTINCT Δ_ins_ed_a2_0.COL0 AS COL0, Δ_ins_ed_a2_0.COL1 AS COL1 
FROM (SELECT DISTINCT new_ced_a2_0.COL0 AS COL0, new_ced_a2_0.COL1 AS COL1 
FROM (SELECT DISTINCT ced_a2_0.EMP_NAME AS COL0, ced_a2_0.DEPT_NAME AS COL1 
FROM public.ced AS ced_a2_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_ced AS __temp__Δ_del_ced_a2 
WHERE __temp__Δ_del_ced_a2.DEPT_NAME = ced_a2_0.DEPT_NAME AND __temp__Δ_del_ced_a2.EMP_NAME = ced_a2_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_ced_a2_0.EMP_NAME AS COL0, __temp__Δ_ins_ced_a2_0.DEPT_NAME AS COL1 
FROM __temp__Δ_ins_ced AS __temp__Δ_ins_ced_a2_0  ) AS new_ced_a2_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.ed AS ed_a2 
WHERE ed_a2.DEPT_NAME = new_ced_a2_0.COL1 AND ed_a2.EMP_NAME = new_ced_a2_0.COL0 ) ) AS Δ_ins_ed_a2_0  ) AS Δ_ins_ed_extra_alia 
            EXCEPT ALL 
            SELECT * FROM  public.ed;

CREATE TEMPORARY TABLE Δ_ins_eed WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1) :: public.eed).* 
            FROM (SELECT DISTINCT Δ_ins_eed_a2_0.COL0 AS COL0, Δ_ins_eed_a2_0.COL1 AS COL1 
FROM (SELECT DISTINCT ed_a2_0.EMP_NAME AS COL0, ed_a2_0.DEPT_NAME AS COL1 
FROM public.ed AS ed_a2_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT ced_a2_0.EMP_NAME AS COL0, ced_a2_0.DEPT_NAME AS COL1 
FROM public.ced AS ced_a2_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_ced AS __temp__Δ_del_ced_a2 
WHERE __temp__Δ_del_ced_a2.DEPT_NAME = ced_a2_0.DEPT_NAME AND __temp__Δ_del_ced_a2.EMP_NAME = ced_a2_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_ced_a2_0.EMP_NAME AS COL0, __temp__Δ_ins_ced_a2_0.DEPT_NAME AS COL1 
FROM __temp__Δ_ins_ced AS __temp__Δ_ins_ced_a2_0  ) AS new_ced_a2 
WHERE new_ced_a2.COL1 = ed_a2_0.DEPT_NAME AND new_ced_a2.COL0 = ed_a2_0.EMP_NAME ) AND NOT EXISTS ( SELECT * 
FROM public.eed AS eed_a2 
WHERE eed_a2.DEPT_NAME = ed_a2_0.DEPT_NAME AND eed_a2.EMP_NAME = ed_a2_0.EMP_NAME ) ) AS Δ_ins_eed_a2_0  ) AS Δ_ins_eed_extra_alia 
            EXCEPT ALL 
            SELECT * FROM  public.eed; 

FOR temprecΔ_del_eed IN ( SELECT * FROM Δ_del_eed) LOOP 
            DELETE FROM public.eed WHERE ROW(EMP_NAME,DEPT_NAME) =  temprecΔ_del_eed;
            END LOOP;
DROP TABLE Δ_del_eed;

INSERT INTO public.ed (SELECT * FROM  Δ_ins_ed) ; 
DROP TABLE Δ_ins_ed;

INSERT INTO public.eed (SELECT * FROM  Δ_ins_eed) ; 
DROP TABLE Δ_ins_eed;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.ced';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.ced ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

CREATE OR REPLACE FUNCTION public.ced_materialization()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '__temp__Δ_ins_ced' OR table_name = '__temp__Δ_del_ced')
    THEN
        -- RAISE LOG 'execute procedure ced_materialization';
        CREATE TEMPORARY TABLE __temp__Δ_ins_ced ( LIKE public.ced INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__ced_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_ins_ced DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.ced_delta_action();

        CREATE TEMPORARY TABLE __temp__Δ_del_ced ( LIKE public.ced INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__ced_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_del_ced DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.ced_delta_action();
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.ced';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.ced ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS ced_trigger_materialization ON public.ced;
CREATE TRIGGER ced_trigger_materialization
    BEFORE INSERT OR UPDATE OR DELETE ON
      public.ced FOR EACH STATEMENT EXECUTE PROCEDURE public.ced_materialization();

CREATE OR REPLACE FUNCTION public.ced_update()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    -- RAISE LOG 'execute procedure ced_update';
    IF TG_OP = 'INSERT' THEN
      -- RAISE LOG 'NEW: %', NEW;
      IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
        RAISE check_violation USING MESSAGE = 'Invalid update on view: view does not accept null value';
      END IF;
      DELETE FROM __temp__Δ_del_ced WHERE ROW(EMP_NAME,DEPT_NAME) = NEW;
      INSERT INTO __temp__Δ_ins_ced SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
        RAISE check_violation USING MESSAGE = 'Invalid update on view: view does not accept null value';
      END IF;
      DELETE FROM __temp__Δ_ins_ced WHERE ROW(EMP_NAME,DEPT_NAME) = OLD;
      INSERT INTO __temp__Δ_del_ced SELECT (OLD).*;
      DELETE FROM __temp__Δ_del_ced WHERE ROW(EMP_NAME,DEPT_NAME) = NEW;
      INSERT INTO __temp__Δ_ins_ced SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      -- RAISE LOG 'OLD: %', OLD;
      DELETE FROM __temp__Δ_ins_ced WHERE ROW(EMP_NAME,DEPT_NAME) = OLD;
      INSERT INTO __temp__Δ_del_ced SELECT (OLD).*;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.ced';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.ced ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS ced_trigger_update ON public.ced;
CREATE TRIGGER ced_trigger_update
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.ced FOR EACH ROW EXECUTE PROCEDURE public.ced_update();

