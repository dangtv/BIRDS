/*view definition (get):
retired(EMP_NAME) :- p_0(EMP_NAME).
p_0(EMP_NAME) :- COL1 = 'unknown' , not ced(EMP_NAME, COL1) , not ced(EMP_NAME, _) , residents(EMP_NAME, _, _).
*/
CREATE OR REPLACE VIEW public.retired AS 
SELECT __dummy__.COL0 AS EMP_NAME 
FROM (SELECT DISTINCT retired_a1_0.COL0 AS COL0 
FROM (SELECT DISTINCT p_0_a1_0.COL0 AS COL0 
FROM (SELECT DISTINCT residents_a3_0.EMP_NAME AS COL0 
FROM public.residents AS residents_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.ced AS ced_a2 
WHERE ced_a2.DEPT_NAME = 'unknown' AND ced_a2.EMP_NAME = residents_a3_0.EMP_NAME ) AND NOT EXISTS ( SELECT * 
FROM public.ced AS ced_a2 
WHERE ced_a2.EMP_NAME = residents_a3_0.EMP_NAME ) ) AS p_0_a1_0  ) AS retired_a1_0  ) AS __dummy__;


CREATE OR REPLACE FUNCTION public.retired_delta_action()
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
  temprecΔ_del_ced public.ced%ROWTYPE;
temprecΔ_ins_ced public.ced%ROWTYPE;
temprecΔ_ins_residents public.residents%ROWTYPE;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'retired_delta_action_flag') THEN
        -- RAISE LOG 'execute procedure retired_delta_action';
        CREATE TEMPORARY TABLE retired_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        IF EXISTS (SELECT WHERE false )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid update on view';
        END IF;
        CREATE TEMPORARY TABLE Δ_del_ced WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1) :: public.ced).* 
            FROM (SELECT DISTINCT Δ_del_ced_a2_0.COL0 AS COL0, Δ_del_ced_a2_0.COL1 AS COL1 
FROM (SELECT DISTINCT new_retired_a1_1.COL0 AS COL0, ced_a2_0.DEPT_NAME AS COL1 
FROM public.ced AS ced_a2_0, (SELECT DISTINCT retired_a1_0.EMP_NAME AS COL0 
FROM public.retired AS retired_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_retired AS __temp__Δ_del_retired_a1 
WHERE __temp__Δ_del_retired_a1.EMP_NAME = retired_a1_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_retired_a1_0.EMP_NAME AS COL0 
FROM __temp__Δ_ins_retired AS __temp__Δ_ins_retired_a1_0  ) AS new_retired_a1_1 
WHERE new_retired_a1_1.COL0 = ced_a2_0.EMP_NAME ) AS Δ_del_ced_a2_0  ) AS Δ_del_ced_extra_alias;

CREATE TEMPORARY TABLE Δ_ins_ced WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1) :: public.ced).* 
            FROM (SELECT DISTINCT Δ_ins_ced_a2_0.COL0 AS COL0, Δ_ins_ced_a2_0.COL1 AS COL1 
FROM (SELECT DISTINCT residents_a3_0.EMP_NAME AS COL0, 'unknown' AS COL1 
FROM public.residents AS residents_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT retired_a1_0.EMP_NAME AS COL0 
FROM public.retired AS retired_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_retired AS __temp__Δ_del_retired_a1 
WHERE __temp__Δ_del_retired_a1.EMP_NAME = retired_a1_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_retired_a1_0.EMP_NAME AS COL0 
FROM __temp__Δ_ins_retired AS __temp__Δ_ins_retired_a1_0  ) AS new_retired_a1 
WHERE new_retired_a1.COL0 = residents_a3_0.EMP_NAME ) AND NOT EXISTS ( SELECT * 
FROM public.ced AS ced_a2 
WHERE ced_a2.EMP_NAME = residents_a3_0.EMP_NAME ) ) AS Δ_ins_ced_a2_0  ) AS Δ_ins_ced_extra_alia 
            EXCEPT ALL 
            SELECT * FROM  public.ced;

CREATE TEMPORARY TABLE Δ_ins_residents WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2) :: public.residents).* 
            FROM (SELECT DISTINCT Δ_ins_residents_a3_0.COL0 AS COL0, Δ_ins_residents_a3_0.COL1 AS COL1, Δ_ins_residents_a3_0.COL2 AS COL2 
FROM (SELECT DISTINCT new_retired_a1_0.COL0 AS COL0, '0001-01-01' AS COL1, 'unknown' AS COL2 
FROM (SELECT DISTINCT retired_a1_0.EMP_NAME AS COL0 
FROM public.retired AS retired_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_retired AS __temp__Δ_del_retired_a1 
WHERE __temp__Δ_del_retired_a1.EMP_NAME = retired_a1_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_retired_a1_0.EMP_NAME AS COL0 
FROM __temp__Δ_ins_retired AS __temp__Δ_ins_retired_a1_0  ) AS new_retired_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.residents AS residents_a3 
WHERE residents_a3.EMP_NAME = new_retired_a1_0.COL0 ) ) AS Δ_ins_residents_a3_0  ) AS Δ_ins_residents_extra_alia 
            EXCEPT ALL 
            SELECT * FROM  public.residents; 

FOR temprecΔ_del_ced IN ( SELECT * FROM Δ_del_ced) LOOP 
            DELETE FROM public.ced WHERE ROW(EMP_NAME,DEPT_NAME) =  temprecΔ_del_ced;
            END LOOP;
DROP TABLE Δ_del_ced;

INSERT INTO public.ced (SELECT * FROM  Δ_ins_ced) ; 
DROP TABLE Δ_ins_ced;

INSERT INTO public.residents (SELECT * FROM  Δ_ins_residents) ; 
DROP TABLE Δ_ins_residents;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.retired';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.retired ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

CREATE OR REPLACE FUNCTION public.retired_materialization()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '__temp__Δ_ins_retired' OR table_name = '__temp__Δ_del_retired')
    THEN
        -- RAISE LOG 'execute procedure retired_materialization';
        CREATE TEMPORARY TABLE __temp__Δ_ins_retired ( LIKE public.retired INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__retired_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_ins_retired DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.retired_delta_action();

        CREATE TEMPORARY TABLE __temp__Δ_del_retired ( LIKE public.retired INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__retired_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_del_retired DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.retired_delta_action();
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.retired';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.retired ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS retired_trigger_materialization ON public.retired;
CREATE TRIGGER retired_trigger_materialization
    BEFORE INSERT OR UPDATE OR DELETE ON
      public.retired FOR EACH STATEMENT EXECUTE PROCEDURE public.retired_materialization();

CREATE OR REPLACE FUNCTION public.retired_update()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    -- RAISE LOG 'execute procedure retired_update';
    IF TG_OP = 'INSERT' THEN
      -- RAISE LOG 'NEW: %', NEW;
      IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
        RAISE check_violation USING MESSAGE = 'Invalid update on view: view does not accept null value';
      END IF;
      DELETE FROM __temp__Δ_del_retired WHERE ROW(EMP_NAME) = NEW;
      INSERT INTO __temp__Δ_ins_retired SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
        RAISE check_violation USING MESSAGE = 'Invalid update on view: view does not accept null value';
      END IF;
      DELETE FROM __temp__Δ_ins_retired WHERE ROW(EMP_NAME) = OLD;
      INSERT INTO __temp__Δ_del_retired SELECT (OLD).*;
      DELETE FROM __temp__Δ_del_retired WHERE ROW(EMP_NAME) = NEW;
      INSERT INTO __temp__Δ_ins_retired SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      -- RAISE LOG 'OLD: %', OLD;
      DELETE FROM __temp__Δ_ins_retired WHERE ROW(EMP_NAME) = OLD;
      INSERT INTO __temp__Δ_del_retired SELECT (OLD).*;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.retired';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.retired ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS retired_trigger_update ON public.retired;
CREATE TRIGGER retired_trigger_update
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.retired FOR EACH ROW EXECUTE PROCEDURE public.retired_update();

