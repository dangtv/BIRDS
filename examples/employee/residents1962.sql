/*view definition (get):
residents1962(EMP_NAME, BIRTH_DATE, GENDER) :- p_0(EMP_NAME, BIRTH_DATE, GENDER).
p_0(EMP_NAME, BIRTH_DATE, GENDER) :- BIRTH_DATE <= '1962-12-31' , BIRTH_DATE >= '1962-01-01' , residents(EMP_NAME, BIRTH_DATE, GENDER).
*/
CREATE OR REPLACE VIEW public.residents1962 AS 
SELECT __dummy__.COL0 AS EMP_NAME,__dummy__.COL1 AS BIRTH_DATE,__dummy__.COL2 AS GENDER 
FROM (SELECT DISTINCT residents1962_a3_0.COL0 AS COL0, residents1962_a3_0.COL1 AS COL1, residents1962_a3_0.COL2 AS COL2 
FROM (SELECT DISTINCT p_0_a3_0.COL0 AS COL0, p_0_a3_0.COL1 AS COL1, p_0_a3_0.COL2 AS COL2 
FROM (SELECT DISTINCT residents_a3_0.EMP_NAME AS COL0, residents_a3_0.BIRTH_DATE AS COL1, residents_a3_0.GENDER AS COL2 
FROM public.residents AS residents_a3_0 
WHERE residents_a3_0.BIRTH_DATE  <=  '1962-12-31' AND residents_a3_0.BIRTH_DATE  >=  '1962-01-01' ) AS p_0_a3_0  ) AS residents1962_a3_0  ) AS __dummy__;


CREATE OR REPLACE FUNCTION public.residents1962_delta_action()
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
  temprecΔ_del_residents public.residents%ROWTYPE;
temprecΔ_ins_residents public.residents%ROWTYPE;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'residents1962_delta_action_flag') THEN
        -- RAISE LOG 'execute procedure residents1962_delta_action';
        CREATE TEMPORARY TABLE residents1962_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        IF EXISTS (SELECT  
FROM (SELECT  
FROM (SELECT  
FROM (SELECT DISTINCT residents1962_a3_0.EMP_NAME AS COL0, residents1962_a3_0.BIRTH_DATE AS COL1, residents1962_a3_0.GENDER AS COL2 
FROM public.residents1962 AS residents1962_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_residents1962 AS __temp__Δ_del_residents1962_a3 
WHERE __temp__Δ_del_residents1962_a3.GENDER = residents1962_a3_0.GENDER AND __temp__Δ_del_residents1962_a3.BIRTH_DATE = residents1962_a3_0.BIRTH_DATE AND __temp__Δ_del_residents1962_a3.EMP_NAME = residents1962_a3_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_residents1962_a3_0.EMP_NAME AS COL0, __temp__Δ_ins_residents1962_a3_0.BIRTH_DATE AS COL1, __temp__Δ_ins_residents1962_a3_0.GENDER AS COL2 
FROM __temp__Δ_ins_residents1962 AS __temp__Δ_ins_residents1962_a3_0  ) AS new_residents1962_a3_0 
WHERE new_residents1962_a3_0.COL1  >  '1962-12-31'  UNION ALL SELECT  
FROM (SELECT DISTINCT residents1962_a3_0.EMP_NAME AS COL0, residents1962_a3_0.BIRTH_DATE AS COL1, residents1962_a3_0.GENDER AS COL2 
FROM public.residents1962 AS residents1962_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_residents1962 AS __temp__Δ_del_residents1962_a3 
WHERE __temp__Δ_del_residents1962_a3.GENDER = residents1962_a3_0.GENDER AND __temp__Δ_del_residents1962_a3.BIRTH_DATE = residents1962_a3_0.BIRTH_DATE AND __temp__Δ_del_residents1962_a3.EMP_NAME = residents1962_a3_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_residents1962_a3_0.EMP_NAME AS COL0, __temp__Δ_ins_residents1962_a3_0.BIRTH_DATE AS COL1, __temp__Δ_ins_residents1962_a3_0.GENDER AS COL2 
FROM __temp__Δ_ins_residents1962 AS __temp__Δ_ins_residents1962_a3_0  ) AS new_residents1962_a3_0 
WHERE new_residents1962_a3_0.COL1  <  '1962-01-01' ) AS ⊥_a0_0  ) AS __dummy__ )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid update on view';
        END IF;
        CREATE TEMPORARY TABLE Δ_del_residents WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2) :: public.residents).* 
            FROM (SELECT DISTINCT Δ_del_residents_a3_0.COL0 AS COL0, Δ_del_residents_a3_0.COL1 AS COL1, Δ_del_residents_a3_0.COL2 AS COL2 
FROM (SELECT DISTINCT residents_a3_0.EMP_NAME AS COL0, residents_a3_0.BIRTH_DATE AS COL1, residents_a3_0.GENDER AS COL2 
FROM public.residents AS residents_a3_0 
WHERE residents_a3_0.BIRTH_DATE  >=  '1962-01-01' AND residents_a3_0.BIRTH_DATE  <=  '1962-12-31' AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT residents1962_a3_0.EMP_NAME AS COL0, residents1962_a3_0.BIRTH_DATE AS COL1, residents1962_a3_0.GENDER AS COL2 
FROM public.residents1962 AS residents1962_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_residents1962 AS __temp__Δ_del_residents1962_a3 
WHERE __temp__Δ_del_residents1962_a3.GENDER = residents1962_a3_0.GENDER AND __temp__Δ_del_residents1962_a3.BIRTH_DATE = residents1962_a3_0.BIRTH_DATE AND __temp__Δ_del_residents1962_a3.EMP_NAME = residents1962_a3_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_residents1962_a3_0.EMP_NAME AS COL0, __temp__Δ_ins_residents1962_a3_0.BIRTH_DATE AS COL1, __temp__Δ_ins_residents1962_a3_0.GENDER AS COL2 
FROM __temp__Δ_ins_residents1962 AS __temp__Δ_ins_residents1962_a3_0  ) AS new_residents1962_a3 
WHERE new_residents1962_a3.COL2 = residents_a3_0.GENDER AND new_residents1962_a3.COL1 = residents_a3_0.BIRTH_DATE AND new_residents1962_a3.COL0 = residents_a3_0.EMP_NAME ) ) AS Δ_del_residents_a3_0  ) AS Δ_del_residents_extra_alias;

CREATE TEMPORARY TABLE Δ_ins_residents WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2) :: public.residents).* 
            FROM (SELECT DISTINCT Δ_ins_residents_a3_0.COL0 AS COL0, Δ_ins_residents_a3_0.COL1 AS COL1, Δ_ins_residents_a3_0.COL2 AS COL2 
FROM (SELECT DISTINCT new_residents1962_a3_0.COL0 AS COL0, new_residents1962_a3_0.COL1 AS COL1, new_residents1962_a3_0.COL2 AS COL2 
FROM (SELECT DISTINCT residents1962_a3_0.EMP_NAME AS COL0, residents1962_a3_0.BIRTH_DATE AS COL1, residents1962_a3_0.GENDER AS COL2 
FROM public.residents1962 AS residents1962_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_residents1962 AS __temp__Δ_del_residents1962_a3 
WHERE __temp__Δ_del_residents1962_a3.GENDER = residents1962_a3_0.GENDER AND __temp__Δ_del_residents1962_a3.BIRTH_DATE = residents1962_a3_0.BIRTH_DATE AND __temp__Δ_del_residents1962_a3.EMP_NAME = residents1962_a3_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_residents1962_a3_0.EMP_NAME AS COL0, __temp__Δ_ins_residents1962_a3_0.BIRTH_DATE AS COL1, __temp__Δ_ins_residents1962_a3_0.GENDER AS COL2 
FROM __temp__Δ_ins_residents1962 AS __temp__Δ_ins_residents1962_a3_0  ) AS new_residents1962_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.residents AS residents_a3 
WHERE residents_a3.GENDER = new_residents1962_a3_0.COL2 AND residents_a3.BIRTH_DATE = new_residents1962_a3_0.COL1 AND residents_a3.EMP_NAME = new_residents1962_a3_0.COL0 ) ) AS Δ_ins_residents_a3_0  ) AS Δ_ins_residents_extra_alia 
            EXCEPT ALL 
            SELECT * FROM  public.residents; 

FOR temprecΔ_del_residents IN ( SELECT * FROM Δ_del_residents) LOOP 
            DELETE FROM public.residents WHERE ROW(EMP_NAME,BIRTH_DATE,GENDER) =  temprecΔ_del_residents;
            END LOOP;
DROP TABLE Δ_del_residents;

INSERT INTO public.residents (SELECT * FROM  Δ_ins_residents) ; 
DROP TABLE Δ_ins_residents;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.residents1962';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.residents1962 ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

CREATE OR REPLACE FUNCTION public.residents1962_materialization()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '__temp__Δ_ins_residents1962' OR table_name = '__temp__Δ_del_residents1962')
    THEN
        -- RAISE LOG 'execute procedure residents1962_materialization';
        CREATE TEMPORARY TABLE __temp__Δ_ins_residents1962 ( LIKE public.residents1962 INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__residents1962_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_ins_residents1962 DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.residents1962_delta_action();

        CREATE TEMPORARY TABLE __temp__Δ_del_residents1962 ( LIKE public.residents1962 INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__residents1962_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_del_residents1962 DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.residents1962_delta_action();
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.residents1962';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.residents1962 ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS residents1962_trigger_materialization ON public.residents1962;
CREATE TRIGGER residents1962_trigger_materialization
    BEFORE INSERT OR UPDATE OR DELETE ON
      public.residents1962 FOR EACH STATEMENT EXECUTE PROCEDURE public.residents1962_materialization();

CREATE OR REPLACE FUNCTION public.residents1962_update()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    -- RAISE LOG 'execute procedure residents1962_update';
    IF TG_OP = 'INSERT' THEN
      -- RAISE LOG 'NEW: %', NEW;
      IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
        RAISE check_violation USING MESSAGE = 'Invalid update on view: view does not accept null value';
      END IF;
      DELETE FROM __temp__Δ_del_residents1962 WHERE ROW(EMP_NAME,BIRTH_DATE,GENDER) = NEW;
      INSERT INTO __temp__Δ_ins_residents1962 SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
        RAISE check_violation USING MESSAGE = 'Invalid update on view: view does not accept null value';
      END IF;
      DELETE FROM __temp__Δ_ins_residents1962 WHERE ROW(EMP_NAME,BIRTH_DATE,GENDER) = OLD;
      INSERT INTO __temp__Δ_del_residents1962 SELECT (OLD).*;
      DELETE FROM __temp__Δ_del_residents1962 WHERE ROW(EMP_NAME,BIRTH_DATE,GENDER) = NEW;
      INSERT INTO __temp__Δ_ins_residents1962 SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      -- RAISE LOG 'OLD: %', OLD;
      DELETE FROM __temp__Δ_ins_residents1962 WHERE ROW(EMP_NAME,BIRTH_DATE,GENDER) = OLD;
      INSERT INTO __temp__Δ_del_residents1962 SELECT (OLD).*;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.residents1962';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.residents1962 ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS residents1962_trigger_update ON public.residents1962;
CREATE TRIGGER residents1962_trigger_update
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.residents1962 FOR EACH ROW EXECUTE PROCEDURE public.residents1962_update();

