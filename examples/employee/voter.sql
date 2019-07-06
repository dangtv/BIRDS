CREATE OR REPLACE VIEW public.voter AS 
SELECT __dummy__.COL0 AS EMP_NAME,__dummy__.COL1 AS BIRTH_DATE 
FROM (SELECT DISTINCT voter_a2_0.COL0 AS COL0, voter_a2_0.COL1 AS COL1 
FROM (SELECT DISTINCT p_0_a2_0.COL0 AS COL0, p_0_a2_0.COL1 AS COL1 
FROM (SELECT DISTINCT residents_a3_0.EMP_NAME AS COL0, residents_a3_0.BIRTH_DATE AS COL1 
FROM public.residents AS residents_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.blacklist AS blacklist_a1 
WHERE blacklist_a1.EMP_NAME = residents_a3_0.EMP_NAME ) ) AS p_0_a2_0  ) AS voter_a2_0  ) AS __dummy__;


CREATE OR REPLACE FUNCTION public.voter_delta_action()
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
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'voter_delta_action_flag') THEN
        -- RAISE LOG 'execute procedure voter_delta_action';
        CREATE TEMPORARY TABLE voter_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        IF EXISTS (SELECT  
FROM (SELECT  
FROM (SELECT  
FROM (SELECT DISTINCT voter_a2_0.EMP_NAME AS COL0, voter_a2_0.BIRTH_DATE AS COL1 
FROM public.voter AS voter_a2_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_voter AS __temp__Δ_del_voter_a2 
WHERE __temp__Δ_del_voter_a2.BIRTH_DATE = voter_a2_0.BIRTH_DATE AND __temp__Δ_del_voter_a2.EMP_NAME = voter_a2_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_voter_a2_0.EMP_NAME AS COL0, __temp__Δ_ins_voter_a2_0.BIRTH_DATE AS COL1 
FROM __temp__Δ_ins_voter AS __temp__Δ_ins_voter_a2_0  ) AS new_voter_a2_0, public.blacklist AS blacklist_a1_1 
WHERE blacklist_a1_1.EMP_NAME = new_voter_a2_0.COL0 ) AS ⊥_a0_0  ) AS __dummy__ )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid view update: constraints on the view are violated';
        END IF;
        IF EXISTS (SELECT WHERE false )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid view update: constraints on the source relations are violated';
        END IF;
        CREATE TEMPORARY TABLE Δ_del_residents WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2) :: public.residents).* 
            FROM (SELECT DISTINCT Δ_del_residents_a3_0.COL0 AS COL0, Δ_del_residents_a3_0.COL1 AS COL1, Δ_del_residents_a3_0.COL2 AS COL2 
FROM (SELECT DISTINCT residents_a3_0.EMP_NAME AS COL0, residents_a3_0.BIRTH_DATE AS COL1, residents_a3_0.GENDER AS COL2 
FROM public.residents AS residents_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.blacklist AS blacklist_a1 
WHERE blacklist_a1.EMP_NAME = residents_a3_0.EMP_NAME ) AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT voter_a2_0.EMP_NAME AS COL0, voter_a2_0.BIRTH_DATE AS COL1 
FROM public.voter AS voter_a2_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_voter AS __temp__Δ_del_voter_a2 
WHERE __temp__Δ_del_voter_a2.BIRTH_DATE = voter_a2_0.BIRTH_DATE AND __temp__Δ_del_voter_a2.EMP_NAME = voter_a2_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_voter_a2_0.EMP_NAME AS COL0, __temp__Δ_ins_voter_a2_0.BIRTH_DATE AS COL1 
FROM __temp__Δ_ins_voter AS __temp__Δ_ins_voter_a2_0  ) AS new_voter_a2 
WHERE new_voter_a2.COL1 = residents_a3_0.BIRTH_DATE AND new_voter_a2.COL0 = residents_a3_0.EMP_NAME ) ) AS Δ_del_residents_a3_0  ) AS Δ_del_residents_extra_alias;

CREATE TEMPORARY TABLE Δ_ins_residents WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2) :: public.residents).* 
            FROM (SELECT DISTINCT Δ_ins_residents_a3_0.COL0 AS COL0, Δ_ins_residents_a3_0.COL1 AS COL1, Δ_ins_residents_a3_0.COL2 AS COL2 
FROM (SELECT DISTINCT new_voter_a2_0.COL0 AS COL0, new_voter_a2_0.COL1 AS COL1, 'unknown' AS COL2 
FROM (SELECT DISTINCT voter_a2_0.EMP_NAME AS COL0, voter_a2_0.BIRTH_DATE AS COL1 
FROM public.voter AS voter_a2_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_voter AS __temp__Δ_del_voter_a2 
WHERE __temp__Δ_del_voter_a2.BIRTH_DATE = voter_a2_0.BIRTH_DATE AND __temp__Δ_del_voter_a2.EMP_NAME = voter_a2_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_voter_a2_0.EMP_NAME AS COL0, __temp__Δ_ins_voter_a2_0.BIRTH_DATE AS COL1 
FROM __temp__Δ_ins_voter AS __temp__Δ_ins_voter_a2_0  ) AS new_voter_a2_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.residents AS residents_a3 
WHERE residents_a3.BIRTH_DATE = new_voter_a2_0.COL1 AND residents_a3.EMP_NAME = new_voter_a2_0.COL0 ) ) AS Δ_ins_residents_a3_0  ) AS Δ_ins_residents_extra_alia 
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
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.voter';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.voter ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

CREATE OR REPLACE FUNCTION public.voter_materialization()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '__temp__Δ_ins_voter' OR table_name = '__temp__Δ_del_voter')
    THEN
        -- RAISE LOG 'execute procedure voter_materialization';
        CREATE TEMPORARY TABLE __temp__Δ_ins_voter ( LIKE public.voter INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__voter_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_ins_voter DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.voter_delta_action();

        CREATE TEMPORARY TABLE __temp__Δ_del_voter ( LIKE public.voter INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__voter_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_del_voter DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.voter_delta_action();
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.voter';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.voter ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS voter_trigger_materialization ON public.voter;
CREATE TRIGGER voter_trigger_materialization
    BEFORE INSERT OR UPDATE OR DELETE ON
      public.voter FOR EACH STATEMENT EXECUTE PROCEDURE public.voter_materialization();

CREATE OR REPLACE FUNCTION public.voter_update()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    -- RAISE LOG 'execute procedure voter_update';
    IF TG_OP = 'INSERT' THEN
      -- RAISE LOG 'NEW: %', NEW;
      IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
        RAISE check_violation USING MESSAGE = 'Invalid update on view: view does not accept null value';
      END IF;
      DELETE FROM __temp__Δ_del_voter WHERE ROW(EMP_NAME,BIRTH_DATE) = NEW;
      INSERT INTO __temp__Δ_ins_voter SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
        RAISE check_violation USING MESSAGE = 'Invalid update on view: view does not accept null value';
      END IF;
      DELETE FROM __temp__Δ_ins_voter WHERE ROW(EMP_NAME,BIRTH_DATE) = OLD;
      INSERT INTO __temp__Δ_del_voter SELECT (OLD).*;
      DELETE FROM __temp__Δ_del_voter WHERE ROW(EMP_NAME,BIRTH_DATE) = NEW;
      INSERT INTO __temp__Δ_ins_voter SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      -- RAISE LOG 'OLD: %', OLD;
      DELETE FROM __temp__Δ_ins_voter WHERE ROW(EMP_NAME,BIRTH_DATE) = OLD;
      INSERT INTO __temp__Δ_del_voter SELECT (OLD).*;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.voter';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.voter ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS voter_trigger_update ON public.voter;
CREATE TRIGGER voter_trigger_update
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.voter FOR EACH ROW EXECUTE PROCEDURE public.voter_update();

