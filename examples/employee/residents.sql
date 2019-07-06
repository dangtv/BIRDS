/*view definition (get):
residents(EMP_NAME, BIRTH_DATE, GENDER) :- p_0(EMP_NAME, BIRTH_DATE, GENDER).
p_0(EMP_NAME, BIRTH_DATE, GENDER) :- others(EMP_NAME, BIRTH_DATE, GENDER).
p_0(EMP_NAME, BIRTH_DATE, GENDER) :- GENDER = 'F' , female(EMP_NAME, BIRTH_DATE).
p_0(EMP_NAME, BIRTH_DATE, GENDER) :- GENDER = 'M' , male(EMP_NAME, BIRTH_DATE).
*/
CREATE OR REPLACE VIEW public.residents AS 
SELECT __dummy__.COL0 AS EMP_NAME,__dummy__.COL1 AS BIRTH_DATE,__dummy__.COL2 AS GENDER 
FROM (SELECT DISTINCT residents_a3_0.COL0 AS COL0, residents_a3_0.COL1 AS COL1, residents_a3_0.COL2 AS COL2 
FROM (SELECT DISTINCT p_0_a3_0.COL0 AS COL0, p_0_a3_0.COL1 AS COL1, p_0_a3_0.COL2 AS COL2 
FROM (SELECT DISTINCT others_a3_0.EMP_NAME AS COL0, others_a3_0.BIRTH_DATE AS COL1, others_a3_0.GENDER AS COL2 
FROM public.others AS others_a3_0   UNION SELECT DISTINCT female_a2_0.EMP_NAME AS COL0, female_a2_0.BIRTH_DATE AS COL1, 'F' AS COL2 
FROM public.female AS female_a2_0   UNION SELECT DISTINCT male_a2_0.EMP_NAME AS COL0, male_a2_0.BIRTH_DATE AS COL1, 'M' AS COL2 
FROM public.male AS male_a2_0  ) AS p_0_a3_0  ) AS residents_a3_0  ) AS __dummy__;


CREATE OR REPLACE FUNCTION public.residents_delta_action()
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
  temprecΔ_del_female public.female%ROWTYPE;
temprecΔ_del_male public.male%ROWTYPE;
temprecΔ_del_others public.others%ROWTYPE;
temprecΔ_ins_female public.female%ROWTYPE;
temprecΔ_ins_male public.male%ROWTYPE;
temprecΔ_ins_others public.others%ROWTYPE;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'residents_delta_action_flag') THEN
        -- RAISE LOG 'execute procedure residents_delta_action';
        CREATE TEMPORARY TABLE residents_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        IF EXISTS (SELECT WHERE false )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid update on view';
        END IF;
        CREATE TEMPORARY TABLE Δ_del_female WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1) :: public.female).* 
            FROM (SELECT DISTINCT Δ_del_female_a2_0.COL0 AS COL0, Δ_del_female_a2_0.COL1 AS COL1 
FROM (SELECT DISTINCT female_a2_0.EMP_NAME AS COL0, female_a2_0.BIRTH_DATE AS COL1 
FROM public.female AS female_a2_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT residents_a3_0.EMP_NAME AS COL0, residents_a3_0.BIRTH_DATE AS COL1, residents_a3_0.GENDER AS COL2 
FROM public.residents AS residents_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_residents AS __temp__Δ_del_residents_a3 
WHERE __temp__Δ_del_residents_a3.GENDER = residents_a3_0.GENDER AND __temp__Δ_del_residents_a3.BIRTH_DATE = residents_a3_0.BIRTH_DATE AND __temp__Δ_del_residents_a3.EMP_NAME = residents_a3_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_residents_a3_0.EMP_NAME AS COL0, __temp__Δ_ins_residents_a3_0.BIRTH_DATE AS COL1, __temp__Δ_ins_residents_a3_0.GENDER AS COL2 
FROM __temp__Δ_ins_residents AS __temp__Δ_ins_residents_a3_0  ) AS new_residents_a3 
WHERE new_residents_a3.COL2 = 'F' AND new_residents_a3.COL1 = female_a2_0.BIRTH_DATE AND new_residents_a3.COL0 = female_a2_0.EMP_NAME ) ) AS Δ_del_female_a2_0  ) AS Δ_del_female_extra_alias;

CREATE TEMPORARY TABLE Δ_del_male WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1) :: public.male).* 
            FROM (SELECT DISTINCT Δ_del_male_a2_0.COL0 AS COL0, Δ_del_male_a2_0.COL1 AS COL1 
FROM (SELECT DISTINCT male_a2_0.EMP_NAME AS COL0, male_a2_0.BIRTH_DATE AS COL1 
FROM public.male AS male_a2_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT residents_a3_0.EMP_NAME AS COL0, residents_a3_0.BIRTH_DATE AS COL1, residents_a3_0.GENDER AS COL2 
FROM public.residents AS residents_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_residents AS __temp__Δ_del_residents_a3 
WHERE __temp__Δ_del_residents_a3.GENDER = residents_a3_0.GENDER AND __temp__Δ_del_residents_a3.BIRTH_DATE = residents_a3_0.BIRTH_DATE AND __temp__Δ_del_residents_a3.EMP_NAME = residents_a3_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_residents_a3_0.EMP_NAME AS COL0, __temp__Δ_ins_residents_a3_0.BIRTH_DATE AS COL1, __temp__Δ_ins_residents_a3_0.GENDER AS COL2 
FROM __temp__Δ_ins_residents AS __temp__Δ_ins_residents_a3_0  ) AS new_residents_a3 
WHERE new_residents_a3.COL2 = 'M' AND new_residents_a3.COL1 = male_a2_0.BIRTH_DATE AND new_residents_a3.COL0 = male_a2_0.EMP_NAME ) ) AS Δ_del_male_a2_0  ) AS Δ_del_male_extra_alias;

CREATE TEMPORARY TABLE Δ_del_others WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2) :: public.others).* 
            FROM (SELECT DISTINCT Δ_del_others_a3_0.COL0 AS COL0, Δ_del_others_a3_0.COL1 AS COL1, Δ_del_others_a3_0.COL2 AS COL2 
FROM (SELECT DISTINCT others_a3_0.EMP_NAME AS COL0, others_a3_0.BIRTH_DATE AS COL1, others_a3_0.GENDER AS COL2 
FROM public.others AS others_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT residents_a3_0.EMP_NAME AS COL0, residents_a3_0.BIRTH_DATE AS COL1, residents_a3_0.GENDER AS COL2 
FROM public.residents AS residents_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_residents AS __temp__Δ_del_residents_a3 
WHERE __temp__Δ_del_residents_a3.GENDER = residents_a3_0.GENDER AND __temp__Δ_del_residents_a3.BIRTH_DATE = residents_a3_0.BIRTH_DATE AND __temp__Δ_del_residents_a3.EMP_NAME = residents_a3_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_residents_a3_0.EMP_NAME AS COL0, __temp__Δ_ins_residents_a3_0.BIRTH_DATE AS COL1, __temp__Δ_ins_residents_a3_0.GENDER AS COL2 
FROM __temp__Δ_ins_residents AS __temp__Δ_ins_residents_a3_0  ) AS new_residents_a3 
WHERE new_residents_a3.COL2 = others_a3_0.GENDER AND new_residents_a3.COL1 = others_a3_0.BIRTH_DATE AND new_residents_a3.COL0 = others_a3_0.EMP_NAME ) ) AS Δ_del_others_a3_0  ) AS Δ_del_others_extra_alias;

CREATE TEMPORARY TABLE Δ_ins_female WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1) :: public.female).* 
            FROM (SELECT DISTINCT Δ_ins_female_a2_0.COL0 AS COL0, Δ_ins_female_a2_0.COL1 AS COL1 
FROM (SELECT DISTINCT new_residents_a3_0.COL0 AS COL0, new_residents_a3_0.COL1 AS COL1 
FROM (SELECT DISTINCT residents_a3_0.EMP_NAME AS COL0, residents_a3_0.BIRTH_DATE AS COL1, residents_a3_0.GENDER AS COL2 
FROM public.residents AS residents_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_residents AS __temp__Δ_del_residents_a3 
WHERE __temp__Δ_del_residents_a3.GENDER = residents_a3_0.GENDER AND __temp__Δ_del_residents_a3.BIRTH_DATE = residents_a3_0.BIRTH_DATE AND __temp__Δ_del_residents_a3.EMP_NAME = residents_a3_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_residents_a3_0.EMP_NAME AS COL0, __temp__Δ_ins_residents_a3_0.BIRTH_DATE AS COL1, __temp__Δ_ins_residents_a3_0.GENDER AS COL2 
FROM __temp__Δ_ins_residents AS __temp__Δ_ins_residents_a3_0  ) AS new_residents_a3_0 
WHERE new_residents_a3_0.COL2 = 'F' AND NOT EXISTS ( SELECT * 
FROM public.female AS female_a2 
WHERE female_a2.BIRTH_DATE = new_residents_a3_0.COL1 AND female_a2.EMP_NAME = new_residents_a3_0.COL0 ) AND NOT EXISTS ( SELECT * 
FROM public.others AS others_a3 
WHERE others_a3.GENDER = new_residents_a3_0.COL2 AND others_a3.BIRTH_DATE = new_residents_a3_0.COL1 AND others_a3.EMP_NAME = new_residents_a3_0.COL0 ) ) AS Δ_ins_female_a2_0  ) AS Δ_ins_female_extra_alia 
            EXCEPT ALL 
            SELECT * FROM  public.female;

CREATE TEMPORARY TABLE Δ_ins_male WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1) :: public.male).* 
            FROM (SELECT DISTINCT Δ_ins_male_a2_0.COL0 AS COL0, Δ_ins_male_a2_0.COL1 AS COL1 
FROM (SELECT DISTINCT new_residents_a3_0.COL0 AS COL0, new_residents_a3_0.COL1 AS COL1 
FROM (SELECT DISTINCT residents_a3_0.EMP_NAME AS COL0, residents_a3_0.BIRTH_DATE AS COL1, residents_a3_0.GENDER AS COL2 
FROM public.residents AS residents_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_residents AS __temp__Δ_del_residents_a3 
WHERE __temp__Δ_del_residents_a3.GENDER = residents_a3_0.GENDER AND __temp__Δ_del_residents_a3.BIRTH_DATE = residents_a3_0.BIRTH_DATE AND __temp__Δ_del_residents_a3.EMP_NAME = residents_a3_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_residents_a3_0.EMP_NAME AS COL0, __temp__Δ_ins_residents_a3_0.BIRTH_DATE AS COL1, __temp__Δ_ins_residents_a3_0.GENDER AS COL2 
FROM __temp__Δ_ins_residents AS __temp__Δ_ins_residents_a3_0  ) AS new_residents_a3_0 
WHERE new_residents_a3_0.COL2 = 'M' AND NOT EXISTS ( SELECT * 
FROM public.male AS male_a2 
WHERE male_a2.BIRTH_DATE = new_residents_a3_0.COL1 AND male_a2.EMP_NAME = new_residents_a3_0.COL0 ) AND NOT EXISTS ( SELECT * 
FROM public.others AS others_a3 
WHERE others_a3.GENDER = new_residents_a3_0.COL2 AND others_a3.BIRTH_DATE = new_residents_a3_0.COL1 AND others_a3.EMP_NAME = new_residents_a3_0.COL0 ) ) AS Δ_ins_male_a2_0  ) AS Δ_ins_male_extra_alia 
            EXCEPT ALL 
            SELECT * FROM  public.male;

CREATE TEMPORARY TABLE Δ_ins_others WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2) :: public.others).* 
            FROM (SELECT DISTINCT Δ_ins_others_a3_0.COL0 AS COL0, Δ_ins_others_a3_0.COL1 AS COL1, Δ_ins_others_a3_0.COL2 AS COL2 
FROM (SELECT DISTINCT new_residents_a3_0.COL0 AS COL0, new_residents_a3_0.COL1 AS COL1, new_residents_a3_0.COL2 AS COL2 
FROM (SELECT DISTINCT residents_a3_0.EMP_NAME AS COL0, residents_a3_0.BIRTH_DATE AS COL1, residents_a3_0.GENDER AS COL2 
FROM public.residents AS residents_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_residents AS __temp__Δ_del_residents_a3 
WHERE __temp__Δ_del_residents_a3.GENDER = residents_a3_0.GENDER AND __temp__Δ_del_residents_a3.BIRTH_DATE = residents_a3_0.BIRTH_DATE AND __temp__Δ_del_residents_a3.EMP_NAME = residents_a3_0.EMP_NAME )  UNION SELECT DISTINCT __temp__Δ_ins_residents_a3_0.EMP_NAME AS COL0, __temp__Δ_ins_residents_a3_0.BIRTH_DATE AS COL1, __temp__Δ_ins_residents_a3_0.GENDER AS COL2 
FROM __temp__Δ_ins_residents AS __temp__Δ_ins_residents_a3_0  ) AS new_residents_a3_0 
WHERE new_residents_a3_0.COL2  <>  'M' AND new_residents_a3_0.COL2  <>  'F' AND NOT EXISTS ( SELECT * 
FROM public.others AS others_a3 
WHERE others_a3.GENDER = new_residents_a3_0.COL2 AND others_a3.BIRTH_DATE = new_residents_a3_0.COL1 AND others_a3.EMP_NAME = new_residents_a3_0.COL0 ) ) AS Δ_ins_others_a3_0  ) AS Δ_ins_others_extra_alia 
            EXCEPT ALL 
            SELECT * FROM  public.others; 

FOR temprecΔ_del_female IN ( SELECT * FROM Δ_del_female) LOOP 
            DELETE FROM public.female WHERE ROW(EMP_NAME,BIRTH_DATE) =  temprecΔ_del_female;
            END LOOP;
DROP TABLE Δ_del_female;

FOR temprecΔ_del_male IN ( SELECT * FROM Δ_del_male) LOOP 
            DELETE FROM public.male WHERE ROW(EMP_NAME,BIRTH_DATE) =  temprecΔ_del_male;
            END LOOP;
DROP TABLE Δ_del_male;

FOR temprecΔ_del_others IN ( SELECT * FROM Δ_del_others) LOOP 
            DELETE FROM public.others WHERE ROW(EMP_NAME,BIRTH_DATE,GENDER) =  temprecΔ_del_others;
            END LOOP;
DROP TABLE Δ_del_others;

INSERT INTO public.female (SELECT * FROM  Δ_ins_female) ; 
DROP TABLE Δ_ins_female;

INSERT INTO public.male (SELECT * FROM  Δ_ins_male) ; 
DROP TABLE Δ_ins_male;

INSERT INTO public.others (SELECT * FROM  Δ_ins_others) ; 
DROP TABLE Δ_ins_others;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.residents';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.residents ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

CREATE OR REPLACE FUNCTION public.residents_materialization()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '__temp__Δ_ins_residents' OR table_name = '__temp__Δ_del_residents')
    THEN
        -- RAISE LOG 'execute procedure residents_materialization';
        CREATE TEMPORARY TABLE __temp__Δ_ins_residents ( LIKE public.residents INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__residents_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_ins_residents DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.residents_delta_action();

        CREATE TEMPORARY TABLE __temp__Δ_del_residents ( LIKE public.residents INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__residents_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_del_residents DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.residents_delta_action();
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.residents';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.residents ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS residents_trigger_materialization ON public.residents;
CREATE TRIGGER residents_trigger_materialization
    BEFORE INSERT OR UPDATE OR DELETE ON
      public.residents FOR EACH STATEMENT EXECUTE PROCEDURE public.residents_materialization();

CREATE OR REPLACE FUNCTION public.residents_update()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    -- RAISE LOG 'execute procedure residents_update';
    IF TG_OP = 'INSERT' THEN
      -- RAISE LOG 'NEW: %', NEW;
      IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
        RAISE check_violation USING MESSAGE = 'Invalid update on view: view does not accept null value';
      END IF;
      DELETE FROM __temp__Δ_del_residents WHERE ROW(EMP_NAME,BIRTH_DATE,GENDER) = NEW;
      INSERT INTO __temp__Δ_ins_residents SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
        RAISE check_violation USING MESSAGE = 'Invalid update on view: view does not accept null value';
      END IF;
      DELETE FROM __temp__Δ_ins_residents WHERE ROW(EMP_NAME,BIRTH_DATE,GENDER) = OLD;
      INSERT INTO __temp__Δ_del_residents SELECT (OLD).*;
      DELETE FROM __temp__Δ_del_residents WHERE ROW(EMP_NAME,BIRTH_DATE,GENDER) = NEW;
      INSERT INTO __temp__Δ_ins_residents SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      -- RAISE LOG 'OLD: %', OLD;
      DELETE FROM __temp__Δ_ins_residents WHERE ROW(EMP_NAME,BIRTH_DATE,GENDER) = OLD;
      INSERT INTO __temp__Δ_del_residents SELECT (OLD).*;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.residents';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.residents ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS residents_trigger_update ON public.residents;
CREATE TRIGGER residents_trigger_update
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.residents FOR EACH ROW EXECUTE PROCEDURE public.residents_update();

