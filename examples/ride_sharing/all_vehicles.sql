/*_____get datalog program_______
?- all_vehicles(COMPANY_ID,VEHICLE_ID,AREA,REQUEST_ID).

all_vehicles(ALL_VEHICLES_A4_COMPANY_ID,ALL_VEHICLES_A4_VEHICLE_ID,ALL_VEHICLES_A4_AREA,ALL_VEHICLES_A4_REQUEST_ID) :- all_vehicles_med(ALL_VEHICLES_A4_COMPANY_ID,ALL_VEHICLES_A4_VEHICLE_ID,ALL_VEHICLES_A4_AREA,ALL_VEHICLES_A4_REQUEST_ID) , not __dummy__delta__insert__peer1_public(ALL_VEHICLES_A4_VEHICLE_ID,ALL_VEHICLES_A4_AREA,ALL_VEHICLES_A4_REQUEST_ID) , not __dummy__delta__insert__peer2_public(ALL_VEHICLES_A4_VEHICLE_ID,ALL_VEHICLES_A4_AREA,ALL_VEHICLES_A4_REQUEST_ID).

__dummy__delta__insert__peer1_public(V,A,R) :- all_vehicles_med(C,V,A,R) , C = 1 , not peer1_public(V,A,R).

all_vehicles_med(C,V,A,R) :- peer1_public(V,A,R) , C = 1.

__dummy__delta__insert__peer2_public(V,A,R) :- all_vehicles_med(C,V,A,R) , C = 2 , not peer2_public(V,A,R).

all_vehicles_med(C,V,A,R) :- peer2_public(V,A,R) , C = 2.

______________*/

CREATE OR REPLACE VIEW public.all_vehicles AS 
SELECT __dummy__.col0 AS COMPANY_ID,__dummy__.col1 AS VEHICLE_ID,__dummy__.col2 AS AREA,__dummy__.col3 AS REQUEST_ID 
FROM (SELECT DISTINCT all_vehicles_a4_0.col0 AS col0, all_vehicles_a4_0.col1 AS col1, all_vehicles_a4_0.col2 AS col2, all_vehicles_a4_0.col3 AS col3 
FROM (SELECT DISTINCT all_vehicles_med_a4_0.col0 AS col0, all_vehicles_med_a4_0.col1 AS col1, all_vehicles_med_a4_0.col2 AS col2, all_vehicles_med_a4_0.col3 AS col3 
FROM (SELECT DISTINCT 1 AS col0, peer1_public_a3_0.VEHICLE_ID AS col1, peer1_public_a3_0.AREA AS col2, peer1_public_a3_0.REQUEST_ID AS col3 
FROM public.peer1_public AS peer1_public_a3_0   UNION SELECT DISTINCT 2 AS col0, peer2_public_a3_0.VEHICLE_ID AS col1, peer2_public_a3_0.AREA AS col2, peer2_public_a3_0.REQUEST_ID AS col3 
FROM public.peer2_public AS peer2_public_a3_0  ) AS all_vehicles_med_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT all_vehicles_med_a4_0.col1 AS col0, all_vehicles_med_a4_0.col2 AS col1, all_vehicles_med_a4_0.col3 AS col2 
FROM (SELECT DISTINCT 1 AS col0, peer1_public_a3_0.VEHICLE_ID AS col1, peer1_public_a3_0.AREA AS col2, peer1_public_a3_0.REQUEST_ID AS col3 
FROM public.peer1_public AS peer1_public_a3_0   UNION SELECT DISTINCT 2 AS col0, peer2_public_a3_0.VEHICLE_ID AS col1, peer2_public_a3_0.AREA AS col2, peer2_public_a3_0.REQUEST_ID AS col3 
FROM public.peer2_public AS peer2_public_a3_0  ) AS all_vehicles_med_a4_0 
WHERE all_vehicles_med_a4_0.col0 = 1 AND NOT EXISTS ( SELECT * 
FROM public.peer1_public AS peer1_public_a3 
WHERE peer1_public_a3.REQUEST_ID IS NOT DISTINCT FROM all_vehicles_med_a4_0.col3 AND peer1_public_a3.AREA IS NOT DISTINCT FROM all_vehicles_med_a4_0.col2 AND peer1_public_a3.VEHICLE_ID IS NOT DISTINCT FROM all_vehicles_med_a4_0.col1 ) ) AS __dummy__delta__insert__peer1_public_a3 
WHERE __dummy__delta__insert__peer1_public_a3.col2 IS NOT DISTINCT FROM all_vehicles_med_a4_0.col3 AND __dummy__delta__insert__peer1_public_a3.col1 IS NOT DISTINCT FROM all_vehicles_med_a4_0.col2 AND __dummy__delta__insert__peer1_public_a3.col0 IS NOT DISTINCT FROM all_vehicles_med_a4_0.col1 ) AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT all_vehicles_med_a4_0.col1 AS col0, all_vehicles_med_a4_0.col2 AS col1, all_vehicles_med_a4_0.col3 AS col2 
FROM (SELECT DISTINCT 1 AS col0, peer1_public_a3_0.VEHICLE_ID AS col1, peer1_public_a3_0.AREA AS col2, peer1_public_a3_0.REQUEST_ID AS col3 
FROM public.peer1_public AS peer1_public_a3_0   UNION SELECT DISTINCT 2 AS col0, peer2_public_a3_0.VEHICLE_ID AS col1, peer2_public_a3_0.AREA AS col2, peer2_public_a3_0.REQUEST_ID AS col3 
FROM public.peer2_public AS peer2_public_a3_0  ) AS all_vehicles_med_a4_0 
WHERE all_vehicles_med_a4_0.col0 = 2 AND NOT EXISTS ( SELECT * 
FROM public.peer2_public AS peer2_public_a3 
WHERE peer2_public_a3.REQUEST_ID IS NOT DISTINCT FROM all_vehicles_med_a4_0.col3 AND peer2_public_a3.AREA IS NOT DISTINCT FROM all_vehicles_med_a4_0.col2 AND peer2_public_a3.VEHICLE_ID IS NOT DISTINCT FROM all_vehicles_med_a4_0.col1 ) ) AS __dummy__delta__insert__peer2_public_a3 
WHERE __dummy__delta__insert__peer2_public_a3.col2 IS NOT DISTINCT FROM all_vehicles_med_a4_0.col3 AND __dummy__delta__insert__peer2_public_a3.col1 IS NOT DISTINCT FROM all_vehicles_med_a4_0.col2 AND __dummy__delta__insert__peer2_public_a3.col0 IS NOT DISTINCT FROM all_vehicles_med_a4_0.col1 ) ) AS all_vehicles_a4_0  ) AS __dummy__;

CREATE OR REPLACE FUNCTION public.all_vehicles_delta_action()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  temprec__dummy__delta__delete__peer1_public public.peer1_public%ROWTYPE;
temprec__dummy__delta__delete__peer2_public public.peer2_public%ROWTYPE;
temprec__dummy__delta__insert__peer1_public public.peer1_public%ROWTYPE;
temprec__dummy__delta__insert__peer2_public public.peer2_public%ROWTYPE;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'all_vehicles_delta_action_flag') THEN
        -- RAISE NOTICE 'execute procedure all_vehicles_delta_action';
        CREATE TEMPORARY TABLE all_vehicles_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        CREATE TEMPORARY TABLE __dummy__delta__delete__peer1_public WITH OIDS ON COMMIT DROP AS SELECT (ROW(col0,col1,col2) :: public.peer1_public).* 
            FROM (SELECT DISTINCT __dummy__delta__delete__peer1_public_a3_0.col0 AS col0, __dummy__delta__delete__peer1_public_a3_0.col1 AS col1, __dummy__delta__delete__peer1_public_a3_0.col2 AS col2 
FROM (SELECT DISTINCT peer1_public_a3_0.VEHICLE_ID AS col0, peer1_public_a3_0.AREA AS col1, peer1_public_a3_0.REQUEST_ID AS col2 
FROM public.peer1_public AS peer1_public_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__all_vehicles_a4_0.COMPANY_ID AS col0, __temp__all_vehicles_a4_0.VEHICLE_ID AS col1, __temp__all_vehicles_a4_0.AREA AS col2, __temp__all_vehicles_a4_0.REQUEST_ID AS col3 
FROM __temp__all_vehicles AS __temp__all_vehicles_a4_0  ) AS all_vehicles_a4 
WHERE all_vehicles_a4.col3 IS NOT DISTINCT FROM peer1_public_a3_0.REQUEST_ID AND all_vehicles_a4.col2 IS NOT DISTINCT FROM peer1_public_a3_0.AREA AND all_vehicles_a4.col1 IS NOT DISTINCT FROM peer1_public_a3_0.VEHICLE_ID AND all_vehicles_a4.col0 IS NOT DISTINCT FROM 1 ) ) AS __dummy__delta__delete__peer1_public_a3_0  ) AS __dummy__delta__delete__peer1_public_extra_alias;

CREATE TEMPORARY TABLE __dummy__delta__delete__peer2_public WITH OIDS ON COMMIT DROP AS SELECT (ROW(col0,col1,col2) :: public.peer2_public).* 
            FROM (SELECT DISTINCT __dummy__delta__delete__peer2_public_a3_0.col0 AS col0, __dummy__delta__delete__peer2_public_a3_0.col1 AS col1, __dummy__delta__delete__peer2_public_a3_0.col2 AS col2 
FROM (SELECT DISTINCT peer2_public_a3_0.VEHICLE_ID AS col0, peer2_public_a3_0.AREA AS col1, peer2_public_a3_0.REQUEST_ID AS col2 
FROM public.peer2_public AS peer2_public_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__all_vehicles_a4_0.COMPANY_ID AS col0, __temp__all_vehicles_a4_0.VEHICLE_ID AS col1, __temp__all_vehicles_a4_0.AREA AS col2, __temp__all_vehicles_a4_0.REQUEST_ID AS col3 
FROM __temp__all_vehicles AS __temp__all_vehicles_a4_0  ) AS all_vehicles_a4 
WHERE all_vehicles_a4.col3 IS NOT DISTINCT FROM peer2_public_a3_0.REQUEST_ID AND all_vehicles_a4.col2 IS NOT DISTINCT FROM peer2_public_a3_0.AREA AND all_vehicles_a4.col1 IS NOT DISTINCT FROM peer2_public_a3_0.VEHICLE_ID AND all_vehicles_a4.col0 IS NOT DISTINCT FROM 2 ) ) AS __dummy__delta__delete__peer2_public_a3_0  ) AS __dummy__delta__delete__peer2_public_extra_alias;

CREATE TEMPORARY TABLE __dummy__delta__insert__peer1_public WITH OIDS ON COMMIT DROP AS SELECT (ROW(col0,col1,col2) :: public.peer1_public).* 
            FROM (SELECT DISTINCT __dummy__delta__insert__peer1_public_a3_0.col0 AS col0, __dummy__delta__insert__peer1_public_a3_0.col1 AS col1, __dummy__delta__insert__peer1_public_a3_0.col2 AS col2 
FROM (SELECT DISTINCT all_vehicles_a4_0.col1 AS col0, all_vehicles_a4_0.col2 AS col1, all_vehicles_a4_0.col3 AS col2 
FROM (SELECT DISTINCT __temp__all_vehicles_a4_0.COMPANY_ID AS col0, __temp__all_vehicles_a4_0.VEHICLE_ID AS col1, __temp__all_vehicles_a4_0.AREA AS col2, __temp__all_vehicles_a4_0.REQUEST_ID AS col3 
FROM __temp__all_vehicles AS __temp__all_vehicles_a4_0  ) AS all_vehicles_a4_0 
WHERE all_vehicles_a4_0.col0 = 1 AND NOT EXISTS ( SELECT * 
FROM public.peer1_public AS peer1_public_a3 
WHERE peer1_public_a3.REQUEST_ID IS NOT DISTINCT FROM all_vehicles_a4_0.col3 AND peer1_public_a3.AREA IS NOT DISTINCT FROM all_vehicles_a4_0.col2 AND peer1_public_a3.VEHICLE_ID IS NOT DISTINCT FROM all_vehicles_a4_0.col1 ) ) AS __dummy__delta__insert__peer1_public_a3_0  ) AS __dummy__delta__insert__peer1_public_extra_alias;

CREATE TEMPORARY TABLE __dummy__delta__insert__peer2_public WITH OIDS ON COMMIT DROP AS SELECT (ROW(col0,col1,col2) :: public.peer2_public).* 
            FROM (SELECT DISTINCT __dummy__delta__insert__peer2_public_a3_0.col0 AS col0, __dummy__delta__insert__peer2_public_a3_0.col1 AS col1, __dummy__delta__insert__peer2_public_a3_0.col2 AS col2 
FROM (SELECT DISTINCT all_vehicles_a4_0.col1 AS col0, all_vehicles_a4_0.col2 AS col1, all_vehicles_a4_0.col3 AS col2 
FROM (SELECT DISTINCT __temp__all_vehicles_a4_0.COMPANY_ID AS col0, __temp__all_vehicles_a4_0.VEHICLE_ID AS col1, __temp__all_vehicles_a4_0.AREA AS col2, __temp__all_vehicles_a4_0.REQUEST_ID AS col3 
FROM __temp__all_vehicles AS __temp__all_vehicles_a4_0  ) AS all_vehicles_a4_0 
WHERE all_vehicles_a4_0.col0 = 2 AND NOT EXISTS ( SELECT * 
FROM public.peer2_public AS peer2_public_a3 
WHERE peer2_public_a3.REQUEST_ID IS NOT DISTINCT FROM all_vehicles_a4_0.col3 AND peer2_public_a3.AREA IS NOT DISTINCT FROM all_vehicles_a4_0.col2 AND peer2_public_a3.VEHICLE_ID IS NOT DISTINCT FROM all_vehicles_a4_0.col1 ) ) AS __dummy__delta__insert__peer2_public_a3_0  ) AS __dummy__delta__insert__peer2_public_extra_alias; 

FOR temprec__dummy__delta__delete__peer1_public IN ( SELECT * FROM __dummy__delta__delete__peer1_public) LOOP 
            DELETE FROM public.peer1_public WHERE ROW(VEHICLE_ID,AREA,REQUEST_ID) IS NOT DISTINCT FROM  temprec__dummy__delta__delete__peer1_public;
            END LOOP;
DROP TABLE __dummy__delta__delete__peer1_public;

FOR temprec__dummy__delta__delete__peer2_public IN ( SELECT * FROM __dummy__delta__delete__peer2_public) LOOP 
            DELETE FROM public.peer2_public WHERE ROW(VEHICLE_ID,AREA,REQUEST_ID) IS NOT DISTINCT FROM  temprec__dummy__delta__delete__peer2_public;
            END LOOP;
DROP TABLE __dummy__delta__delete__peer2_public;

INSERT INTO public.peer1_public SELECT * FROM  __dummy__delta__insert__peer1_public; 
DROP TABLE __dummy__delta__insert__peer1_public;

INSERT INTO public.peer2_public SELECT * FROM  __dummy__delta__insert__peer2_public; 
DROP TABLE __dummy__delta__insert__peer2_public;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.all_vehicles';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.all_vehicles ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

CREATE OR REPLACE FUNCTION public.all_vehicles_materialization()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '__temp__all_vehicles')
    THEN
        -- RAISE NOTICE 'execute procedure all_vehicles_materialization';
        CREATE TEMPORARY TABLE __temp__all_vehicles WITH OIDS ON COMMIT DROP AS SELECT * FROM public.all_vehicles;
        CREATE CONSTRAINT TRIGGER __temp__peer1_public_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__all_vehicles DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.all_vehicles_delta_action();
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.all_vehicles';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.all_vehicles ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS all_vehicles_trigger_materialization ON public.all_vehicles;
CREATE TRIGGER all_vehicles_trigger_materialization
    BEFORE INSERT OR UPDATE OR DELETE ON
      public.all_vehicles FOR EACH STATEMENT EXECUTE PROCEDURE public.all_vehicles_materialization();

CREATE OR REPLACE FUNCTION public.all_vehicles_update()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    -- RAISE NOTICE 'execute procedure all_vehicles_update';
    IF TG_OP = 'INSERT' THEN
      INSERT INTO __temp__all_vehicles SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM __temp__all_vehicles WHERE ROW(COMPANY_ID,VEHICLE_ID,AREA,REQUEST_ID) IS NOT DISTINCT FROM OLD;
      INSERT INTO __temp__all_vehicles SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      DELETE FROM __temp__all_vehicles WHERE ROW(COMPANY_ID,VEHICLE_ID,AREA,REQUEST_ID) IS NOT DISTINCT FROM OLD;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.all_vehicles';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.all_vehicles ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS all_vehicles_trigger_update ON public.all_vehicles;
CREATE TRIGGER all_vehicles_trigger_update
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.all_vehicles FOR EACH ROW EXECUTE PROCEDURE public.all_vehicles_update();

