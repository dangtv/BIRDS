/*view definition (get):
all_vehicles(COMPANY_ID, VEHICLE_ID, CURRENT_AREA, SEAT_COUNT, REQUEST_ID, PICKUP_LOCATION, DROPOFF_LOCATION) :- p_0(COMPANY_ID, VEHICLE_ID, CURRENT_AREA, SEAT_COUNT, REQUEST_ID, PICKUP_LOCATION, DROPOFF_LOCATION).
p_0(COMPANY_ID, VEHICLE_ID, CURRENT_AREA, SEAT_COUNT, REQUEST_ID, PICKUP_LOCATION, DROPOFF_LOCATION) :- COMPANY_ID = 1 , peer1_public(VEHICLE_ID, CURRENT_AREA, SEAT_COUNT, REQUEST_ID, PICKUP_LOCATION, DROPOFF_LOCATION).
p_0(COMPANY_ID, VEHICLE_ID, CURRENT_AREA, SEAT_COUNT, REQUEST_ID, PICKUP_LOCATION, DROPOFF_LOCATION) :- COMPANY_ID = 2 , peer2_public(VEHICLE_ID, CURRENT_AREA, SEAT_COUNT, REQUEST_ID, PICKUP_LOCATION, DROPOFF_LOCATION).
*/
CREATE OR REPLACE VIEW public.all_vehicles AS 
SELECT __dummy__.COL0 AS COMPANY_ID,__dummy__.COL1 AS VEHICLE_ID,__dummy__.COL2 AS CURRENT_AREA,__dummy__.COL3 AS SEAT_COUNT,__dummy__.COL4 AS REQUEST_ID,__dummy__.COL5 AS PICKUP_LOCATION,__dummy__.COL6 AS DROPOFF_LOCATION 
FROM (SELECT DISTINCT all_vehicles_a7_0.COL0 AS COL0, all_vehicles_a7_0.COL1 AS COL1, all_vehicles_a7_0.COL2 AS COL2, all_vehicles_a7_0.COL3 AS COL3, all_vehicles_a7_0.COL4 AS COL4, all_vehicles_a7_0.COL5 AS COL5, all_vehicles_a7_0.COL6 AS COL6 
FROM (SELECT DISTINCT p_0_a7_0.COL0 AS COL0, p_0_a7_0.COL1 AS COL1, p_0_a7_0.COL2 AS COL2, p_0_a7_0.COL3 AS COL3, p_0_a7_0.COL4 AS COL4, p_0_a7_0.COL5 AS COL5, p_0_a7_0.COL6 AS COL6 
FROM (SELECT DISTINCT 1 AS COL0, peer1_public_a6_0.VEHICLE_ID AS COL1, peer1_public_a6_0.CURRENT_AREA AS COL2, peer1_public_a6_0.SEAT_COUNT AS COL3, peer1_public_a6_0.REQUEST_ID AS COL4, peer1_public_a6_0.PICKUP_LOCATION AS COL5, peer1_public_a6_0.DROPOFF_LOCATION AS COL6 
FROM public.peer1_public AS peer1_public_a6_0   UNION SELECT DISTINCT 2 AS COL0, peer2_public_a6_0.VEHICLE_ID AS COL1, peer2_public_a6_0.CURRENT_AREA AS COL2, peer2_public_a6_0.SEAT_COUNT AS COL3, peer2_public_a6_0.REQUEST_ID AS COL4, peer2_public_a6_0.PICKUP_LOCATION AS COL5, peer2_public_a6_0.DROPOFF_LOCATION AS COL6 
FROM public.peer2_public AS peer2_public_a6_0  ) AS p_0_a7_0  ) AS all_vehicles_a7_0  ) AS __dummy__;


CREATE OR REPLACE FUNCTION public.all_vehicles_delta_action()
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
  temprecΔ_del_peer1_public public.peer1_public%ROWTYPE;
temprecΔ_del_peer2_public public.peer2_public%ROWTYPE;
temprecΔ_ins_peer1_public public.peer1_public%ROWTYPE;
temprecΔ_ins_peer2_public public.peer2_public%ROWTYPE;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'all_vehicles_delta_action_flag') THEN
        -- RAISE LOG 'execute procedure all_vehicles_delta_action';
        CREATE TEMPORARY TABLE all_vehicles_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        IF EXISTS (SELECT  
FROM (SELECT  
FROM (SELECT  
FROM (SELECT DISTINCT all_vehicles_a7_0.COMPANY_ID AS COL0, all_vehicles_a7_0.VEHICLE_ID AS COL1, all_vehicles_a7_0.CURRENT_AREA AS COL2, all_vehicles_a7_0.SEAT_COUNT AS COL3, all_vehicles_a7_0.REQUEST_ID AS COL4, all_vehicles_a7_0.PICKUP_LOCATION AS COL5, all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM public.all_vehicles AS all_vehicles_a7_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_all_vehicles AS __temp__Δ_del_all_vehicles_a7 
WHERE __temp__Δ_del_all_vehicles_a7.DROPOFF_LOCATION = all_vehicles_a7_0.DROPOFF_LOCATION AND __temp__Δ_del_all_vehicles_a7.PICKUP_LOCATION = all_vehicles_a7_0.PICKUP_LOCATION AND __temp__Δ_del_all_vehicles_a7.REQUEST_ID = all_vehicles_a7_0.REQUEST_ID AND __temp__Δ_del_all_vehicles_a7.SEAT_COUNT = all_vehicles_a7_0.SEAT_COUNT AND __temp__Δ_del_all_vehicles_a7.CURRENT_AREA = all_vehicles_a7_0.CURRENT_AREA AND __temp__Δ_del_all_vehicles_a7.VEHICLE_ID = all_vehicles_a7_0.VEHICLE_ID AND __temp__Δ_del_all_vehicles_a7.COMPANY_ID = all_vehicles_a7_0.COMPANY_ID )  UNION SELECT DISTINCT __temp__Δ_ins_all_vehicles_a7_0.COMPANY_ID AS COL0, __temp__Δ_ins_all_vehicles_a7_0.VEHICLE_ID AS COL1, __temp__Δ_ins_all_vehicles_a7_0.CURRENT_AREA AS COL2, __temp__Δ_ins_all_vehicles_a7_0.SEAT_COUNT AS COL3, __temp__Δ_ins_all_vehicles_a7_0.REQUEST_ID AS COL4, __temp__Δ_ins_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __temp__Δ_ins_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM __temp__Δ_ins_all_vehicles AS __temp__Δ_ins_all_vehicles_a7_0  ) AS new_all_vehicles_a7_0 
WHERE new_all_vehicles_a7_0.COL0  <>  1 AND new_all_vehicles_a7_0.COL0  <>  2 ) AS ⊥_a0_0  ) AS __dummy__ )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid update on view';
        END IF;
        CREATE TEMPORARY TABLE Δ_del_peer1_public WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2,COL3,COL4,COL5) :: public.peer1_public).* 
            FROM (SELECT DISTINCT Δ_del_peer1_public_a6_0.COL0 AS COL0, Δ_del_peer1_public_a6_0.COL1 AS COL1, Δ_del_peer1_public_a6_0.COL2 AS COL2, Δ_del_peer1_public_a6_0.COL3 AS COL3, Δ_del_peer1_public_a6_0.COL4 AS COL4, Δ_del_peer1_public_a6_0.COL5 AS COL5 
FROM (SELECT DISTINCT peer1_public_a6_0.VEHICLE_ID AS COL0, peer1_public_a6_0.CURRENT_AREA AS COL1, peer1_public_a6_0.SEAT_COUNT AS COL2, peer1_public_a6_0.REQUEST_ID AS COL3, peer1_public_a6_0.PICKUP_LOCATION AS COL4, peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.peer1_public AS peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT all_vehicles_a7_0.COMPANY_ID AS COL0, all_vehicles_a7_0.VEHICLE_ID AS COL1, all_vehicles_a7_0.CURRENT_AREA AS COL2, all_vehicles_a7_0.SEAT_COUNT AS COL3, all_vehicles_a7_0.REQUEST_ID AS COL4, all_vehicles_a7_0.PICKUP_LOCATION AS COL5, all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM public.all_vehicles AS all_vehicles_a7_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_all_vehicles AS __temp__Δ_del_all_vehicles_a7 
WHERE __temp__Δ_del_all_vehicles_a7.DROPOFF_LOCATION = all_vehicles_a7_0.DROPOFF_LOCATION AND __temp__Δ_del_all_vehicles_a7.PICKUP_LOCATION = all_vehicles_a7_0.PICKUP_LOCATION AND __temp__Δ_del_all_vehicles_a7.REQUEST_ID = all_vehicles_a7_0.REQUEST_ID AND __temp__Δ_del_all_vehicles_a7.SEAT_COUNT = all_vehicles_a7_0.SEAT_COUNT AND __temp__Δ_del_all_vehicles_a7.CURRENT_AREA = all_vehicles_a7_0.CURRENT_AREA AND __temp__Δ_del_all_vehicles_a7.VEHICLE_ID = all_vehicles_a7_0.VEHICLE_ID AND __temp__Δ_del_all_vehicles_a7.COMPANY_ID = all_vehicles_a7_0.COMPANY_ID )  UNION SELECT DISTINCT __temp__Δ_ins_all_vehicles_a7_0.COMPANY_ID AS COL0, __temp__Δ_ins_all_vehicles_a7_0.VEHICLE_ID AS COL1, __temp__Δ_ins_all_vehicles_a7_0.CURRENT_AREA AS COL2, __temp__Δ_ins_all_vehicles_a7_0.SEAT_COUNT AS COL3, __temp__Δ_ins_all_vehicles_a7_0.REQUEST_ID AS COL4, __temp__Δ_ins_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __temp__Δ_ins_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM __temp__Δ_ins_all_vehicles AS __temp__Δ_ins_all_vehicles_a7_0  ) AS new_all_vehicles_a7 
WHERE new_all_vehicles_a7.COL6 = peer1_public_a6_0.DROPOFF_LOCATION AND new_all_vehicles_a7.COL5 = peer1_public_a6_0.PICKUP_LOCATION AND new_all_vehicles_a7.COL4 = peer1_public_a6_0.REQUEST_ID AND new_all_vehicles_a7.COL3 = peer1_public_a6_0.SEAT_COUNT AND new_all_vehicles_a7.COL2 = peer1_public_a6_0.CURRENT_AREA AND new_all_vehicles_a7.COL1 = peer1_public_a6_0.VEHICLE_ID AND new_all_vehicles_a7.COL0 = 1 ) ) AS Δ_del_peer1_public_a6_0  ) AS Δ_del_peer1_public_extra_alias;

CREATE TEMPORARY TABLE Δ_del_peer2_public WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2,COL3,COL4,COL5) :: public.peer2_public).* 
            FROM (SELECT DISTINCT Δ_del_peer2_public_a6_0.COL0 AS COL0, Δ_del_peer2_public_a6_0.COL1 AS COL1, Δ_del_peer2_public_a6_0.COL2 AS COL2, Δ_del_peer2_public_a6_0.COL3 AS COL3, Δ_del_peer2_public_a6_0.COL4 AS COL4, Δ_del_peer2_public_a6_0.COL5 AS COL5 
FROM (SELECT DISTINCT peer2_public_a6_0.VEHICLE_ID AS COL0, peer2_public_a6_0.CURRENT_AREA AS COL1, peer2_public_a6_0.SEAT_COUNT AS COL2, peer2_public_a6_0.REQUEST_ID AS COL3, peer2_public_a6_0.PICKUP_LOCATION AS COL4, peer2_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.peer2_public AS peer2_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT all_vehicles_a7_0.COMPANY_ID AS COL0, all_vehicles_a7_0.VEHICLE_ID AS COL1, all_vehicles_a7_0.CURRENT_AREA AS COL2, all_vehicles_a7_0.SEAT_COUNT AS COL3, all_vehicles_a7_0.REQUEST_ID AS COL4, all_vehicles_a7_0.PICKUP_LOCATION AS COL5, all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM public.all_vehicles AS all_vehicles_a7_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_all_vehicles AS __temp__Δ_del_all_vehicles_a7 
WHERE __temp__Δ_del_all_vehicles_a7.DROPOFF_LOCATION = all_vehicles_a7_0.DROPOFF_LOCATION AND __temp__Δ_del_all_vehicles_a7.PICKUP_LOCATION = all_vehicles_a7_0.PICKUP_LOCATION AND __temp__Δ_del_all_vehicles_a7.REQUEST_ID = all_vehicles_a7_0.REQUEST_ID AND __temp__Δ_del_all_vehicles_a7.SEAT_COUNT = all_vehicles_a7_0.SEAT_COUNT AND __temp__Δ_del_all_vehicles_a7.CURRENT_AREA = all_vehicles_a7_0.CURRENT_AREA AND __temp__Δ_del_all_vehicles_a7.VEHICLE_ID = all_vehicles_a7_0.VEHICLE_ID AND __temp__Δ_del_all_vehicles_a7.COMPANY_ID = all_vehicles_a7_0.COMPANY_ID )  UNION SELECT DISTINCT __temp__Δ_ins_all_vehicles_a7_0.COMPANY_ID AS COL0, __temp__Δ_ins_all_vehicles_a7_0.VEHICLE_ID AS COL1, __temp__Δ_ins_all_vehicles_a7_0.CURRENT_AREA AS COL2, __temp__Δ_ins_all_vehicles_a7_0.SEAT_COUNT AS COL3, __temp__Δ_ins_all_vehicles_a7_0.REQUEST_ID AS COL4, __temp__Δ_ins_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __temp__Δ_ins_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM __temp__Δ_ins_all_vehicles AS __temp__Δ_ins_all_vehicles_a7_0  ) AS new_all_vehicles_a7 
WHERE new_all_vehicles_a7.COL6 = peer2_public_a6_0.DROPOFF_LOCATION AND new_all_vehicles_a7.COL5 = peer2_public_a6_0.PICKUP_LOCATION AND new_all_vehicles_a7.COL4 = peer2_public_a6_0.REQUEST_ID AND new_all_vehicles_a7.COL3 = peer2_public_a6_0.SEAT_COUNT AND new_all_vehicles_a7.COL2 = peer2_public_a6_0.CURRENT_AREA AND new_all_vehicles_a7.COL1 = peer2_public_a6_0.VEHICLE_ID AND new_all_vehicles_a7.COL0 = 2 ) ) AS Δ_del_peer2_public_a6_0  ) AS Δ_del_peer2_public_extra_alias;

CREATE TEMPORARY TABLE Δ_ins_peer1_public WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2,COL3,COL4,COL5) :: public.peer1_public).* 
            FROM (SELECT DISTINCT Δ_ins_peer1_public_a6_0.COL0 AS COL0, Δ_ins_peer1_public_a6_0.COL1 AS COL1, Δ_ins_peer1_public_a6_0.COL2 AS COL2, Δ_ins_peer1_public_a6_0.COL3 AS COL3, Δ_ins_peer1_public_a6_0.COL4 AS COL4, Δ_ins_peer1_public_a6_0.COL5 AS COL5 
FROM (SELECT DISTINCT new_all_vehicles_a7_0.COL1 AS COL0, new_all_vehicles_a7_0.COL2 AS COL1, new_all_vehicles_a7_0.COL3 AS COL2, new_all_vehicles_a7_0.COL4 AS COL3, new_all_vehicles_a7_0.COL5 AS COL4, new_all_vehicles_a7_0.COL6 AS COL5 
FROM (SELECT DISTINCT all_vehicles_a7_0.COMPANY_ID AS COL0, all_vehicles_a7_0.VEHICLE_ID AS COL1, all_vehicles_a7_0.CURRENT_AREA AS COL2, all_vehicles_a7_0.SEAT_COUNT AS COL3, all_vehicles_a7_0.REQUEST_ID AS COL4, all_vehicles_a7_0.PICKUP_LOCATION AS COL5, all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM public.all_vehicles AS all_vehicles_a7_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_all_vehicles AS __temp__Δ_del_all_vehicles_a7 
WHERE __temp__Δ_del_all_vehicles_a7.DROPOFF_LOCATION = all_vehicles_a7_0.DROPOFF_LOCATION AND __temp__Δ_del_all_vehicles_a7.PICKUP_LOCATION = all_vehicles_a7_0.PICKUP_LOCATION AND __temp__Δ_del_all_vehicles_a7.REQUEST_ID = all_vehicles_a7_0.REQUEST_ID AND __temp__Δ_del_all_vehicles_a7.SEAT_COUNT = all_vehicles_a7_0.SEAT_COUNT AND __temp__Δ_del_all_vehicles_a7.CURRENT_AREA = all_vehicles_a7_0.CURRENT_AREA AND __temp__Δ_del_all_vehicles_a7.VEHICLE_ID = all_vehicles_a7_0.VEHICLE_ID AND __temp__Δ_del_all_vehicles_a7.COMPANY_ID = all_vehicles_a7_0.COMPANY_ID )  UNION SELECT DISTINCT __temp__Δ_ins_all_vehicles_a7_0.COMPANY_ID AS COL0, __temp__Δ_ins_all_vehicles_a7_0.VEHICLE_ID AS COL1, __temp__Δ_ins_all_vehicles_a7_0.CURRENT_AREA AS COL2, __temp__Δ_ins_all_vehicles_a7_0.SEAT_COUNT AS COL3, __temp__Δ_ins_all_vehicles_a7_0.REQUEST_ID AS COL4, __temp__Δ_ins_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __temp__Δ_ins_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM __temp__Δ_ins_all_vehicles AS __temp__Δ_ins_all_vehicles_a7_0  ) AS new_all_vehicles_a7_0 
WHERE new_all_vehicles_a7_0.COL0 = 1 AND NOT EXISTS ( SELECT * 
FROM public.peer1_public AS peer1_public_a6 
WHERE peer1_public_a6.DROPOFF_LOCATION = new_all_vehicles_a7_0.COL6 AND peer1_public_a6.PICKUP_LOCATION = new_all_vehicles_a7_0.COL5 AND peer1_public_a6.REQUEST_ID = new_all_vehicles_a7_0.COL4 AND peer1_public_a6.SEAT_COUNT = new_all_vehicles_a7_0.COL3 AND peer1_public_a6.CURRENT_AREA = new_all_vehicles_a7_0.COL2 AND peer1_public_a6.VEHICLE_ID = new_all_vehicles_a7_0.COL1 ) ) AS Δ_ins_peer1_public_a6_0  ) AS Δ_ins_peer1_public_extra_alia 
            EXCEPT ALL 
            SELECT * FROM  public.peer1_public;

CREATE TEMPORARY TABLE Δ_ins_peer2_public WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2,COL3,COL4,COL5) :: public.peer2_public).* 
            FROM (SELECT DISTINCT Δ_ins_peer2_public_a6_0.COL0 AS COL0, Δ_ins_peer2_public_a6_0.COL1 AS COL1, Δ_ins_peer2_public_a6_0.COL2 AS COL2, Δ_ins_peer2_public_a6_0.COL3 AS COL3, Δ_ins_peer2_public_a6_0.COL4 AS COL4, Δ_ins_peer2_public_a6_0.COL5 AS COL5 
FROM (SELECT DISTINCT new_all_vehicles_a7_0.COL1 AS COL0, new_all_vehicles_a7_0.COL2 AS COL1, new_all_vehicles_a7_0.COL3 AS COL2, new_all_vehicles_a7_0.COL4 AS COL3, new_all_vehicles_a7_0.COL5 AS COL4, new_all_vehicles_a7_0.COL6 AS COL5 
FROM (SELECT DISTINCT all_vehicles_a7_0.COMPANY_ID AS COL0, all_vehicles_a7_0.VEHICLE_ID AS COL1, all_vehicles_a7_0.CURRENT_AREA AS COL2, all_vehicles_a7_0.SEAT_COUNT AS COL3, all_vehicles_a7_0.REQUEST_ID AS COL4, all_vehicles_a7_0.PICKUP_LOCATION AS COL5, all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM public.all_vehicles AS all_vehicles_a7_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_all_vehicles AS __temp__Δ_del_all_vehicles_a7 
WHERE __temp__Δ_del_all_vehicles_a7.DROPOFF_LOCATION = all_vehicles_a7_0.DROPOFF_LOCATION AND __temp__Δ_del_all_vehicles_a7.PICKUP_LOCATION = all_vehicles_a7_0.PICKUP_LOCATION AND __temp__Δ_del_all_vehicles_a7.REQUEST_ID = all_vehicles_a7_0.REQUEST_ID AND __temp__Δ_del_all_vehicles_a7.SEAT_COUNT = all_vehicles_a7_0.SEAT_COUNT AND __temp__Δ_del_all_vehicles_a7.CURRENT_AREA = all_vehicles_a7_0.CURRENT_AREA AND __temp__Δ_del_all_vehicles_a7.VEHICLE_ID = all_vehicles_a7_0.VEHICLE_ID AND __temp__Δ_del_all_vehicles_a7.COMPANY_ID = all_vehicles_a7_0.COMPANY_ID )  UNION SELECT DISTINCT __temp__Δ_ins_all_vehicles_a7_0.COMPANY_ID AS COL0, __temp__Δ_ins_all_vehicles_a7_0.VEHICLE_ID AS COL1, __temp__Δ_ins_all_vehicles_a7_0.CURRENT_AREA AS COL2, __temp__Δ_ins_all_vehicles_a7_0.SEAT_COUNT AS COL3, __temp__Δ_ins_all_vehicles_a7_0.REQUEST_ID AS COL4, __temp__Δ_ins_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __temp__Δ_ins_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM __temp__Δ_ins_all_vehicles AS __temp__Δ_ins_all_vehicles_a7_0  ) AS new_all_vehicles_a7_0 
WHERE new_all_vehicles_a7_0.COL0 = 2 AND NOT EXISTS ( SELECT * 
FROM public.peer2_public AS peer2_public_a6 
WHERE peer2_public_a6.DROPOFF_LOCATION = new_all_vehicles_a7_0.COL6 AND peer2_public_a6.PICKUP_LOCATION = new_all_vehicles_a7_0.COL5 AND peer2_public_a6.REQUEST_ID = new_all_vehicles_a7_0.COL4 AND peer2_public_a6.SEAT_COUNT = new_all_vehicles_a7_0.COL3 AND peer2_public_a6.CURRENT_AREA = new_all_vehicles_a7_0.COL2 AND peer2_public_a6.VEHICLE_ID = new_all_vehicles_a7_0.COL1 ) ) AS Δ_ins_peer2_public_a6_0  ) AS Δ_ins_peer2_public_extra_alia 
            EXCEPT ALL 
            SELECT * FROM  public.peer2_public; 

FOR temprecΔ_del_peer1_public IN ( SELECT * FROM Δ_del_peer1_public) LOOP 
            DELETE FROM public.peer1_public WHERE ROW(VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) =  temprecΔ_del_peer1_public;
            END LOOP;
DROP TABLE Δ_del_peer1_public;

FOR temprecΔ_del_peer2_public IN ( SELECT * FROM Δ_del_peer2_public) LOOP 
            DELETE FROM public.peer2_public WHERE ROW(VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) =  temprecΔ_del_peer2_public;
            END LOOP;
DROP TABLE Δ_del_peer2_public;

INSERT INTO public.peer1_public (SELECT * FROM  Δ_ins_peer1_public) ; 
DROP TABLE Δ_ins_peer1_public;

INSERT INTO public.peer2_public (SELECT * FROM  Δ_ins_peer2_public) ; 
DROP TABLE Δ_ins_peer2_public;
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
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '__temp__Δ_ins_all_vehicles' OR table_name = '__temp__Δ_del_all_vehicles')
    THEN
        -- RAISE LOG 'execute procedure all_vehicles_materialization';
        CREATE TEMPORARY TABLE __temp__Δ_ins_all_vehicles ( LIKE public.all_vehicles INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__all_vehicles_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_ins_all_vehicles DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.all_vehicles_delta_action();

        CREATE TEMPORARY TABLE __temp__Δ_del_all_vehicles ( LIKE public.all_vehicles INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__all_vehicles_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_del_all_vehicles DEFERRABLE INITIALLY DEFERRED 
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
    -- RAISE LOG 'execute procedure all_vehicles_update';
    IF TG_OP = 'INSERT' THEN
      -- RAISE LOG 'NEW: %', NEW;
      IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
        RAISE check_violation USING MESSAGE = 'Invalid update on view: view does not accept null value';
      END IF;
      DELETE FROM __temp__Δ_del_all_vehicles WHERE ROW(COMPANY_ID,VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) = NEW;
      INSERT INTO __temp__Δ_ins_all_vehicles SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      IF (SELECT count(*) FILTER (WHERE j.value = jsonb 'null') FROM  jsonb_each(to_jsonb(NEW)) j) > 0 THEN 
        RAISE check_violation USING MESSAGE = 'Invalid update on view: view does not accept null value';
      END IF;
      DELETE FROM __temp__Δ_ins_all_vehicles WHERE ROW(COMPANY_ID,VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) = OLD;
      INSERT INTO __temp__Δ_del_all_vehicles SELECT (OLD).*;
      DELETE FROM __temp__Δ_del_all_vehicles WHERE ROW(COMPANY_ID,VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) = NEW;
      INSERT INTO __temp__Δ_ins_all_vehicles SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      -- RAISE LOG 'OLD: %', OLD;
      DELETE FROM __temp__Δ_ins_all_vehicles WHERE ROW(COMPANY_ID,VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) = OLD;
      INSERT INTO __temp__Δ_del_all_vehicles SELECT (OLD).*;
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

