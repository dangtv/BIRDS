CREATE OR REPLACE VIEW public.all_vehicles AS 
SELECT __dummy__.COL0 AS COMPANY_ID,__dummy__.COL1 AS VEHICLE_ID,__dummy__.COL2 AS CURRENT_AREA,__dummy__.COL3 AS SEAT_COUNT,__dummy__.COL4 AS REQUEST_ID,__dummy__.COL5 AS PICKUP_LOCATION,__dummy__.COL6 AS DROPOFF_LOCATION 
FROM (SELECT DISTINCT all_vehicles_a7_0.COL0 AS COL0, all_vehicles_a7_0.COL1 AS COL1, all_vehicles_a7_0.COL2 AS COL2, all_vehicles_a7_0.COL3 AS COL3, all_vehicles_a7_0.COL4 AS COL4, all_vehicles_a7_0.COL5 AS COL5, all_vehicles_a7_0.COL6 AS COL6 
FROM (SELECT DISTINCT all_vehicles_med_a7_0.COL0 AS COL0, all_vehicles_med_a7_0.COL1 AS COL1, all_vehicles_med_a7_0.COL2 AS COL2, all_vehicles_med_a7_0.COL3 AS COL3, all_vehicles_med_a7_0.COL4 AS COL4, all_vehicles_med_a7_0.COL5 AS COL5, all_vehicles_med_a7_0.COL6 AS COL6 
FROM (SELECT DISTINCT 1 AS COL0, peer1_public_a6_0.VEHICLE_ID AS COL1, peer1_public_a6_0.CURRENT_AREA AS COL2, peer1_public_a6_0.SEAT_COUNT AS COL3, peer1_public_a6_0.REQUEST_ID AS COL4, peer1_public_a6_0.PICKUP_LOCATION AS COL5, peer1_public_a6_0.DROPOFF_LOCATION AS COL6 
FROM public.peer1_public AS peer1_public_a6_0   UNION SELECT DISTINCT 2 AS COL0, peer2_public_a6_0.VEHICLE_ID AS COL1, peer2_public_a6_0.CURRENT_AREA AS COL2, peer2_public_a6_0.SEAT_COUNT AS COL3, peer2_public_a6_0.REQUEST_ID AS COL4, peer2_public_a6_0.PICKUP_LOCATION AS COL5, peer2_public_a6_0.DROPOFF_LOCATION AS COL6 
FROM public.peer2_public AS peer2_public_a6_0  ) AS all_vehicles_med_a7_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT all_vehicles_med_a7_0.COL1 AS COL0, all_vehicles_med_a7_0.COL2 AS COL1, all_vehicles_med_a7_0.COL3 AS COL2, all_vehicles_med_a7_0.COL4 AS COL3, all_vehicles_med_a7_0.COL5 AS COL4, all_vehicles_med_a7_0.COL6 AS COL5 
FROM (SELECT DISTINCT 1 AS COL0, peer1_public_a6_0.VEHICLE_ID AS COL1, peer1_public_a6_0.CURRENT_AREA AS COL2, peer1_public_a6_0.SEAT_COUNT AS COL3, peer1_public_a6_0.REQUEST_ID AS COL4, peer1_public_a6_0.PICKUP_LOCATION AS COL5, peer1_public_a6_0.DROPOFF_LOCATION AS COL6 
FROM public.peer1_public AS peer1_public_a6_0   UNION SELECT DISTINCT 2 AS COL0, peer2_public_a6_0.VEHICLE_ID AS COL1, peer2_public_a6_0.CURRENT_AREA AS COL2, peer2_public_a6_0.SEAT_COUNT AS COL3, peer2_public_a6_0.REQUEST_ID AS COL4, peer2_public_a6_0.PICKUP_LOCATION AS COL5, peer2_public_a6_0.DROPOFF_LOCATION AS COL6 
FROM public.peer2_public AS peer2_public_a6_0  ) AS all_vehicles_med_a7_0 
WHERE all_vehicles_med_a7_0.COL0 = 1 AND NOT EXISTS ( SELECT * 
FROM public.peer1_public AS peer1_public_a6 
WHERE peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL6 AND peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL5 AND peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL4 AND peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL3 AND peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL2 AND peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL1 ) ) AS _derived_Δ_ins_peer1_public_a6 
WHERE _derived_Δ_ins_peer1_public_a6.COL5 IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL6 AND _derived_Δ_ins_peer1_public_a6.COL4 IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL5 AND _derived_Δ_ins_peer1_public_a6.COL3 IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL4 AND _derived_Δ_ins_peer1_public_a6.COL2 IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL3 AND _derived_Δ_ins_peer1_public_a6.COL1 IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL2 AND _derived_Δ_ins_peer1_public_a6.COL0 IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL1 ) AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT all_vehicles_med_a7_0.COL1 AS COL0, all_vehicles_med_a7_0.COL2 AS COL1, all_vehicles_med_a7_0.COL3 AS COL2, all_vehicles_med_a7_0.COL4 AS COL3, all_vehicles_med_a7_0.COL5 AS COL4, all_vehicles_med_a7_0.COL6 AS COL5 
FROM (SELECT DISTINCT 1 AS COL0, peer1_public_a6_0.VEHICLE_ID AS COL1, peer1_public_a6_0.CURRENT_AREA AS COL2, peer1_public_a6_0.SEAT_COUNT AS COL3, peer1_public_a6_0.REQUEST_ID AS COL4, peer1_public_a6_0.PICKUP_LOCATION AS COL5, peer1_public_a6_0.DROPOFF_LOCATION AS COL6 
FROM public.peer1_public AS peer1_public_a6_0   UNION SELECT DISTINCT 2 AS COL0, peer2_public_a6_0.VEHICLE_ID AS COL1, peer2_public_a6_0.CURRENT_AREA AS COL2, peer2_public_a6_0.SEAT_COUNT AS COL3, peer2_public_a6_0.REQUEST_ID AS COL4, peer2_public_a6_0.PICKUP_LOCATION AS COL5, peer2_public_a6_0.DROPOFF_LOCATION AS COL6 
FROM public.peer2_public AS peer2_public_a6_0  ) AS all_vehicles_med_a7_0 
WHERE all_vehicles_med_a7_0.COL0 = 2 AND NOT EXISTS ( SELECT * 
FROM public.peer2_public AS peer2_public_a6 
WHERE peer2_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL6 AND peer2_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL5 AND peer2_public_a6.REQUEST_ID IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL4 AND peer2_public_a6.SEAT_COUNT IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL3 AND peer2_public_a6.CURRENT_AREA IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL2 AND peer2_public_a6.VEHICLE_ID IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL1 ) ) AS _derived_Δ_ins_peer2_public_a6 
WHERE _derived_Δ_ins_peer2_public_a6.COL5 IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL6 AND _derived_Δ_ins_peer2_public_a6.COL4 IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL5 AND _derived_Δ_ins_peer2_public_a6.COL3 IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL4 AND _derived_Δ_ins_peer2_public_a6.COL2 IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL3 AND _derived_Δ_ins_peer2_public_a6.COL1 IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL2 AND _derived_Δ_ins_peer2_public_a6.COL0 IS NOT DISTINCT FROM all_vehicles_med_a7_0.COL1 ) ) AS all_vehicles_a7_0  ) AS __dummy__;

DROP MATERIALIZED VIEW IF EXISTS public.__dummy__materialized_all_vehicles;

CREATE  MATERIALIZED VIEW public.__dummy__materialized_all_vehicles AS 
SELECT * FROM public.all_vehicles;

CREATE OR REPLACE FUNCTION public.all_vehicles_delta_action()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  temprecΔ_del_peer1_public public.peer1_public%ROWTYPE;
temprecΔ_del_peer2_public public.peer2_public%ROWTYPE;
temprecΔ_ins_peer1_public public.peer1_public%ROWTYPE;
temprecΔ_ins_peer2_public public.peer2_public%ROWTYPE;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'all_vehicles_delta_action_flag') THEN
        -- RAISE NOTICE 'execute procedure all_vehicles_delta_action';
        CREATE TEMPORARY TABLE all_vehicles_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        IF EXISTS (SELECT  
FROM (SELECT  
FROM (SELECT  
FROM (SELECT DISTINCT __dummy__materialized_all_vehicles_a7_0.COMPANY_ID AS COL0, __dummy__materialized_all_vehicles_a7_0.VEHICLE_ID AS COL1, __dummy__materialized_all_vehicles_a7_0.CURRENT_AREA AS COL2, __dummy__materialized_all_vehicles_a7_0.SEAT_COUNT AS COL3, __dummy__materialized_all_vehicles_a7_0.REQUEST_ID AS COL4, __dummy__materialized_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __dummy__materialized_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM public.__dummy__materialized_all_vehicles AS __dummy__materialized_all_vehicles_a7_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_all_vehicles AS __temp__Δ_del_all_vehicles_a7 
WHERE __temp__Δ_del_all_vehicles_a7.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.DROPOFF_LOCATION AND __temp__Δ_del_all_vehicles_a7.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.PICKUP_LOCATION AND __temp__Δ_del_all_vehicles_a7.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.REQUEST_ID AND __temp__Δ_del_all_vehicles_a7.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.SEAT_COUNT AND __temp__Δ_del_all_vehicles_a7.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.CURRENT_AREA AND __temp__Δ_del_all_vehicles_a7.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.VEHICLE_ID AND __temp__Δ_del_all_vehicles_a7.COMPANY_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.COMPANY_ID )  UNION SELECT DISTINCT __temp__Δ_ins_all_vehicles_a7_0.COMPANY_ID AS COL0, __temp__Δ_ins_all_vehicles_a7_0.VEHICLE_ID AS COL1, __temp__Δ_ins_all_vehicles_a7_0.CURRENT_AREA AS COL2, __temp__Δ_ins_all_vehicles_a7_0.SEAT_COUNT AS COL3, __temp__Δ_ins_all_vehicles_a7_0.REQUEST_ID AS COL4, __temp__Δ_ins_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __temp__Δ_ins_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM __temp__Δ_ins_all_vehicles AS __temp__Δ_ins_all_vehicles_a7_0  ) AS all_vehicles_a7_0 
WHERE all_vehicles_a7_0.COL0  IS DISTINCT FROM  1 AND all_vehicles_a7_0.COL0  IS DISTINCT FROM  2 ) AS ⊥_a0_0  ) AS __dummy__ )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid update on view';
        END IF;
        CREATE TEMPORARY TABLE Δ_del_peer1_public WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2,COL3,COL4,COL5) :: public.peer1_public).* 
            FROM (SELECT DISTINCT Δ_del_peer1_public_a6_0.COL0 AS COL0, Δ_del_peer1_public_a6_0.COL1 AS COL1, Δ_del_peer1_public_a6_0.COL2 AS COL2, Δ_del_peer1_public_a6_0.COL3 AS COL3, Δ_del_peer1_public_a6_0.COL4 AS COL4, Δ_del_peer1_public_a6_0.COL5 AS COL5 
FROM (SELECT DISTINCT peer1_public_a6_0.VEHICLE_ID AS COL0, peer1_public_a6_0.CURRENT_AREA AS COL1, peer1_public_a6_0.SEAT_COUNT AS COL2, peer1_public_a6_0.REQUEST_ID AS COL3, peer1_public_a6_0.PICKUP_LOCATION AS COL4, peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.peer1_public AS peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __dummy__materialized_all_vehicles_a7_0.COMPANY_ID AS COL0, __dummy__materialized_all_vehicles_a7_0.VEHICLE_ID AS COL1, __dummy__materialized_all_vehicles_a7_0.CURRENT_AREA AS COL2, __dummy__materialized_all_vehicles_a7_0.SEAT_COUNT AS COL3, __dummy__materialized_all_vehicles_a7_0.REQUEST_ID AS COL4, __dummy__materialized_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __dummy__materialized_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM public.__dummy__materialized_all_vehicles AS __dummy__materialized_all_vehicles_a7_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_all_vehicles AS __temp__Δ_del_all_vehicles_a7 
WHERE __temp__Δ_del_all_vehicles_a7.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.DROPOFF_LOCATION AND __temp__Δ_del_all_vehicles_a7.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.PICKUP_LOCATION AND __temp__Δ_del_all_vehicles_a7.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.REQUEST_ID AND __temp__Δ_del_all_vehicles_a7.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.SEAT_COUNT AND __temp__Δ_del_all_vehicles_a7.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.CURRENT_AREA AND __temp__Δ_del_all_vehicles_a7.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.VEHICLE_ID AND __temp__Δ_del_all_vehicles_a7.COMPANY_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.COMPANY_ID )  UNION SELECT DISTINCT __temp__Δ_ins_all_vehicles_a7_0.COMPANY_ID AS COL0, __temp__Δ_ins_all_vehicles_a7_0.VEHICLE_ID AS COL1, __temp__Δ_ins_all_vehicles_a7_0.CURRENT_AREA AS COL2, __temp__Δ_ins_all_vehicles_a7_0.SEAT_COUNT AS COL3, __temp__Δ_ins_all_vehicles_a7_0.REQUEST_ID AS COL4, __temp__Δ_ins_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __temp__Δ_ins_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM __temp__Δ_ins_all_vehicles AS __temp__Δ_ins_all_vehicles_a7_0  ) AS all_vehicles_a7 
WHERE all_vehicles_a7.COL6 IS NOT DISTINCT FROM peer1_public_a6_0.DROPOFF_LOCATION AND all_vehicles_a7.COL5 IS NOT DISTINCT FROM peer1_public_a6_0.PICKUP_LOCATION AND all_vehicles_a7.COL4 IS NOT DISTINCT FROM peer1_public_a6_0.REQUEST_ID AND all_vehicles_a7.COL3 IS NOT DISTINCT FROM peer1_public_a6_0.SEAT_COUNT AND all_vehicles_a7.COL2 IS NOT DISTINCT FROM peer1_public_a6_0.CURRENT_AREA AND all_vehicles_a7.COL1 IS NOT DISTINCT FROM peer1_public_a6_0.VEHICLE_ID AND all_vehicles_a7.COL0 IS NOT DISTINCT FROM 1 ) ) AS Δ_del_peer1_public_a6_0  ) AS Δ_del_peer1_public_extra_alias;

CREATE TEMPORARY TABLE Δ_del_peer2_public WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2,COL3,COL4,COL5) :: public.peer2_public).* 
            FROM (SELECT DISTINCT Δ_del_peer2_public_a6_0.COL0 AS COL0, Δ_del_peer2_public_a6_0.COL1 AS COL1, Δ_del_peer2_public_a6_0.COL2 AS COL2, Δ_del_peer2_public_a6_0.COL3 AS COL3, Δ_del_peer2_public_a6_0.COL4 AS COL4, Δ_del_peer2_public_a6_0.COL5 AS COL5 
FROM (SELECT DISTINCT peer2_public_a6_0.VEHICLE_ID AS COL0, peer2_public_a6_0.CURRENT_AREA AS COL1, peer2_public_a6_0.SEAT_COUNT AS COL2, peer2_public_a6_0.REQUEST_ID AS COL3, peer2_public_a6_0.PICKUP_LOCATION AS COL4, peer2_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.peer2_public AS peer2_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __dummy__materialized_all_vehicles_a7_0.COMPANY_ID AS COL0, __dummy__materialized_all_vehicles_a7_0.VEHICLE_ID AS COL1, __dummy__materialized_all_vehicles_a7_0.CURRENT_AREA AS COL2, __dummy__materialized_all_vehicles_a7_0.SEAT_COUNT AS COL3, __dummy__materialized_all_vehicles_a7_0.REQUEST_ID AS COL4, __dummy__materialized_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __dummy__materialized_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM public.__dummy__materialized_all_vehicles AS __dummy__materialized_all_vehicles_a7_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_all_vehicles AS __temp__Δ_del_all_vehicles_a7 
WHERE __temp__Δ_del_all_vehicles_a7.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.DROPOFF_LOCATION AND __temp__Δ_del_all_vehicles_a7.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.PICKUP_LOCATION AND __temp__Δ_del_all_vehicles_a7.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.REQUEST_ID AND __temp__Δ_del_all_vehicles_a7.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.SEAT_COUNT AND __temp__Δ_del_all_vehicles_a7.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.CURRENT_AREA AND __temp__Δ_del_all_vehicles_a7.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.VEHICLE_ID AND __temp__Δ_del_all_vehicles_a7.COMPANY_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.COMPANY_ID )  UNION SELECT DISTINCT __temp__Δ_ins_all_vehicles_a7_0.COMPANY_ID AS COL0, __temp__Δ_ins_all_vehicles_a7_0.VEHICLE_ID AS COL1, __temp__Δ_ins_all_vehicles_a7_0.CURRENT_AREA AS COL2, __temp__Δ_ins_all_vehicles_a7_0.SEAT_COUNT AS COL3, __temp__Δ_ins_all_vehicles_a7_0.REQUEST_ID AS COL4, __temp__Δ_ins_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __temp__Δ_ins_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM __temp__Δ_ins_all_vehicles AS __temp__Δ_ins_all_vehicles_a7_0  ) AS all_vehicles_a7 
WHERE all_vehicles_a7.COL6 IS NOT DISTINCT FROM peer2_public_a6_0.DROPOFF_LOCATION AND all_vehicles_a7.COL5 IS NOT DISTINCT FROM peer2_public_a6_0.PICKUP_LOCATION AND all_vehicles_a7.COL4 IS NOT DISTINCT FROM peer2_public_a6_0.REQUEST_ID AND all_vehicles_a7.COL3 IS NOT DISTINCT FROM peer2_public_a6_0.SEAT_COUNT AND all_vehicles_a7.COL2 IS NOT DISTINCT FROM peer2_public_a6_0.CURRENT_AREA AND all_vehicles_a7.COL1 IS NOT DISTINCT FROM peer2_public_a6_0.VEHICLE_ID AND all_vehicles_a7.COL0 IS NOT DISTINCT FROM 2 ) ) AS Δ_del_peer2_public_a6_0  ) AS Δ_del_peer2_public_extra_alias;

CREATE TEMPORARY TABLE Δ_ins_peer1_public WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2,COL3,COL4,COL5) :: public.peer1_public).* 
            FROM (SELECT DISTINCT Δ_ins_peer1_public_a6_0.COL0 AS COL0, Δ_ins_peer1_public_a6_0.COL1 AS COL1, Δ_ins_peer1_public_a6_0.COL2 AS COL2, Δ_ins_peer1_public_a6_0.COL3 AS COL3, Δ_ins_peer1_public_a6_0.COL4 AS COL4, Δ_ins_peer1_public_a6_0.COL5 AS COL5 
FROM (SELECT DISTINCT all_vehicles_a7_0.COL1 AS COL0, all_vehicles_a7_0.COL2 AS COL1, all_vehicles_a7_0.COL3 AS COL2, all_vehicles_a7_0.COL4 AS COL3, all_vehicles_a7_0.COL5 AS COL4, all_vehicles_a7_0.COL6 AS COL5 
FROM (SELECT DISTINCT __dummy__materialized_all_vehicles_a7_0.COMPANY_ID AS COL0, __dummy__materialized_all_vehicles_a7_0.VEHICLE_ID AS COL1, __dummy__materialized_all_vehicles_a7_0.CURRENT_AREA AS COL2, __dummy__materialized_all_vehicles_a7_0.SEAT_COUNT AS COL3, __dummy__materialized_all_vehicles_a7_0.REQUEST_ID AS COL4, __dummy__materialized_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __dummy__materialized_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM public.__dummy__materialized_all_vehicles AS __dummy__materialized_all_vehicles_a7_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_all_vehicles AS __temp__Δ_del_all_vehicles_a7 
WHERE __temp__Δ_del_all_vehicles_a7.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.DROPOFF_LOCATION AND __temp__Δ_del_all_vehicles_a7.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.PICKUP_LOCATION AND __temp__Δ_del_all_vehicles_a7.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.REQUEST_ID AND __temp__Δ_del_all_vehicles_a7.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.SEAT_COUNT AND __temp__Δ_del_all_vehicles_a7.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.CURRENT_AREA AND __temp__Δ_del_all_vehicles_a7.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.VEHICLE_ID AND __temp__Δ_del_all_vehicles_a7.COMPANY_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.COMPANY_ID )  UNION SELECT DISTINCT __temp__Δ_ins_all_vehicles_a7_0.COMPANY_ID AS COL0, __temp__Δ_ins_all_vehicles_a7_0.VEHICLE_ID AS COL1, __temp__Δ_ins_all_vehicles_a7_0.CURRENT_AREA AS COL2, __temp__Δ_ins_all_vehicles_a7_0.SEAT_COUNT AS COL3, __temp__Δ_ins_all_vehicles_a7_0.REQUEST_ID AS COL4, __temp__Δ_ins_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __temp__Δ_ins_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM __temp__Δ_ins_all_vehicles AS __temp__Δ_ins_all_vehicles_a7_0  ) AS all_vehicles_a7_0 
WHERE all_vehicles_a7_0.COL0 = 1 AND NOT EXISTS ( SELECT * 
FROM public.peer1_public AS peer1_public_a6 
WHERE peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM all_vehicles_a7_0.COL6 AND peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM all_vehicles_a7_0.COL5 AND peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM all_vehicles_a7_0.COL4 AND peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM all_vehicles_a7_0.COL3 AND peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM all_vehicles_a7_0.COL2 AND peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM all_vehicles_a7_0.COL1 ) ) AS Δ_ins_peer1_public_a6_0  ) AS Δ_ins_peer1_public_extra_alias;

CREATE TEMPORARY TABLE Δ_ins_peer2_public WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2,COL3,COL4,COL5) :: public.peer2_public).* 
            FROM (SELECT DISTINCT Δ_ins_peer2_public_a6_0.COL0 AS COL0, Δ_ins_peer2_public_a6_0.COL1 AS COL1, Δ_ins_peer2_public_a6_0.COL2 AS COL2, Δ_ins_peer2_public_a6_0.COL3 AS COL3, Δ_ins_peer2_public_a6_0.COL4 AS COL4, Δ_ins_peer2_public_a6_0.COL5 AS COL5 
FROM (SELECT DISTINCT all_vehicles_a7_0.COL1 AS COL0, all_vehicles_a7_0.COL2 AS COL1, all_vehicles_a7_0.COL3 AS COL2, all_vehicles_a7_0.COL4 AS COL3, all_vehicles_a7_0.COL5 AS COL4, all_vehicles_a7_0.COL6 AS COL5 
FROM (SELECT DISTINCT __dummy__materialized_all_vehicles_a7_0.COMPANY_ID AS COL0, __dummy__materialized_all_vehicles_a7_0.VEHICLE_ID AS COL1, __dummy__materialized_all_vehicles_a7_0.CURRENT_AREA AS COL2, __dummy__materialized_all_vehicles_a7_0.SEAT_COUNT AS COL3, __dummy__materialized_all_vehicles_a7_0.REQUEST_ID AS COL4, __dummy__materialized_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __dummy__materialized_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM public.__dummy__materialized_all_vehicles AS __dummy__materialized_all_vehicles_a7_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_all_vehicles AS __temp__Δ_del_all_vehicles_a7 
WHERE __temp__Δ_del_all_vehicles_a7.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.DROPOFF_LOCATION AND __temp__Δ_del_all_vehicles_a7.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.PICKUP_LOCATION AND __temp__Δ_del_all_vehicles_a7.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.REQUEST_ID AND __temp__Δ_del_all_vehicles_a7.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.SEAT_COUNT AND __temp__Δ_del_all_vehicles_a7.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.CURRENT_AREA AND __temp__Δ_del_all_vehicles_a7.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.VEHICLE_ID AND __temp__Δ_del_all_vehicles_a7.COMPANY_ID IS NOT DISTINCT FROM __dummy__materialized_all_vehicles_a7_0.COMPANY_ID )  UNION SELECT DISTINCT __temp__Δ_ins_all_vehicles_a7_0.COMPANY_ID AS COL0, __temp__Δ_ins_all_vehicles_a7_0.VEHICLE_ID AS COL1, __temp__Δ_ins_all_vehicles_a7_0.CURRENT_AREA AS COL2, __temp__Δ_ins_all_vehicles_a7_0.SEAT_COUNT AS COL3, __temp__Δ_ins_all_vehicles_a7_0.REQUEST_ID AS COL4, __temp__Δ_ins_all_vehicles_a7_0.PICKUP_LOCATION AS COL5, __temp__Δ_ins_all_vehicles_a7_0.DROPOFF_LOCATION AS COL6 
FROM __temp__Δ_ins_all_vehicles AS __temp__Δ_ins_all_vehicles_a7_0  ) AS all_vehicles_a7_0 
WHERE all_vehicles_a7_0.COL0 = 2 AND NOT EXISTS ( SELECT * 
FROM public.peer2_public AS peer2_public_a6 
WHERE peer2_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM all_vehicles_a7_0.COL6 AND peer2_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM all_vehicles_a7_0.COL5 AND peer2_public_a6.REQUEST_ID IS NOT DISTINCT FROM all_vehicles_a7_0.COL4 AND peer2_public_a6.SEAT_COUNT IS NOT DISTINCT FROM all_vehicles_a7_0.COL3 AND peer2_public_a6.CURRENT_AREA IS NOT DISTINCT FROM all_vehicles_a7_0.COL2 AND peer2_public_a6.VEHICLE_ID IS NOT DISTINCT FROM all_vehicles_a7_0.COL1 ) ) AS Δ_ins_peer2_public_a6_0  ) AS Δ_ins_peer2_public_extra_alias; 

FOR temprecΔ_del_peer1_public IN ( SELECT * FROM Δ_del_peer1_public) LOOP 
            DELETE FROM public.peer1_public WHERE ROW(VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) IS NOT DISTINCT FROM  temprecΔ_del_peer1_public;
            END LOOP;
DROP TABLE Δ_del_peer1_public;

FOR temprecΔ_del_peer2_public IN ( SELECT * FROM Δ_del_peer2_public) LOOP 
            DELETE FROM public.peer2_public WHERE ROW(VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) IS NOT DISTINCT FROM  temprecΔ_del_peer2_public;
            END LOOP;
DROP TABLE Δ_del_peer2_public;

INSERT INTO public.peer1_public SELECT * FROM  Δ_ins_peer1_public; 
DROP TABLE Δ_ins_peer1_public;

INSERT INTO public.peer2_public SELECT * FROM  Δ_ins_peer2_public; 
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
        -- RAISE NOTICE 'execute procedure all_vehicles_materialization';
        REFRESH MATERIALIZED VIEW public.__dummy__materialized_all_vehicles;
        CREATE TEMPORARY TABLE __temp__Δ_ins_all_vehicles ( LIKE public.__dummy__materialized_all_vehicles INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__all_vehicles_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_ins_all_vehicles DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.all_vehicles_delta_action();

        CREATE TEMPORARY TABLE __temp__Δ_del_all_vehicles ( LIKE public.__dummy__materialized_all_vehicles INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
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
    -- RAISE NOTICE 'execute procedure all_vehicles_update';
    IF TG_OP = 'INSERT' THEN
      -- raise notice 'NEW: %', NEW;
      DELETE FROM __temp__Δ_del_all_vehicles WHERE ROW(COMPANY_ID,VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) IS NOT DISTINCT FROM NEW;
      INSERT INTO __temp__Δ_ins_all_vehicles SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM __temp__Δ_ins_all_vehicles WHERE ROW(COMPANY_ID,VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) IS NOT DISTINCT FROM OLD;
      INSERT INTO __temp__Δ_del_all_vehicles SELECT (OLD).*;
      DELETE FROM __temp__Δ_del_all_vehicles WHERE ROW(COMPANY_ID,VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) IS NOT DISTINCT FROM NEW;
      INSERT INTO __temp__Δ_ins_all_vehicles SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      -- raise notice 'OLD: %', OLD;
      DELETE FROM __temp__Δ_ins_all_vehicles WHERE ROW(COMPANY_ID,VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) IS NOT DISTINCT FROM OLD;
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

