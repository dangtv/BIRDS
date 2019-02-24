CREATE OR REPLACE VIEW public.peer1_public AS 
SELECT __dummy__.COL0 AS VEHICLE_ID,__dummy__.COL1 AS CURRENT_AREA,__dummy__.COL2 AS SEAT_COUNT,__dummy__.COL3 AS REQUEST_ID,__dummy__.COL4 AS PICKUP_LOCATION,__dummy__.COL5 AS DROPOFF_LOCATION 
FROM (SELECT DISTINCT peer1_public_a6_0.COL0 AS COL0, peer1_public_a6_0.COL1 AS COL1, peer1_public_a6_0.COL2 AS COL2, peer1_public_a6_0.COL3 AS COL3, peer1_public_a6_0.COL4 AS COL4, peer1_public_a6_0.COL5 AS COL5 
FROM (SELECT DISTINCT peer1_public_med_a6_0.COL0 AS COL0, peer1_public_med_a6_0.COL1 AS COL1, peer1_public_med_a6_0.COL2 AS COL2, peer1_public_med_a6_0.COL3 AS COL3, peer1_public_med_a6_0.COL4 AS COL4, peer1_public_med_a6_0.COL5 AS COL5 
FROM (SELECT DISTINCT peer1_vehicle_a6_0.VEHICLE_ID AS COL0, area_a2_1.AREA_NODE AS COL1, peer1_vehicle_a6_0.SEAT_COUNT AS COL2, peer1_vehicle_a6_0.REQUEST_ID AS COL3, peer1_vehicle_a6_0.PICKUP_LOCATION AS COL4, peer1_vehicle_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.peer1_vehicle AS peer1_vehicle_a6_0, public.area AS area_a2_1 
WHERE area_a2_1.ORIGINAL_NODE = peer1_vehicle_a6_0.CURRENT_LOCATION ) AS peer1_public_med_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT peer1_vehicle_a6_1.VEHICLE_ID AS COL0, area_a2_2.ORIGINAL_NODE AS COL1, peer1_public_med_a6_0.COL2 AS COL2, peer1_public_med_a6_0.COL3 AS COL3, peer1_public_med_a6_0.COL4 AS COL4, peer1_public_med_a6_0.COL5 AS COL5 
FROM (SELECT DISTINCT peer1_vehicle_a6_0.VEHICLE_ID AS COL0, area_a2_1.AREA_NODE AS COL1, peer1_vehicle_a6_0.SEAT_COUNT AS COL2, peer1_vehicle_a6_0.REQUEST_ID AS COL3, peer1_vehicle_a6_0.PICKUP_LOCATION AS COL4, peer1_vehicle_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.peer1_vehicle AS peer1_vehicle_a6_0, public.area AS area_a2_1 
WHERE area_a2_1.ORIGINAL_NODE = peer1_vehicle_a6_0.CURRENT_LOCATION ) AS peer1_public_med_a6_0, public.peer1_vehicle AS peer1_vehicle_a6_1, public.area AS area_a2_2 
WHERE area_a2_2.AREA_NODE = peer1_public_med_a6_0.COL1 AND peer1_vehicle_a6_1.VEHICLE_ID = peer1_public_med_a6_0.COL0 AND area_a2_2.ORIGINAL_NODE = peer1_vehicle_a6_1.CURRENT_LOCATION AND NOT EXISTS ( SELECT * 
FROM public.peer1_vehicle AS peer1_vehicle_a6 
WHERE peer1_vehicle_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM peer1_public_med_a6_0.COL5 AND peer1_vehicle_a6.PICKUP_LOCATION IS NOT DISTINCT FROM peer1_public_med_a6_0.COL4 AND peer1_vehicle_a6.REQUEST_ID IS NOT DISTINCT FROM peer1_public_med_a6_0.COL3 AND peer1_vehicle_a6.SEAT_COUNT IS NOT DISTINCT FROM peer1_public_med_a6_0.COL2 AND peer1_vehicle_a6.CURRENT_LOCATION IS NOT DISTINCT FROM area_a2_2.ORIGINAL_NODE AND peer1_vehicle_a6.VEHICLE_ID IS NOT DISTINCT FROM peer1_vehicle_a6_1.VEHICLE_ID ) ) AS _derived_Δ_ins_peer1_vehicle_a6 
WHERE _derived_Δ_ins_peer1_vehicle_a6.COL5 IS NOT DISTINCT FROM peer1_public_med_a6_0.COL5 AND _derived_Δ_ins_peer1_vehicle_a6.COL4 IS NOT DISTINCT FROM peer1_public_med_a6_0.COL4 AND _derived_Δ_ins_peer1_vehicle_a6.COL3 IS NOT DISTINCT FROM peer1_public_med_a6_0.COL3 AND _derived_Δ_ins_peer1_vehicle_a6.COL2 IS NOT DISTINCT FROM peer1_public_med_a6_0.COL2 AND _derived_Δ_ins_peer1_vehicle_a6.COL0 IS NOT DISTINCT FROM peer1_public_med_a6_0.COL0 ) ) AS peer1_public_a6_0  ) AS __dummy__;

DROP MATERIALIZED VIEW IF EXISTS public.__dummy__materialized_peer1_public;

CREATE  MATERIALIZED VIEW public.__dummy__materialized_peer1_public AS 
SELECT * FROM public.peer1_public;

CREATE OR REPLACE FUNCTION public.peer1_public_delta_action()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  temprecΔ_del_peer1_vehicle public.peer1_vehicle%ROWTYPE;
temprecΔ_ins_peer1_vehicle public.peer1_vehicle%ROWTYPE;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'peer1_public_delta_action_flag') THEN
        -- RAISE NOTICE 'execute procedure peer1_public_delta_action';
        CREATE TEMPORARY TABLE peer1_public_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        IF EXISTS (SELECT  
FROM (SELECT  
FROM (SELECT  
FROM (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6_0, (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6_1 
WHERE peer1_public_a6_1.COL0 = peer1_public_a6_0.COL0 AND peer1_public_a6_0.COL1  IS DISTINCT FROM  peer1_public_a6_1.COL1  UNION SELECT  
FROM (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6_0, (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6_1 
WHERE peer1_public_a6_1.COL0 = peer1_public_a6_0.COL0 AND peer1_public_a6_0.COL2  IS DISTINCT FROM  peer1_public_a6_1.COL2  UNION SELECT  
FROM (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6_0, (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6_1 
WHERE peer1_public_a6_1.COL0 = peer1_public_a6_0.COL0 AND peer1_public_a6_0.COL3  IS DISTINCT FROM  peer1_public_a6_1.COL3  UNION SELECT  
FROM (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6_0, (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6_1 
WHERE peer1_public_a6_1.COL0 = peer1_public_a6_0.COL0 AND peer1_public_a6_0.COL4  IS DISTINCT FROM  peer1_public_a6_1.COL4  UNION SELECT  
FROM (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6_0, (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6_1 
WHERE peer1_public_a6_1.COL0 = peer1_public_a6_0.COL0 AND peer1_public_a6_0.COL5  IS DISTINCT FROM  peer1_public_a6_1.COL5  UNION SELECT  
FROM public.peer1_vehicle AS peer1_vehicle_a6_0, public.area AS area_a2_1, (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6_2 
WHERE peer1_public_a6_2.COL0 = peer1_vehicle_a6_0.VEHICLE_ID AND area_a2_1.ORIGINAL_NODE = peer1_vehicle_a6_0.CURRENT_LOCATION AND area_a2_1.AREA_NODE  IS DISTINCT FROM  peer1_public_a6_2.COL1  UNION SELECT  
FROM (SELECT DISTINCT peer1_vehicle_a6_0.VEHICLE_ID AS COL0 
FROM public.peer1_vehicle AS peer1_vehicle_a6_0, public.area AS area_a2_1 
WHERE area_a2_1.ORIGINAL_NODE = peer1_vehicle_a6_0.CURRENT_LOCATION ) AS all_id_a1_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6 
WHERE peer1_public_a6.COL0 IS NOT DISTINCT FROM all_id_a1_0.COL0 )  UNION SELECT  
FROM (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT peer1_vehicle_a6_0.VEHICLE_ID AS COL0 
FROM public.peer1_vehicle AS peer1_vehicle_a6_0, public.area AS area_a2_1 
WHERE area_a2_1.ORIGINAL_NODE = peer1_vehicle_a6_0.CURRENT_LOCATION ) AS all_id_a1 
WHERE all_id_a1.COL0 IS NOT DISTINCT FROM peer1_public_a6_0.COL0 ) ) AS ⊥_a0_0  ) AS __dummy__ )
        THEN 
          RAISE check_violation USING MESSAGE = 'Invalid update on view';
        END IF;
        CREATE TEMPORARY TABLE Δ_del_peer1_vehicle WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2,COL3,COL4,COL5) :: public.peer1_vehicle).* 
            FROM (SELECT DISTINCT Δ_del_peer1_vehicle_a6_0.COL0 AS COL0, Δ_del_peer1_vehicle_a6_0.COL1 AS COL1, Δ_del_peer1_vehicle_a6_0.COL2 AS COL2, Δ_del_peer1_vehicle_a6_0.COL3 AS COL3, Δ_del_peer1_vehicle_a6_0.COL4 AS COL4, Δ_del_peer1_vehicle_a6_0.COL5 AS COL5 
FROM (SELECT DISTINCT peer1_vehicle_a6_0.VEHICLE_ID AS COL0, area_a2_1.ORIGINAL_NODE AS COL1, peer1_vehicle_a6_0.SEAT_COUNT AS COL2, peer1_vehicle_a6_0.REQUEST_ID AS COL3, peer1_vehicle_a6_0.PICKUP_LOCATION AS COL4, peer1_vehicle_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.peer1_vehicle AS peer1_vehicle_a6_0, public.area AS area_a2_1 
WHERE area_a2_1.ORIGINAL_NODE = peer1_vehicle_a6_0.CURRENT_LOCATION AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6 
WHERE peer1_public_a6.COL5 IS NOT DISTINCT FROM peer1_vehicle_a6_0.DROPOFF_LOCATION AND peer1_public_a6.COL4 IS NOT DISTINCT FROM peer1_vehicle_a6_0.PICKUP_LOCATION AND peer1_public_a6.COL3 IS NOT DISTINCT FROM peer1_vehicle_a6_0.REQUEST_ID AND peer1_public_a6.COL2 IS NOT DISTINCT FROM peer1_vehicle_a6_0.SEAT_COUNT AND peer1_public_a6.COL1 IS NOT DISTINCT FROM area_a2_1.AREA_NODE AND peer1_public_a6.COL0 IS NOT DISTINCT FROM peer1_vehicle_a6_0.VEHICLE_ID ) ) AS Δ_del_peer1_vehicle_a6_0  ) AS Δ_del_peer1_vehicle_extra_alias;

CREATE TEMPORARY TABLE Δ_ins_peer1_vehicle WITH OIDS ON COMMIT DROP AS SELECT (ROW(COL0,COL1,COL2,COL3,COL4,COL5) :: public.peer1_vehicle).* 
            FROM (SELECT DISTINCT Δ_ins_peer1_vehicle_a6_0.COL0 AS COL0, Δ_ins_peer1_vehicle_a6_0.COL1 AS COL1, Δ_ins_peer1_vehicle_a6_0.COL2 AS COL2, Δ_ins_peer1_vehicle_a6_0.COL3 AS COL3, Δ_ins_peer1_vehicle_a6_0.COL4 AS COL4, Δ_ins_peer1_vehicle_a6_0.COL5 AS COL5 
FROM (SELECT DISTINCT peer1_vehicle_a6_1.VEHICLE_ID AS COL0, area_a2_2.ORIGINAL_NODE AS COL1, peer1_public_a6_0.COL2 AS COL2, peer1_public_a6_0.COL3 AS COL3, peer1_public_a6_0.COL4 AS COL4, peer1_public_a6_0.COL5 AS COL5 
FROM (SELECT DISTINCT __dummy__materialized_peer1_public_a6_0.VEHICLE_ID AS COL0, __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AS COL1, __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AS COL2, __dummy__materialized_peer1_public_a6_0.REQUEST_ID AS COL3, __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM public.__dummy__materialized_peer1_public AS __dummy__materialized_peer1_public_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM __temp__Δ_del_peer1_public AS __temp__Δ_del_peer1_public_a6 
WHERE __temp__Δ_del_peer1_public_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.DROPOFF_LOCATION AND __temp__Δ_del_peer1_public_a6.PICKUP_LOCATION IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.PICKUP_LOCATION AND __temp__Δ_del_peer1_public_a6.REQUEST_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.REQUEST_ID AND __temp__Δ_del_peer1_public_a6.SEAT_COUNT IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.SEAT_COUNT AND __temp__Δ_del_peer1_public_a6.CURRENT_AREA IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.CURRENT_AREA AND __temp__Δ_del_peer1_public_a6.VEHICLE_ID IS NOT DISTINCT FROM __dummy__materialized_peer1_public_a6_0.VEHICLE_ID )  UNION SELECT DISTINCT __temp__Δ_ins_peer1_public_a6_0.VEHICLE_ID AS COL0, __temp__Δ_ins_peer1_public_a6_0.CURRENT_AREA AS COL1, __temp__Δ_ins_peer1_public_a6_0.SEAT_COUNT AS COL2, __temp__Δ_ins_peer1_public_a6_0.REQUEST_ID AS COL3, __temp__Δ_ins_peer1_public_a6_0.PICKUP_LOCATION AS COL4, __temp__Δ_ins_peer1_public_a6_0.DROPOFF_LOCATION AS COL5 
FROM __temp__Δ_ins_peer1_public AS __temp__Δ_ins_peer1_public_a6_0  ) AS peer1_public_a6_0, public.peer1_vehicle AS peer1_vehicle_a6_1, public.area AS area_a2_2 
WHERE area_a2_2.AREA_NODE = peer1_public_a6_0.COL1 AND peer1_vehicle_a6_1.VEHICLE_ID = peer1_public_a6_0.COL0 AND area_a2_2.ORIGINAL_NODE = peer1_vehicle_a6_1.CURRENT_LOCATION AND NOT EXISTS ( SELECT * 
FROM public.peer1_vehicle AS peer1_vehicle_a6 
WHERE peer1_vehicle_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM peer1_public_a6_0.COL5 AND peer1_vehicle_a6.PICKUP_LOCATION IS NOT DISTINCT FROM peer1_public_a6_0.COL4 AND peer1_vehicle_a6.REQUEST_ID IS NOT DISTINCT FROM peer1_public_a6_0.COL3 AND peer1_vehicle_a6.SEAT_COUNT IS NOT DISTINCT FROM peer1_public_a6_0.COL2 AND peer1_vehicle_a6.CURRENT_LOCATION IS NOT DISTINCT FROM area_a2_2.ORIGINAL_NODE AND peer1_vehicle_a6.VEHICLE_ID IS NOT DISTINCT FROM peer1_vehicle_a6_1.VEHICLE_ID ) ) AS Δ_ins_peer1_vehicle_a6_0  ) AS Δ_ins_peer1_vehicle_extra_alias; 

FOR temprecΔ_del_peer1_vehicle IN ( SELECT * FROM Δ_del_peer1_vehicle) LOOP 
            DELETE FROM public.peer1_vehicle WHERE ROW(VEHICLE_ID,CURRENT_LOCATION,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) IS NOT DISTINCT FROM  temprecΔ_del_peer1_vehicle;
            END LOOP;
DROP TABLE Δ_del_peer1_vehicle;

INSERT INTO public.peer1_vehicle SELECT * FROM  Δ_ins_peer1_vehicle; 
DROP TABLE Δ_ins_peer1_vehicle;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.peer1_public';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.peer1_public ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

CREATE OR REPLACE FUNCTION public.peer1_public_materialization()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '__temp__Δ_ins_peer1_public' OR table_name = '__temp__Δ_del_peer1_public')
    THEN
        -- RAISE NOTICE 'execute procedure peer1_public_materialization';
        REFRESH MATERIALIZED VIEW public.__dummy__materialized_peer1_public;
        CREATE TEMPORARY TABLE __temp__Δ_ins_peer1_public ( LIKE public.__dummy__materialized_peer1_public INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__peer1_public_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_ins_peer1_public DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.peer1_public_delta_action();

        CREATE TEMPORARY TABLE __temp__Δ_del_peer1_public ( LIKE public.__dummy__materialized_peer1_public INCLUDING ALL ) WITH OIDS ON COMMIT DROP;
        CREATE CONSTRAINT TRIGGER __temp__peer1_public_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__Δ_del_peer1_public DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.peer1_public_delta_action();
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.peer1_public';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.peer1_public ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS peer1_public_trigger_materialization ON public.peer1_public;
CREATE TRIGGER peer1_public_trigger_materialization
    BEFORE INSERT OR UPDATE OR DELETE ON
      public.peer1_public FOR EACH STATEMENT EXECUTE PROCEDURE public.peer1_public_materialization();

CREATE OR REPLACE FUNCTION public.peer1_public_update()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    -- RAISE NOTICE 'execute procedure peer1_public_update';
    IF TG_OP = 'INSERT' THEN
      -- raise notice 'NEW: %', NEW;
      DELETE FROM __temp__Δ_del_peer1_public WHERE ROW(VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) IS NOT DISTINCT FROM NEW;
      INSERT INTO __temp__Δ_ins_peer1_public SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM __temp__Δ_ins_peer1_public WHERE ROW(VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) IS NOT DISTINCT FROM OLD;
      INSERT INTO __temp__Δ_del_peer1_public SELECT (OLD).*;
      DELETE FROM __temp__Δ_del_peer1_public WHERE ROW(VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) IS NOT DISTINCT FROM NEW;
      INSERT INTO __temp__Δ_ins_peer1_public SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      -- raise notice 'OLD: %', OLD;
      DELETE FROM __temp__Δ_ins_peer1_public WHERE ROW(VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) IS NOT DISTINCT FROM OLD;
      INSERT INTO __temp__Δ_del_peer1_public SELECT (OLD).*;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.peer1_public';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.peer1_public ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS peer1_public_trigger_update ON public.peer1_public;
CREATE TRIGGER peer1_public_trigger_update
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.peer1_public FOR EACH ROW EXECUTE PROCEDURE public.peer1_public_update();

