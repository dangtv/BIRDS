/*_____get datalog program_______
?- peer2_public(VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION).

peer2_public(PEER2_PUBLIC_A6_VEHICLE_ID,PEER2_PUBLIC_A6_CURRENT_AREA,PEER2_PUBLIC_A6_SEAT_COUNT,PEER2_PUBLIC_A6_REQUEST_ID,PEER2_PUBLIC_A6_PICKUP_LOCATION,PEER2_PUBLIC_A6_DROPOFF_LOCATION) :- peer2_public_med(PEER2_PUBLIC_A6_VEHICLE_ID,PEER2_PUBLIC_A6_CURRENT_AREA,PEER2_PUBLIC_A6_SEAT_COUNT,PEER2_PUBLIC_A6_REQUEST_ID,PEER2_PUBLIC_A6_PICKUP_LOCATION,PEER2_PUBLIC_A6_DROPOFF_LOCATION) , not __dummy__delta__insert__peer2_vehicle(PEER2_PUBLIC_A6_VEHICLE_ID,_,PEER2_PUBLIC_A6_SEAT_COUNT,PEER2_PUBLIC_A6_REQUEST_ID,PEER2_PUBLIC_A6_PICKUP_LOCATION,PEER2_PUBLIC_A6_DROPOFF_LOCATION).

peer2_public_med(V,A,S,R,P,D) :- peer2_vehicle(V,L,S,R,P,D) , area(L,A).

__dummy__delta__insert__peer2_vehicle(V,L,S,R,P,D) :- peer2_public_med(V,A,S,R,P,D) , peer2_vehicle(V,L,_,_,_,_) , area(L,A) , not peer2_vehicle(V,L,S,R,P,D).

______________*/

CREATE OR REPLACE VIEW public.peer2_public AS 
SELECT __dummy__.col0 AS VEHICLE_ID,__dummy__.col1 AS CURRENT_AREA,__dummy__.col2 AS SEAT_COUNT,__dummy__.col3 AS REQUEST_ID,__dummy__.col4 AS PICKUP_LOCATION,__dummy__.col5 AS DROPOFF_LOCATION 
FROM (SELECT peer2_public_a6_0.col0 AS col0, peer2_public_a6_0.col1 AS col1, peer2_public_a6_0.col2 AS col2, peer2_public_a6_0.col3 AS col3, peer2_public_a6_0.col4 AS col4, peer2_public_a6_0.col5 AS col5 
FROM (SELECT peer2_public_med_a6_0.col0 AS col0, peer2_public_med_a6_0.col1 AS col1, peer2_public_med_a6_0.col2 AS col2, peer2_public_med_a6_0.col3 AS col3, peer2_public_med_a6_0.col4 AS col4, peer2_public_med_a6_0.col5 AS col5 
FROM (SELECT peer2_vehicle_a6_0.VEHICLE_ID AS col0, area_a2_1.AREA_NODE AS col1, peer2_vehicle_a6_0.SEAT_COUNT AS col2, peer2_vehicle_a6_0.REQUEST_ID AS col3, peer2_vehicle_a6_0.PICKUP_LOCATION AS col4, peer2_vehicle_a6_0.DROPOFF_LOCATION AS col5 
FROM public.peer2_vehicle AS peer2_vehicle_a6_0, public.area AS area_a2_1 
WHERE area_a2_1.ORIGINAL_NODE = peer2_vehicle_a6_0.CURRENT_LOCATION ) AS peer2_public_med_a6_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT peer2_vehicle_a6_1.VEHICLE_ID AS col0, area_a2_2.ORIGINAL_NODE AS col1, peer2_public_med_a6_0.col2 AS col2, peer2_public_med_a6_0.col3 AS col3, peer2_public_med_a6_0.col4 AS col4, peer2_public_med_a6_0.col5 AS col5 
FROM (SELECT peer2_vehicle_a6_0.VEHICLE_ID AS col0, area_a2_1.AREA_NODE AS col1, peer2_vehicle_a6_0.SEAT_COUNT AS col2, peer2_vehicle_a6_0.REQUEST_ID AS col3, peer2_vehicle_a6_0.PICKUP_LOCATION AS col4, peer2_vehicle_a6_0.DROPOFF_LOCATION AS col5 
FROM public.peer2_vehicle AS peer2_vehicle_a6_0, public.area AS area_a2_1 
WHERE area_a2_1.ORIGINAL_NODE = peer2_vehicle_a6_0.CURRENT_LOCATION ) AS peer2_public_med_a6_0, public.peer2_vehicle AS peer2_vehicle_a6_1, public.area AS area_a2_2 
WHERE peer2_vehicle_a6_1.VEHICLE_ID = peer2_public_med_a6_0.col0 AND area_a2_2.ORIGINAL_NODE = peer2_vehicle_a6_1.CURRENT_LOCATION AND area_a2_2.AREA_NODE = peer2_public_med_a6_0.col1 AND NOT EXISTS ( SELECT * 
FROM public.peer2_vehicle AS peer2_vehicle_a6 
WHERE peer2_vehicle_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM peer2_public_med_a6_0.col5 AND peer2_vehicle_a6.PICKUP_LOCATION IS NOT DISTINCT FROM peer2_public_med_a6_0.col4 AND peer2_vehicle_a6.REQUEST_ID IS NOT DISTINCT FROM peer2_public_med_a6_0.col3 AND peer2_vehicle_a6.SEAT_COUNT IS NOT DISTINCT FROM peer2_public_med_a6_0.col2 AND peer2_vehicle_a6.CURRENT_LOCATION IS NOT DISTINCT FROM area_a2_2.ORIGINAL_NODE AND peer2_vehicle_a6.VEHICLE_ID IS NOT DISTINCT FROM peer2_vehicle_a6_1.VEHICLE_ID ) ) AS __dummy__delta__insert__peer2_vehicle_a6 
WHERE __dummy__delta__insert__peer2_vehicle_a6.col5 IS NOT DISTINCT FROM peer2_public_med_a6_0.col5 AND __dummy__delta__insert__peer2_vehicle_a6.col4 IS NOT DISTINCT FROM peer2_public_med_a6_0.col4 AND __dummy__delta__insert__peer2_vehicle_a6.col3 IS NOT DISTINCT FROM peer2_public_med_a6_0.col3 AND __dummy__delta__insert__peer2_vehicle_a6.col2 IS NOT DISTINCT FROM peer2_public_med_a6_0.col2 AND __dummy__delta__insert__peer2_vehicle_a6.col0 IS NOT DISTINCT FROM peer2_public_med_a6_0.col0 ) ) AS peer2_public_a6_0  ) AS __dummy__;

CREATE OR REPLACE FUNCTION public.peer2_public_procedure()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  temprec record;
  BEGIN
    CREATE TEMPORARY TABLE __temp__peer2_public WITH OIDS ON COMMIT DROP AS SELECT * FROM public.peer2_public;
    IF TG_OP = 'INSERT' THEN
      INSERT INTO __temp__peer2_public SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM __temp__peer2_public WHERE (VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) = OLD;
      INSERT INTO __temp__peer2_public SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      DELETE FROM __temp__peer2_public WHERE (VEHICLE_ID,CURRENT_AREA,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) = OLD;
    END IF;
    CREATE TEMPORARY TABLE __dummy__delta__delete__peer2_vehicle WITH OIDS ON COMMIT DROP AS SELECT __dummy__delta__delete__peer2_vehicle_a6_0.col0 AS col0, __dummy__delta__delete__peer2_vehicle_a6_0.col1 AS col1, __dummy__delta__delete__peer2_vehicle_a6_0.col2 AS col2, __dummy__delta__delete__peer2_vehicle_a6_0.col3 AS col3, __dummy__delta__delete__peer2_vehicle_a6_0.col4 AS col4, __dummy__delta__delete__peer2_vehicle_a6_0.col5 AS col5 
FROM (SELECT peer2_vehicle_a6_0.VEHICLE_ID AS col0, area_a2_1.ORIGINAL_NODE AS col1, peer2_vehicle_a6_0.SEAT_COUNT AS col2, peer2_vehicle_a6_0.REQUEST_ID AS col3, peer2_vehicle_a6_0.PICKUP_LOCATION AS col4, peer2_vehicle_a6_0.DROPOFF_LOCATION AS col5 
FROM public.peer2_vehicle AS peer2_vehicle_a6_0, public.area AS area_a2_1 
WHERE area_a2_1.ORIGINAL_NODE = peer2_vehicle_a6_0.CURRENT_LOCATION AND NOT EXISTS ( SELECT * 
FROM (SELECT __temp__peer2_public_a6_0.VEHICLE_ID AS col0, __temp__peer2_public_a6_0.CURRENT_AREA AS col1, __temp__peer2_public_a6_0.SEAT_COUNT AS col2, __temp__peer2_public_a6_0.REQUEST_ID AS col3, __temp__peer2_public_a6_0.PICKUP_LOCATION AS col4, __temp__peer2_public_a6_0.DROPOFF_LOCATION AS col5 
FROM __temp__peer2_public AS __temp__peer2_public_a6_0  ) AS peer2_public_a6 
WHERE peer2_public_a6.col5 IS NOT DISTINCT FROM peer2_vehicle_a6_0.DROPOFF_LOCATION AND peer2_public_a6.col4 IS NOT DISTINCT FROM peer2_vehicle_a6_0.PICKUP_LOCATION AND peer2_public_a6.col3 IS NOT DISTINCT FROM peer2_vehicle_a6_0.REQUEST_ID AND peer2_public_a6.col2 IS NOT DISTINCT FROM peer2_vehicle_a6_0.SEAT_COUNT AND peer2_public_a6.col1 IS NOT DISTINCT FROM area_a2_1.AREA_NODE AND peer2_public_a6.col0 IS NOT DISTINCT FROM peer2_vehicle_a6_0.VEHICLE_ID ) ) AS __dummy__delta__delete__peer2_vehicle_a6_0  ;

CREATE TEMPORARY TABLE __dummy__delta__insert__peer2_vehicle WITH OIDS ON COMMIT DROP AS SELECT __dummy__delta__insert__peer2_vehicle_a6_0.col0 AS col0, __dummy__delta__insert__peer2_vehicle_a6_0.col1 AS col1, __dummy__delta__insert__peer2_vehicle_a6_0.col2 AS col2, __dummy__delta__insert__peer2_vehicle_a6_0.col3 AS col3, __dummy__delta__insert__peer2_vehicle_a6_0.col4 AS col4, __dummy__delta__insert__peer2_vehicle_a6_0.col5 AS col5 
FROM (SELECT peer2_vehicle_a6_1.VEHICLE_ID AS col0, area_a2_2.ORIGINAL_NODE AS col1, peer2_public_a6_0.col2 AS col2, peer2_public_a6_0.col3 AS col3, peer2_public_a6_0.col4 AS col4, peer2_public_a6_0.col5 AS col5 
FROM (SELECT __temp__peer2_public_a6_0.VEHICLE_ID AS col0, __temp__peer2_public_a6_0.CURRENT_AREA AS col1, __temp__peer2_public_a6_0.SEAT_COUNT AS col2, __temp__peer2_public_a6_0.REQUEST_ID AS col3, __temp__peer2_public_a6_0.PICKUP_LOCATION AS col4, __temp__peer2_public_a6_0.DROPOFF_LOCATION AS col5 
FROM __temp__peer2_public AS __temp__peer2_public_a6_0  ) AS peer2_public_a6_0, public.peer2_vehicle AS peer2_vehicle_a6_1, public.area AS area_a2_2 
WHERE peer2_vehicle_a6_1.VEHICLE_ID = peer2_public_a6_0.col0 AND area_a2_2.ORIGINAL_NODE = peer2_vehicle_a6_1.CURRENT_LOCATION AND area_a2_2.AREA_NODE = peer2_public_a6_0.col1 AND NOT EXISTS ( SELECT * 
FROM public.peer2_vehicle AS peer2_vehicle_a6 
WHERE peer2_vehicle_a6.DROPOFF_LOCATION IS NOT DISTINCT FROM peer2_public_a6_0.col5 AND peer2_vehicle_a6.PICKUP_LOCATION IS NOT DISTINCT FROM peer2_public_a6_0.col4 AND peer2_vehicle_a6.REQUEST_ID IS NOT DISTINCT FROM peer2_public_a6_0.col3 AND peer2_vehicle_a6.SEAT_COUNT IS NOT DISTINCT FROM peer2_public_a6_0.col2 AND peer2_vehicle_a6.CURRENT_LOCATION IS NOT DISTINCT FROM area_a2_2.ORIGINAL_NODE AND peer2_vehicle_a6.VEHICLE_ID IS NOT DISTINCT FROM peer2_vehicle_a6_1.VEHICLE_ID ) ) AS __dummy__delta__insert__peer2_vehicle_a6_0  ; 

 FOR temprec IN ( SELECT * FROM __dummy__delta__delete__peer2_vehicle) LOOP 
        DELETE FROM public.peer2_vehicle WHERE (VEHICLE_ID,CURRENT_LOCATION,SEAT_COUNT,REQUEST_ID,PICKUP_LOCATION,DROPOFF_LOCATION) IS NOT DISTINCT FROM  (temprec.col0,temprec.col1,temprec.col2,temprec.col3,temprec.col4,temprec.col5);
        END LOOP;

INSERT INTO public.peer2_vehicle SELECT * FROM __dummy__delta__insert__peer2_vehicle;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to base tables of public.peer2_public';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.peer2_public ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
  
$$;
DROP TRIGGER IF EXISTS peer2_public_trigger ON public.peer2_public;
CREATE TRIGGER peer2_public_trigger
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.peer2_public FOR EACH ROW EXECUTE PROCEDURE public.peer2_public_procedure();

