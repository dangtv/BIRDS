/*_____get datalog program_______
?- peer_public(VEHICLE_ID,AREA,REQUEST_ID).

peer_public(PEER_PUBLIC_A3_VEHICLE_ID,PEER_PUBLIC_A3_AREA,PEER_PUBLIC_A3_REQUEST_ID) :- peer_public_med(PEER_PUBLIC_A3_VEHICLE_ID,PEER_PUBLIC_A3_AREA,PEER_PUBLIC_A3_REQUEST_ID) , not __dummy__delta__insert__vehicle(PEER_PUBLIC_A3_VEHICLE_ID,_,PEER_PUBLIC_A3_REQUEST_ID).

peer_public_med(V,A,R) :- vehicle(V,L,R) , area_map(L,A).

__dummy__delta__insert__vehicle(V,L,R) :- peer_public_med(V,A,R) , vehicle(V,L,_) , area_map(L,A) , not vehicle(V,L,R).

______________*/

CREATE OR REPLACE VIEW public.peer_public AS SELECT __dummy__.col0 AS VEHICLE_ID,__dummy__.col1 AS AREA,__dummy__.col2 AS REQUEST_ID FROM (SELECT peer_public_a3_0.col0 AS col0, peer_public_a3_0.col1 AS col1, peer_public_a3_0.col2 AS col2 FROM (SELECT peer_public_med_a3_0.col0 AS col0, peer_public_med_a3_0.col1 AS col1, peer_public_med_a3_0.col2 AS col2 FROM (SELECT vehicle_a3_0.VEHICLE_ID AS col0, area_map_a2_1.AREA AS col1, vehicle_a3_0.REQUEST_ID AS col2 FROM public.vehicle AS vehicle_a3_0, public.area_map AS area_map_a2_1 WHERE area_map_a2_1.LOCATION = vehicle_a3_0.LOCATION ) AS peer_public_med_a3_0 WHERE NOT EXISTS ( SELECT * FROM (SELECT vehicle_a3_1.VEHICLE_ID AS col0, area_map_a2_2.LOCATION AS col1, peer_public_med_a3_0.col2 AS col2 FROM (SELECT vehicle_a3_0.VEHICLE_ID AS col0, area_map_a2_1.AREA AS col1, vehicle_a3_0.REQUEST_ID AS col2 FROM public.vehicle AS vehicle_a3_0, public.area_map AS area_map_a2_1 WHERE area_map_a2_1.LOCATION = vehicle_a3_0.LOCATION ) AS peer_public_med_a3_0, public.vehicle AS vehicle_a3_1, public.area_map AS area_map_a2_2 WHERE vehicle_a3_1.VEHICLE_ID = peer_public_med_a3_0.col0 AND area_map_a2_2.LOCATION = vehicle_a3_1.LOCATION AND area_map_a2_2.AREA = peer_public_med_a3_0.col1 AND NOT EXISTS ( SELECT * FROM public.vehicle AS vehicle_a3 WHERE vehicle_a3.REQUEST_ID IS NOT DISTINCT FROM peer_public_med_a3_0.col2 AND vehicle_a3.LOCATION IS NOT DISTINCT FROM area_map_a2_2.LOCATION AND vehicle_a3.VEHICLE_ID IS NOT DISTINCT FROM vehicle_a3_1.VEHICLE_ID ) ) AS __dummy__delta__insert__vehicle_a3 WHERE __dummy__delta__insert__vehicle_a3.col2 IS NOT DISTINCT FROM peer_public_med_a3_0.col2 AND __dummy__delta__insert__vehicle_a3.col0 IS NOT DISTINCT FROM peer_public_med_a3_0.col0 ) ) AS peer_public_a3_0  ) AS __dummy__;

CREATE OR REPLACE FUNCTION public.peer_public_procedure()
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
    CREATE TEMPORARY TABLE __temp__peer_public WITH OIDS ON COMMIT DROP AS SELECT * FROM public.peer_public;
    IF TG_OP = 'INSERT' THEN
      INSERT INTO __temp__peer_public SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM __temp__peer_public WHERE (VEHICLE_ID,AREA,REQUEST_ID) = OLD;
      INSERT INTO __temp__peer_public SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      DELETE FROM __temp__peer_public WHERE (VEHICLE_ID,AREA,REQUEST_ID) = OLD;
    END IF;
    CREATE TEMPORARY TABLE __dummy__delta__delete__vehicle WITH OIDS ON COMMIT DROP AS SELECT __dummy__delta__delete__vehicle_a3_0.col0 AS col0, __dummy__delta__delete__vehicle_a3_0.col1 AS col1, __dummy__delta__delete__vehicle_a3_0.col2 AS col2 FROM (SELECT vehicle_a3_0.VEHICLE_ID AS col0, area_map_a2_1.LOCATION AS col1, vehicle_a3_0.REQUEST_ID AS col2 FROM public.vehicle AS vehicle_a3_0, public.area_map AS area_map_a2_1 WHERE area_map_a2_1.LOCATION = vehicle_a3_0.LOCATION AND NOT EXISTS ( SELECT * FROM (SELECT __temp__peer_public_a3_0.VEHICLE_ID AS col0, __temp__peer_public_a3_0.AREA AS col1, __temp__peer_public_a3_0.REQUEST_ID AS col2 FROM __temp__peer_public AS __temp__peer_public_a3_0  ) AS peer_public_a3 WHERE peer_public_a3.col2 IS NOT DISTINCT FROM vehicle_a3_0.REQUEST_ID AND peer_public_a3.col1 IS NOT DISTINCT FROM area_map_a2_1.AREA AND peer_public_a3.col0 IS NOT DISTINCT FROM vehicle_a3_0.VEHICLE_ID ) ) AS __dummy__delta__delete__vehicle_a3_0  ;
CREATE TEMPORARY TABLE __dummy__delta__insert__vehicle WITH OIDS ON COMMIT DROP AS SELECT __dummy__delta__insert__vehicle_a3_0.col0 AS col0, __dummy__delta__insert__vehicle_a3_0.col1 AS col1, __dummy__delta__insert__vehicle_a3_0.col2 AS col2 FROM (SELECT vehicle_a3_1.VEHICLE_ID AS col0, area_map_a2_2.LOCATION AS col1, peer_public_a3_0.col2 AS col2 FROM (SELECT __temp__peer_public_a3_0.VEHICLE_ID AS col0, __temp__peer_public_a3_0.AREA AS col1, __temp__peer_public_a3_0.REQUEST_ID AS col2 FROM __temp__peer_public AS __temp__peer_public_a3_0  ) AS peer_public_a3_0, public.vehicle AS vehicle_a3_1, public.area_map AS area_map_a2_2 WHERE vehicle_a3_1.VEHICLE_ID = peer_public_a3_0.col0 AND area_map_a2_2.LOCATION = vehicle_a3_1.LOCATION AND area_map_a2_2.AREA = peer_public_a3_0.col1 AND NOT EXISTS ( SELECT * FROM public.vehicle AS vehicle_a3 WHERE vehicle_a3.REQUEST_ID IS NOT DISTINCT FROM peer_public_a3_0.col2 AND vehicle_a3.LOCATION IS NOT DISTINCT FROM area_map_a2_2.LOCATION AND vehicle_a3.VEHICLE_ID IS NOT DISTINCT FROM vehicle_a3_1.VEHICLE_ID ) ) AS __dummy__delta__insert__vehicle_a3_0  ; 
 FOR temprec IN ( SELECT * FROM __dummy__delta__delete__vehicle) LOOP 
        DELETE FROM public.vehicle WHERE (VEHICLE_ID,LOCATION,REQUEST_ID) IS NOT DISTINCT FROM  (temprec.col0,temprec.col1,temprec.col2);
        END LOOP;
INSERT INTO public.vehicle SELECT * FROM __dummy__delta__insert__vehicle;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or update or delete from public.peer_public';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.peer_public ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
  
$$;
DROP TRIGGER IF EXISTS peer_public_trigger ON public.peer_public;
CREATE TRIGGER peer_public_trigger
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.peer_public FOR EACH ROW EXECUTE PROCEDURE public.peer_public_procedure();

