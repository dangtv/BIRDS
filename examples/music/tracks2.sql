/*_____get datalog program_______
?- tracks2(TRACK,RATING,ALBUM,QUANTITY).

tracks2(TRACKS2_A4_TRACK,TRACKS2_A4_RATING,TRACKS2_A4_ALBUM,TRACKS2_A4_QUANTITY) :- tracks2_med(TRACKS2_A4_TRACK,TRACKS2_A4_RATING,TRACKS2_A4_ALBUM,TRACKS2_A4_QUANTITY) , not __dummy__delta__insert__tracks1(TRACKS2_A4_TRACK,_,TRACKS2_A4_RATING,TRACKS2_A4_ALBUM,TRACKS2_A4_QUANTITY).

__dummy__delta__insert__tracks1(TRACK,2018,RATING,ALBUM,QUANTITY) :- tracks2_med(TRACK,RATING,ALBUM,QUANTITY) , not tracks1(TRACK,_,RATING,ALBUM,QUANTITY).

tracks2_med(TRACK,RATING,ALBUM,QUANTITY) :- tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY).

______________*/

CREATE OR REPLACE VIEW public.tracks2 AS 
SELECT __dummy__.col0 AS TRACK,__dummy__.col1 AS RATING,__dummy__.col2 AS ALBUM,__dummy__.col3 AS QUANTITY 
FROM (SELECT tracks2_a4_0.col0 AS col0, tracks2_a4_0.col1 AS col1, tracks2_a4_0.col2 AS col2, tracks2_a4_0.col3 AS col3 
FROM (SELECT tracks2_med_a4_0.col0 AS col0, tracks2_med_a4_0.col1 AS col1, tracks2_med_a4_0.col2 AS col2, tracks2_med_a4_0.col3 AS col3 
FROM (SELECT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.RATING AS col1, tracks1_a5_0.ALBUM AS col2, tracks1_a5_0.QUANTITY AS col3 
FROM public.tracks1 AS tracks1_a5_0  ) AS tracks2_med_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT tracks2_med_a4_0.col0 AS col0, 2018 AS col1, tracks2_med_a4_0.col1 AS col2, tracks2_med_a4_0.col2 AS col3, tracks2_med_a4_0.col3 AS col4 
FROM (SELECT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.RATING AS col1, tracks1_a5_0.ALBUM AS col2, tracks1_a5_0.QUANTITY AS col3 
FROM public.tracks1 AS tracks1_a5_0  ) AS tracks2_med_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_med_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks2_med_a4_0.col2 AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_med_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_med_a4_0.col0 ) ) AS __dummy__delta__insert__tracks1_a5 
WHERE __dummy__delta__insert__tracks1_a5.col4 IS NOT DISTINCT FROM tracks2_med_a4_0.col3 AND __dummy__delta__insert__tracks1_a5.col3 IS NOT DISTINCT FROM tracks2_med_a4_0.col2 AND __dummy__delta__insert__tracks1_a5.col2 IS NOT DISTINCT FROM tracks2_med_a4_0.col1 AND __dummy__delta__insert__tracks1_a5.col0 IS NOT DISTINCT FROM tracks2_med_a4_0.col0 ) ) AS tracks2_a4_0  ) AS __dummy__;

CREATE OR REPLACE FUNCTION public.tracks2_procedure()
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
    CREATE TEMPORARY TABLE __temp__tracks2 WITH OIDS ON COMMIT DROP AS SELECT * FROM public.tracks2;
    IF TG_OP = 'INSERT' THEN
      INSERT INTO __temp__tracks2 SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM __temp__tracks2 WHERE (TRACK,RATING,ALBUM,QUANTITY) = OLD;
      INSERT INTO __temp__tracks2 SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      DELETE FROM __temp__tracks2 WHERE (TRACK,RATING,ALBUM,QUANTITY) = OLD;
    END IF;
    CREATE TEMPORARY TABLE __dummy__delta__delete__tracks1 WITH OIDS ON COMMIT DROP AS SELECT __dummy__delta__delete__tracks1_a5_0.col0 AS col0, __dummy__delta__delete__tracks1_a5_0.col1 AS col1, __dummy__delta__delete__tracks1_a5_0.col2 AS col2, __dummy__delta__delete__tracks1_a5_0.col3 AS col3, __dummy__delta__delete__tracks1_a5_0.col4 AS col4 
FROM (SELECT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) AND NOT EXISTS ( SELECT * 
FROM (SELECT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS __dummy__delta__delete__tracks1_a5_0  ;

CREATE TEMPORARY TABLE __dummy__delta__insert__tracks1 WITH OIDS ON COMMIT DROP AS SELECT __dummy__delta__insert__tracks1_a5_0.col0 AS col0, __dummy__delta__insert__tracks1_a5_0.col1 AS col1, __dummy__delta__insert__tracks1_a5_0.col2 AS col2, __dummy__delta__insert__tracks1_a5_0.col3 AS col3, __dummy__delta__insert__tracks1_a5_0.col4 AS col4 
FROM (SELECT tracks2_a4_0.col0 AS col0, 2018 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks2_a4_0.col2 AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS __dummy__delta__insert__tracks1_a5_0  ; 

 FOR temprec IN ( SELECT * FROM __dummy__delta__delete__tracks1) LOOP 
        DELETE FROM public.tracks1 WHERE (TRACK,DATE,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM  (temprec.col0,temprec.col1,temprec.col2,temprec.col3,temprec.col4);
        END LOOP;

INSERT INTO public.tracks1 SELECT * FROM __dummy__delta__insert__tracks1;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to base tables of public.tracks2';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.tracks2 ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
  
$$;
DROP TRIGGER IF EXISTS tracks2_trigger ON public.tracks2;
CREATE TRIGGER tracks2_trigger
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.tracks2 FOR EACH ROW EXECUTE PROCEDURE public.tracks2_procedure();

