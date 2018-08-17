/*_____get datalog program_______
?- v(X).

v(V_A1_X) :- v_med(V_A1_X) , not __dummy__delta__insert__s1(V_A1_X).

v_med(X) :- s1(X).

v_med(X) :- s2(X).

__dummy__delta__insert__s1(X) :- v_med(X) , not s1(X) , not s2(X).

______________*/

CREATE OR REPLACE VIEW public.v AS SELECT __dummy__.col0 AS X FROM (SELECT v_a1_0.col0 AS col0 FROM (SELECT v_med_a1_0.col0 AS col0 FROM (SELECT s1_a1_0.X AS col0 FROM public.s1 AS s1_a1_0   UNION ALL SELECT s2_a1_0.X AS col0 FROM public.s2 AS s2_a1_0  ) AS v_med_a1_0 WHERE NOT EXISTS ( SELECT * FROM (SELECT v_med_a1_0.col0 AS col0 FROM (SELECT s1_a1_0.X AS col0 FROM public.s1 AS s1_a1_0   UNION ALL SELECT s2_a1_0.X AS col0 FROM public.s2 AS s2_a1_0  ) AS v_med_a1_0 WHERE NOT EXISTS ( SELECT * FROM public.s1 AS s1_a1 WHERE s1_a1.X IS NOT DISTINCT FROM v_med_a1_0.col0 ) AND NOT EXISTS ( SELECT * FROM public.s2 AS s2_a1 WHERE s2_a1.X IS NOT DISTINCT FROM v_med_a1_0.col0 ) ) AS __dummy__delta__insert__s1_a1 WHERE __dummy__delta__insert__s1_a1.col0 IS NOT DISTINCT FROM v_med_a1_0.col0 ) ) AS v_a1_0  ) AS __dummy__;

CREATE OR REPLACE FUNCTION public.v_procedure()
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

    CREATE TEMPORARY TABLE __temp__v WITH OIDS ON COMMIT DROP AS SELECT * FROM public.v;
    IF TG_OP = 'INSERT' THEN
      INSERT INTO __temp__v SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM __temp__v WHERE (X) = OLD;
      INSERT INTO __temp__v SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      DELETE FROM __temp__v WHERE (X) = OLD;
    END IF;

    CREATE TEMPORARY TABLE __dummy__delta__delete__s1 WITH OIDS ON COMMIT DROP AS 
    SELECT __dummy__delta__delete__s1_a1_0.col0 AS col0 FROM (SELECT s1_a1_0.X AS col0 FROM public.s1 AS s1_a1_0 WHERE NOT EXISTS ( SELECT * FROM (SELECT __temp__v_a1_0.X AS col0 FROM __temp__v AS __temp__v_a1_0  ) AS v_a1 WHERE v_a1.col0 IS NOT DISTINCT FROM s1_a1_0.X ) ) AS __dummy__delta__delete__s1_a1_0  ;

CREATE TEMPORARY TABLE __dummy__delta__delete__s2 WITH OIDS ON COMMIT DROP AS SELECT __dummy__delta__delete__s2_a1_0.col0 AS col0 FROM (SELECT s2_a1_0.X AS col0 FROM public.s2 AS s2_a1_0 WHERE NOT EXISTS ( SELECT * FROM (SELECT __temp__v_a1_0.X AS col0 FROM __temp__v AS __temp__v_a1_0  ) AS v_a1 WHERE v_a1.col0 IS NOT DISTINCT FROM s2_a1_0.X ) ) AS __dummy__delta__delete__s2_a1_0  ;

CREATE TEMPORARY TABLE __dummy__delta__insert__s1 WITH OIDS ON COMMIT DROP AS SELECT __dummy__delta__insert__s1_a1_0.col0 AS col0 FROM (SELECT v_a1_0.col0 AS col0 FROM (SELECT __temp__v_a1_0.X AS col0 FROM __temp__v AS __temp__v_a1_0  ) AS v_a1_0 WHERE NOT EXISTS ( SELECT * FROM public.s1 AS s1_a1 WHERE s1_a1.X IS NOT DISTINCT FROM v_a1_0.col0 ) AND NOT EXISTS ( SELECT * FROM public.s2 AS s2_a1 WHERE s2_a1.X IS NOT DISTINCT FROM v_a1_0.col0 ) ) AS __dummy__delta__insert__s1_a1_0  ; 


 FOR temprec IN ( SELECT * FROM __dummy__delta__delete__s1) LOOP 
        DELETE FROM public.s1 WHERE (X) IS NOT DISTINCT FROM  (temprec.col0);
        END LOOP;

 FOR temprec IN ( SELECT * FROM __dummy__delta__delete__s2) LOOP 
        DELETE FROM public.s2 WHERE (X) IS NOT DISTINCT FROM  (temprec.col0);
        END LOOP;

INSERT INTO public.s1 SELECT * FROM __dummy__delta__insert__s1;
    RETURN NULL;

  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert to local db';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'something is wrong;; error code: ' || text_var1 || ' ;; ' || text_var2 ||' ;; ' || text_var3;
        RETURN NULL;
  END;
  
$$;
DROP TRIGGER IF EXISTS v_trigger ON public.v;
CREATE TRIGGER v_trigger
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.v FOR EACH ROW EXECUTE PROCEDURE public.v_procedure();

