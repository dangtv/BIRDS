
CREATE OR REPLACE FUNCTION public.edm_delta_action()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  temprec__dummy__delta__delete__dm public.dm%ROWTYPE;
temprec__dummy__delta__delete__ed public.ed%ROWTYPE;
temprec__dummy__delta__insert__dm public.dm%ROWTYPE;
temprec__dummy__delta__insert__ed public.ed%ROWTYPE;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'edm_delta_action_flag') THEN
        -- RAISE NOTICE 'execute procedure edm_delta_action';
        CREATE TEMPORARY TABLE edm_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        CREATE TEMPORARY TABLE __dummy__delta__delete__dm WITH OIDS ON COMMIT DROP AS SELECT ((col0,col1) :: public.dm).* 
            FROM (SELECT DISTINCT __dummy__delta__delete__dm_a2_0.col0 AS col0, __dummy__delta__delete__dm_a2_0.col1 AS col1 
FROM (SELECT DISTINCT edm_keep_a3_1.col1 AS col0, edm_del_a3_0.col2 AS col1 
FROM (SELECT DISTINCT tempv_a3_0.col0 AS col0, tempv_a3_0.col1 AS col1, tempv_a3_0.col2 AS col2 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3 
WHERE edm_a3.col2 IS NOT DISTINCT FROM tempv_a3_0.col2 AND edm_a3.col1 IS NOT DISTINCT FROM tempv_a3_0.col1 AND edm_a3.col0 IS NOT DISTINCT FROM tempv_a3_0.col0 ) ) AS edm_del_a3_0, (SELECT DISTINCT edm_a3_1.col0 AS col0, edm_a3_1.col1 AS col1, edm_a3_1.col2 AS col2 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3_0, (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3_1 
WHERE edm_a3_1.col2 = tempv_a3_0.col2 AND edm_a3_1.col0 = tempv_a3_0.col0 AND edm_a3_1.col1 = tempv_a3_0.col1 ) AS edm_keep_a3_1 
WHERE edm_keep_a3_1.col0 = edm_del_a3_0.col0 AND edm_keep_a3_1.col1 = edm_del_a3_0.col1  UNION SELECT DISTINCT edm_ins_a3_1.col1 AS col0, edm_del_a3_0.col2 AS col1 
FROM (SELECT DISTINCT tempv_a3_0.col0 AS col0, tempv_a3_0.col1 AS col1, tempv_a3_0.col2 AS col2 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3 
WHERE edm_a3.col2 IS NOT DISTINCT FROM tempv_a3_0.col2 AND edm_a3.col1 IS NOT DISTINCT FROM tempv_a3_0.col1 AND edm_a3.col0 IS NOT DISTINCT FROM tempv_a3_0.col0 ) ) AS edm_del_a3_0, (SELECT DISTINCT edm_a3_0.col0 AS col0, edm_a3_0.col1 AS col1, edm_a3_0.col2 AS col2 
FROM (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3 
WHERE tempv_a3.col2 IS NOT DISTINCT FROM edm_a3_0.col2 AND tempv_a3.col1 IS NOT DISTINCT FROM edm_a3_0.col1 AND tempv_a3.col0 IS NOT DISTINCT FROM edm_a3_0.col0 ) ) AS edm_ins_a3_1 
WHERE edm_ins_a3_1.col0 = edm_del_a3_0.col0 AND edm_ins_a3_1.col1 = edm_del_a3_0.col1 ) AS __dummy__delta__delete__dm_a2_0  ) AS __dummy__delta__delete__dm_extra_alias;

CREATE TEMPORARY TABLE __dummy__delta__delete__ed WITH OIDS ON COMMIT DROP AS SELECT ((col0,col1) :: public.ed).* 
            FROM (SELECT DISTINCT __dummy__delta__delete__ed_a2_0.col0 AS col0, __dummy__delta__delete__ed_a2_0.col1 AS col1 
FROM (SELECT DISTINCT edm_del_a3_0.col0 AS col0, edm_keep_a3_1.col1 AS col1 
FROM (SELECT DISTINCT tempv_a3_0.col0 AS col0, tempv_a3_0.col1 AS col1, tempv_a3_0.col2 AS col2 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3 
WHERE edm_a3.col2 IS NOT DISTINCT FROM tempv_a3_0.col2 AND edm_a3.col1 IS NOT DISTINCT FROM tempv_a3_0.col1 AND edm_a3.col0 IS NOT DISTINCT FROM tempv_a3_0.col0 ) ) AS edm_del_a3_0, (SELECT DISTINCT edm_a3_1.col0 AS col0, edm_a3_1.col1 AS col1, edm_a3_1.col2 AS col2 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3_0, (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3_1 
WHERE edm_a3_1.col2 = tempv_a3_0.col2 AND edm_a3_1.col0 = tempv_a3_0.col0 AND edm_a3_1.col1 = tempv_a3_0.col1 ) AS edm_keep_a3_1 
WHERE edm_keep_a3_1.col2 = edm_del_a3_0.col2 AND edm_keep_a3_1.col1 = edm_del_a3_0.col1  UNION SELECT DISTINCT edm_del_a3_0.col0 AS col0, edm_ins_a3_1.col1 AS col1 
FROM (SELECT DISTINCT tempv_a3_0.col0 AS col0, tempv_a3_0.col1 AS col1, tempv_a3_0.col2 AS col2 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3 
WHERE edm_a3.col2 IS NOT DISTINCT FROM tempv_a3_0.col2 AND edm_a3.col1 IS NOT DISTINCT FROM tempv_a3_0.col1 AND edm_a3.col0 IS NOT DISTINCT FROM tempv_a3_0.col0 ) ) AS edm_del_a3_0, (SELECT DISTINCT edm_a3_0.col0 AS col0, edm_a3_0.col1 AS col1, edm_a3_0.col2 AS col2 
FROM (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3 
WHERE tempv_a3.col2 IS NOT DISTINCT FROM edm_a3_0.col2 AND tempv_a3.col1 IS NOT DISTINCT FROM edm_a3_0.col1 AND tempv_a3.col0 IS NOT DISTINCT FROM edm_a3_0.col0 ) ) AS edm_ins_a3_1 
WHERE edm_ins_a3_1.col2 = edm_del_a3_0.col2 AND edm_ins_a3_1.col1 = edm_del_a3_0.col1  UNION SELECT DISTINCT edm_del_a3_0.col0 AS col0, edm_del_a3_0.col1 AS col1 
FROM (SELECT DISTINCT tempv_a3_0.col0 AS col0, tempv_a3_0.col1 AS col1, tempv_a3_0.col2 AS col2 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3 
WHERE edm_a3.col2 IS NOT DISTINCT FROM tempv_a3_0.col2 AND edm_a3.col1 IS NOT DISTINCT FROM tempv_a3_0.col1 AND edm_a3.col0 IS NOT DISTINCT FROM tempv_a3_0.col0 ) ) AS edm_del_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT edm_a3_1.col0 AS col0, edm_a3_1.col1 AS col1, edm_a3_1.col2 AS col2 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3_0, (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3_1 
WHERE edm_a3_1.col2 = tempv_a3_0.col2 AND edm_a3_1.col0 = tempv_a3_0.col0 AND edm_a3_1.col1 = tempv_a3_0.col1 ) AS edm_keep_a3 
WHERE edm_keep_a3.col1 IS NOT DISTINCT FROM edm_del_a3_0.col1 AND edm_keep_a3.col0 IS NOT DISTINCT FROM edm_del_a3_0.col0 ) AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT edm_a3_0.col0 AS col0, edm_a3_0.col1 AS col1, edm_a3_0.col2 AS col2 
FROM (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3 
WHERE tempv_a3.col2 IS NOT DISTINCT FROM edm_a3_0.col2 AND tempv_a3.col1 IS NOT DISTINCT FROM edm_a3_0.col1 AND tempv_a3.col0 IS NOT DISTINCT FROM edm_a3_0.col0 ) ) AS edm_ins_a3 
WHERE edm_ins_a3.col2 IS NOT DISTINCT FROM edm_del_a3_0.col2 AND edm_ins_a3.col1 IS NOT DISTINCT FROM edm_del_a3_0.col1 ) AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT edm_a3_1.col0 AS col0, edm_a3_1.col1 AS col1, edm_a3_1.col2 AS col2 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3_0, (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3_1 
WHERE edm_a3_1.col2 = tempv_a3_0.col2 AND edm_a3_1.col0 = tempv_a3_0.col0 AND edm_a3_1.col1 = tempv_a3_0.col1 ) AS edm_keep_a3 
WHERE edm_keep_a3.col2 IS NOT DISTINCT FROM edm_del_a3_0.col2 AND edm_keep_a3.col1 IS NOT DISTINCT FROM edm_del_a3_0.col1 ) AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT edm_a3_0.col0 AS col0, edm_a3_0.col1 AS col1, edm_a3_0.col2 AS col2 
FROM (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3 
WHERE tempv_a3.col2 IS NOT DISTINCT FROM edm_a3_0.col2 AND tempv_a3.col1 IS NOT DISTINCT FROM edm_a3_0.col1 AND tempv_a3.col0 IS NOT DISTINCT FROM edm_a3_0.col0 ) ) AS edm_ins_a3 
WHERE edm_ins_a3.col1 IS NOT DISTINCT FROM edm_del_a3_0.col1 AND edm_ins_a3.col0 IS NOT DISTINCT FROM edm_del_a3_0.col0 ) ) AS __dummy__delta__delete__ed_a2_0  ) AS __dummy__delta__delete__ed_extra_alias;

CREATE TEMPORARY TABLE __dummy__delta__insert__dm WITH OIDS ON COMMIT DROP AS SELECT ((col0,col1) :: public.dm).* 
            FROM (SELECT DISTINCT __dummy__delta__insert__dm_a2_0.col0 AS col0, __dummy__delta__insert__dm_a2_0.col1 AS col1 
FROM (SELECT DISTINCT edm_ins_a3_0.col1 AS col0, edm_ins_a3_0.col2 AS col1 
FROM (SELECT DISTINCT edm_a3_0.col0 AS col0, edm_a3_0.col1 AS col1, edm_a3_0.col2 AS col2 
FROM (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3 
WHERE tempv_a3.col2 IS NOT DISTINCT FROM edm_a3_0.col2 AND tempv_a3.col1 IS NOT DISTINCT FROM edm_a3_0.col1 AND tempv_a3.col0 IS NOT DISTINCT FROM edm_a3_0.col0 ) ) AS edm_ins_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.dm AS dm_a2 
WHERE dm_a2.MGR IS NOT DISTINCT FROM edm_ins_a3_0.col2 AND dm_a2.DEPT IS NOT DISTINCT FROM edm_ins_a3_0.col1 ) ) AS __dummy__delta__insert__dm_a2_0  ) AS __dummy__delta__insert__dm_extra_alias;

CREATE TEMPORARY TABLE __dummy__delta__insert__ed WITH OIDS ON COMMIT DROP AS SELECT ((col0,col1) :: public.ed).* 
            FROM (SELECT DISTINCT __dummy__delta__insert__ed_a2_0.col0 AS col0, __dummy__delta__insert__ed_a2_0.col1 AS col1 
FROM (SELECT DISTINCT edm_ins_a3_0.col0 AS col0, edm_ins_a3_0.col1 AS col1 
FROM (SELECT DISTINCT edm_a3_0.col0 AS col0, edm_a3_0.col1 AS col1, edm_a3_0.col2 AS col2 
FROM (SELECT DISTINCT __temp__edm_a3_0.ENAME AS col0, __temp__edm_a3_0.DEPT AS col1, __temp__edm_a3_0.MGR AS col2 
FROM __temp__edm AS __temp__edm_a3_0  ) AS edm_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT ed_a2_0.ENAME AS col0, dm_a2_1.DEPT AS col1, dm_a2_1.MGR AS col2 
FROM public.ed AS ed_a2_0, public.dm AS dm_a2_1 
WHERE dm_a2_1.DEPT = ed_a2_0.DEPT ) AS tempv_a3 
WHERE tempv_a3.col2 IS NOT DISTINCT FROM edm_a3_0.col2 AND tempv_a3.col1 IS NOT DISTINCT FROM edm_a3_0.col1 AND tempv_a3.col0 IS NOT DISTINCT FROM edm_a3_0.col0 ) ) AS edm_ins_a3_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.ed AS ed_a2 
WHERE ed_a2.DEPT IS NOT DISTINCT FROM edm_ins_a3_0.col1 AND ed_a2.ENAME IS NOT DISTINCT FROM edm_ins_a3_0.col0 ) ) AS __dummy__delta__insert__ed_a2_0  ) AS __dummy__delta__insert__ed_extra_alias; 

FOR temprec__dummy__delta__delete__dm IN ( SELECT * FROM __dummy__delta__delete__dm) LOOP 
            DELETE FROM public.dm WHERE ROW(DEPT,MGR) IS NOT DISTINCT FROM  temprec__dummy__delta__delete__dm;
            END LOOP;
DROP TABLE __dummy__delta__delete__dm;

FOR temprec__dummy__delta__delete__ed IN ( SELECT * FROM __dummy__delta__delete__ed) LOOP 
            DELETE FROM public.ed WHERE ROW(ENAME,DEPT) IS NOT DISTINCT FROM  temprec__dummy__delta__delete__ed;
            END LOOP;
DROP TABLE __dummy__delta__delete__ed;

INSERT INTO public.dm SELECT * FROM  __dummy__delta__insert__dm; 
DROP TABLE __dummy__delta__insert__dm;

INSERT INTO public.ed SELECT * FROM  __dummy__delta__insert__ed; 
DROP TABLE __dummy__delta__insert__ed;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.edm';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.edm ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

CREATE OR REPLACE FUNCTION public.edm_materialization()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '__temp__edm')
    THEN
        -- RAISE NOTICE 'execute procedure edm_materialization';
        CREATE TEMPORARY TABLE __temp__edm WITH OIDS ON COMMIT DROP AS SELECT * FROM public.edm;
        CREATE CONSTRAINT TRIGGER __temp__peer1_public_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__edm DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.edm_delta_action();
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.edm';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.edm ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS edm_trigger_materialization ON public.edm;
CREATE TRIGGER edm_trigger_materialization
    BEFORE INSERT OR UPDATE OR DELETE ON
      public.edm FOR EACH STATEMENT EXECUTE PROCEDURE public.edm_materialization();

CREATE OR REPLACE FUNCTION public.edm_update()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    -- RAISE NOTICE 'execute procedure edm_update';
    IF TG_OP = 'INSERT' THEN
      INSERT INTO __temp__edm SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM __temp__edm WHERE ROW(ENAME,DEPT,MGR) IS NOT DISTINCT FROM OLD;
      INSERT INTO __temp__edm SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      DELETE FROM __temp__edm WHERE ROW(ENAME,DEPT,MGR) IS NOT DISTINCT FROM OLD;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.edm';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.edm ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS edm_trigger_update ON public.edm;
CREATE TRIGGER edm_trigger_update
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.edm FOR EACH ROW EXECUTE PROCEDURE public.edm_update();

