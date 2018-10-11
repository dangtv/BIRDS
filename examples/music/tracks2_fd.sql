/*_____get datalog program_______
?- tracks2(TRACK,RATING,ALBUM,QUANTITY).

tracks2(TRACKS2_A4_TRACK,TRACKS2_A4_RATING,TRACKS2_A4_ALBUM,TRACKS2_A4_QUANTITY) :- tracks2_med(TRACKS2_A4_TRACK,TRACKS2_A4_RATING,TRACKS2_A4_ALBUM,TRACKS2_A4_QUANTITY) , not init_ins_tracks1(TRACKS2_A4_TRACK,_,TRACKS2_A4_RATING,TRACKS2_A4_ALBUM,TRACKS2_A4_QUANTITY).

tracks2_med(TRACK,RATING,ALBUM,QUANTITY) :- tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY).

init_ins_tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY) :- tracks2_med(TRACK,RATING,ALBUM,QUANTITY) , not tracks1(TRACK,_,RATING,ALBUM,QUANTITY) , tracks1(TRACK,DATE,_,ALBUM,_).

init_ins_tracks1(TRACK,0,RATING,ALBUM,QUANTITY) :- tracks2_med(TRACK,RATING,ALBUM,QUANTITY) , not tracks1(TRACK,_,_,_,_).

______________*/

CREATE OR REPLACE VIEW public.tracks2 AS 
SELECT __dummy__.col0 AS TRACK,__dummy__.col1 AS RATING,__dummy__.col2 AS ALBUM,__dummy__.col3 AS QUANTITY 
FROM (SELECT DISTINCT tracks2_a4_0.col0 AS col0, tracks2_a4_0.col1 AS col1, tracks2_a4_0.col2 AS col2, tracks2_a4_0.col3 AS col3 
FROM (SELECT DISTINCT tracks2_med_a4_0.col0 AS col0, tracks2_med_a4_0.col1 AS col1, tracks2_med_a4_0.col2 AS col2, tracks2_med_a4_0.col3 AS col3 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.RATING AS col1, tracks1_a5_0.ALBUM AS col2, tracks1_a5_0.QUANTITY AS col3 
FROM public.tracks1 AS tracks1_a5_0  ) AS tracks2_med_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_med_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_med_a4_0.col3 AS col4 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.RATING AS col1, tracks1_a5_0.ALBUM AS col2, tracks1_a5_0.QUANTITY AS col3 
FROM public.tracks1 AS tracks1_a5_0  ) AS tracks2_med_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_med_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_med_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_med_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_med_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_med_a4_0.col0 AS col0, 0 AS col1, tracks2_med_a4_0.col1 AS col2, tracks2_med_a4_0.col2 AS col3, tracks2_med_a4_0.col3 AS col4 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.RATING AS col1, tracks1_a5_0.ALBUM AS col2, tracks1_a5_0.QUANTITY AS col3 
FROM public.tracks1 AS tracks1_a5_0  ) AS tracks2_med_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_med_a4_0.col0 ) ) AS init_ins_tracks1_a5 
WHERE init_ins_tracks1_a5.col4 IS NOT DISTINCT FROM tracks2_med_a4_0.col3 AND init_ins_tracks1_a5.col3 IS NOT DISTINCT FROM tracks2_med_a4_0.col2 AND init_ins_tracks1_a5.col2 IS NOT DISTINCT FROM tracks2_med_a4_0.col1 AND init_ins_tracks1_a5.col0 IS NOT DISTINCT FROM tracks2_med_a4_0.col0 ) ) AS tracks2_a4_0  ) AS __dummy__;

CREATE OR REPLACE FUNCTION public.tracks2_delta_action()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  temprec__dummy__delta__delete__tracks1 public.tracks1%ROWTYPE;
temprec__dummy__delta__insert__tracks1 public.tracks1%ROWTYPE;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = 'tracks2_delta_action_flag') THEN
        -- RAISE NOTICE 'execute procedure tracks2_delta_action';
        CREATE TEMPORARY TABLE tracks2_delta_action_flag ON COMMIT DROP AS (SELECT true as finish);
        CREATE TEMPORARY TABLE __dummy__delta__delete__tracks1 WITH OIDS ON COMMIT DROP AS SELECT ((col0,col1,col2,col3,col4) :: public.tracks1).* 
            FROM (SELECT DISTINCT __dummy__delta__delete__tracks1_a5_0.col0 AS col0, __dummy__delta__delete__tracks1_a5_0.col1 AS col1, __dummy__delta__delete__tracks1_a5_0.col2 AS col2, __dummy__delta__delete__tracks1_a5_0.col3 AS col3, __dummy__delta__delete__tracks1_a5_0.col4 AS col4 
FROM (SELECT DISTINCT init_del_tracks1_a5_0.col0 AS col0, init_del_tracks1_a5_0.col1 AS col1, init_del_tracks1_a5_0.col2 AS col2, init_del_tracks1_a5_0.col3 AS col3, init_del_tracks1_a5_0.col4 AS col4 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS init_del_tracks1_a5_0   UNION SELECT DISTINCT fd_del_tracks1_a5_0.col0 AS col0, fd_del_tracks1_a5_0.col1 AS col1, fd_del_tracks1_a5_0.col2 AS col2, fd_del_tracks1_a5_0.col3 AS col3, fd_del_tracks1_a5_0.col4 AS col4 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, init_ins_tracks1_a5_1.col3 AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1 
WHERE init_ins_tracks1_a5_1.col3 = tracks1_a5_0.ALBUM AND tracks1_a5_0.QUANTITY  IS DISTINCT FROM  init_ins_tracks1_a5_1.col4 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS init_del_tracks1_a5 
WHERE init_del_tracks1_a5.col4 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND init_del_tracks1_a5.col3 IS NOT DISTINCT FROM init_ins_tracks1_a5_1.col3 AND init_del_tracks1_a5.col2 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND init_del_tracks1_a5.col1 IS NOT DISTINCT FROM tracks1_a5_0.DATE AND init_del_tracks1_a5.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK )  UNION SELECT DISTINCT init_ins_tracks1_a5_1.col0 AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1 
WHERE init_ins_tracks1_a5_1.col0 = tracks1_a5_0.TRACK AND tracks1_a5_0.RATING  IS DISTINCT FROM  init_ins_tracks1_a5_1.col2 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS init_del_tracks1_a5 
WHERE init_del_tracks1_a5.col4 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND init_del_tracks1_a5.col3 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND init_del_tracks1_a5.col2 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND init_del_tracks1_a5.col1 IS NOT DISTINCT FROM tracks1_a5_0.DATE AND init_del_tracks1_a5.col0 IS NOT DISTINCT FROM init_ins_tracks1_a5_1.col0 )  UNION SELECT DISTINCT init_ins_tracks1_a5_1.col0 AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1 
WHERE init_ins_tracks1_a5_1.col0 = tracks1_a5_0.TRACK AND tracks1_a5_0.DATE  IS DISTINCT FROM  init_ins_tracks1_a5_1.col1 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS init_del_tracks1_a5 
WHERE init_del_tracks1_a5.col4 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND init_del_tracks1_a5.col3 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND init_del_tracks1_a5.col2 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND init_del_tracks1_a5.col1 IS NOT DISTINCT FROM tracks1_a5_0.DATE AND init_del_tracks1_a5.col0 IS NOT DISTINCT FROM init_ins_tracks1_a5_1.col0 ) ) AS fd_del_tracks1_a5_0  ) AS __dummy__delta__delete__tracks1_a5_0  ) AS __dummy__delta__delete__tracks1_extra_alias;

CREATE TEMPORARY TABLE __dummy__delta__insert__tracks1 WITH OIDS ON COMMIT DROP AS SELECT ((col0,col1,col2,col3,col4) :: public.tracks1).* 
            FROM (SELECT DISTINCT __dummy__delta__insert__tracks1_a5_0.col0 AS col0, __dummy__delta__insert__tracks1_a5_0.col1 AS col1, __dummy__delta__insert__tracks1_a5_0.col2 AS col2, __dummy__delta__insert__tracks1_a5_0.col3 AS col3, __dummy__delta__insert__tracks1_a5_0.col4 AS col4 
FROM (SELECT DISTINCT init_ins_tracks1_a5_0.col0 AS col0, init_ins_tracks1_a5_0.col1 AS col1, init_ins_tracks1_a5_0.col2 AS col2, init_ins_tracks1_a5_0.col3 AS col3, init_ins_tracks1_a5_0.col4 AS col4 
FROM (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_0   UNION SELECT DISTINCT fd_ins_tracks1_a5_0.col0 AS col0, fd_ins_tracks1_a5_0.col1 AS col1, fd_ins_tracks1_a5_0.col2 AS col2, fd_ins_tracks1_a5_0.col3 AS col3, fd_ins_tracks1_a5_0.col4 AS col4 
FROM (SELECT DISTINCT init_ins_tracks1_a5_1.col0 AS col0, init_ins_tracks1_a5_1.col1 AS col1, init_ins_tracks1_a5_1.col2 AS col2, init_ins_tracks1_a5_2.col3 AS col3, init_ins_tracks1_a5_2.col4 AS col4 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, init_ins_tracks1_a5_1.col3 AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1 
WHERE init_ins_tracks1_a5_1.col3 = tracks1_a5_0.ALBUM AND tracks1_a5_0.QUANTITY  IS DISTINCT FROM  init_ins_tracks1_a5_1.col4 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS init_del_tracks1_a5 
WHERE init_del_tracks1_a5.col4 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND init_del_tracks1_a5.col3 IS NOT DISTINCT FROM init_ins_tracks1_a5_1.col3 AND init_del_tracks1_a5.col2 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND init_del_tracks1_a5.col1 IS NOT DISTINCT FROM tracks1_a5_0.DATE AND init_del_tracks1_a5.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK )  UNION SELECT DISTINCT init_ins_tracks1_a5_1.col0 AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1 
WHERE init_ins_tracks1_a5_1.col0 = tracks1_a5_0.TRACK AND tracks1_a5_0.RATING  IS DISTINCT FROM  init_ins_tracks1_a5_1.col2 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS init_del_tracks1_a5 
WHERE init_del_tracks1_a5.col4 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND init_del_tracks1_a5.col3 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND init_del_tracks1_a5.col2 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND init_del_tracks1_a5.col1 IS NOT DISTINCT FROM tracks1_a5_0.DATE AND init_del_tracks1_a5.col0 IS NOT DISTINCT FROM init_ins_tracks1_a5_1.col0 )  UNION SELECT DISTINCT init_ins_tracks1_a5_1.col0 AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1 
WHERE init_ins_tracks1_a5_1.col0 = tracks1_a5_0.TRACK AND tracks1_a5_0.DATE  IS DISTINCT FROM  init_ins_tracks1_a5_1.col1 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS init_del_tracks1_a5 
WHERE init_del_tracks1_a5.col4 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND init_del_tracks1_a5.col3 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND init_del_tracks1_a5.col2 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND init_del_tracks1_a5.col1 IS NOT DISTINCT FROM tracks1_a5_0.DATE AND init_del_tracks1_a5.col0 IS NOT DISTINCT FROM init_ins_tracks1_a5_1.col0 ) ) AS fd_del_tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_2 
WHERE init_ins_tracks1_a5_2.col3 = fd_del_tracks1_a5_0.col3 AND init_ins_tracks1_a5_1.col0 = fd_del_tracks1_a5_0.col0  UNION SELECT DISTINCT fd_del_tracks1_a5_0.col0 AS col0, fd_del_tracks1_a5_0.col1 AS col1, fd_del_tracks1_a5_0.col2 AS col2, fd_del_tracks1_a5_0.col3 AS col3, init_ins_tracks1_a5_1.col4 AS col4 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, init_ins_tracks1_a5_1.col3 AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1 
WHERE init_ins_tracks1_a5_1.col3 = tracks1_a5_0.ALBUM AND tracks1_a5_0.QUANTITY  IS DISTINCT FROM  init_ins_tracks1_a5_1.col4 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS init_del_tracks1_a5 
WHERE init_del_tracks1_a5.col4 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND init_del_tracks1_a5.col3 IS NOT DISTINCT FROM init_ins_tracks1_a5_1.col3 AND init_del_tracks1_a5.col2 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND init_del_tracks1_a5.col1 IS NOT DISTINCT FROM tracks1_a5_0.DATE AND init_del_tracks1_a5.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK )  UNION SELECT DISTINCT init_ins_tracks1_a5_1.col0 AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1 
WHERE init_ins_tracks1_a5_1.col0 = tracks1_a5_0.TRACK AND tracks1_a5_0.RATING  IS DISTINCT FROM  init_ins_tracks1_a5_1.col2 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS init_del_tracks1_a5 
WHERE init_del_tracks1_a5.col4 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND init_del_tracks1_a5.col3 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND init_del_tracks1_a5.col2 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND init_del_tracks1_a5.col1 IS NOT DISTINCT FROM tracks1_a5_0.DATE AND init_del_tracks1_a5.col0 IS NOT DISTINCT FROM init_ins_tracks1_a5_1.col0 )  UNION SELECT DISTINCT init_ins_tracks1_a5_1.col0 AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1 
WHERE init_ins_tracks1_a5_1.col0 = tracks1_a5_0.TRACK AND tracks1_a5_0.DATE  IS DISTINCT FROM  init_ins_tracks1_a5_1.col1 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS init_del_tracks1_a5 
WHERE init_del_tracks1_a5.col4 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND init_del_tracks1_a5.col3 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND init_del_tracks1_a5.col2 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND init_del_tracks1_a5.col1 IS NOT DISTINCT FROM tracks1_a5_0.DATE AND init_del_tracks1_a5.col0 IS NOT DISTINCT FROM init_ins_tracks1_a5_1.col0 ) ) AS fd_del_tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1 
WHERE fd_del_tracks1_a5_0.col3 = init_ins_tracks1_a5_1.col3 AND fd_del_tracks1_a5_0.col0  IS DISTINCT FROM  init_ins_tracks1_a5_1.col0 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5 
WHERE init_ins_tracks1_a5.col0 IS NOT DISTINCT FROM fd_del_tracks1_a5_0.col0 )  UNION SELECT DISTINCT init_ins_tracks1_a5_1.col0 AS col0, init_ins_tracks1_a5_1.col1 AS col1, init_ins_tracks1_a5_1.col2 AS col2, fd_del_tracks1_a5_0.col3 AS col3, fd_del_tracks1_a5_0.col4 AS col4 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, init_ins_tracks1_a5_1.col3 AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1 
WHERE init_ins_tracks1_a5_1.col3 = tracks1_a5_0.ALBUM AND tracks1_a5_0.QUANTITY  IS DISTINCT FROM  init_ins_tracks1_a5_1.col4 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS init_del_tracks1_a5 
WHERE init_del_tracks1_a5.col4 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND init_del_tracks1_a5.col3 IS NOT DISTINCT FROM init_ins_tracks1_a5_1.col3 AND init_del_tracks1_a5.col2 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND init_del_tracks1_a5.col1 IS NOT DISTINCT FROM tracks1_a5_0.DATE AND init_del_tracks1_a5.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK )  UNION SELECT DISTINCT init_ins_tracks1_a5_1.col0 AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1 
WHERE init_ins_tracks1_a5_1.col0 = tracks1_a5_0.TRACK AND tracks1_a5_0.RATING  IS DISTINCT FROM  init_ins_tracks1_a5_1.col2 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS init_del_tracks1_a5 
WHERE init_del_tracks1_a5.col4 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND init_del_tracks1_a5.col3 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND init_del_tracks1_a5.col2 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND init_del_tracks1_a5.col1 IS NOT DISTINCT FROM tracks1_a5_0.DATE AND init_del_tracks1_a5.col0 IS NOT DISTINCT FROM init_ins_tracks1_a5_1.col0 )  UNION SELECT DISTINCT init_ins_tracks1_a5_1.col0 AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1 
WHERE init_ins_tracks1_a5_1.col0 = tracks1_a5_0.TRACK AND tracks1_a5_0.DATE  IS DISTINCT FROM  init_ins_tracks1_a5_1.col1 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.DATE AS col1, tracks1_a5_0.RATING AS col2, tracks1_a5_0.ALBUM AS col3, tracks1_a5_0.QUANTITY AS col4 
FROM public.tracks1 AS tracks1_a5_0 
WHERE NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4 
WHERE tracks2_a4.col3 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND tracks2_a4.col2 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND tracks2_a4.col1 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND tracks2_a4.col0 IS NOT DISTINCT FROM tracks1_a5_0.TRACK ) ) AS init_del_tracks1_a5 
WHERE init_del_tracks1_a5.col4 IS NOT DISTINCT FROM tracks1_a5_0.QUANTITY AND init_del_tracks1_a5.col3 IS NOT DISTINCT FROM tracks1_a5_0.ALBUM AND init_del_tracks1_a5.col2 IS NOT DISTINCT FROM tracks1_a5_0.RATING AND init_del_tracks1_a5.col1 IS NOT DISTINCT FROM tracks1_a5_0.DATE AND init_del_tracks1_a5.col0 IS NOT DISTINCT FROM init_ins_tracks1_a5_1.col0 ) ) AS fd_del_tracks1_a5_0, (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5_1 
WHERE init_ins_tracks1_a5_1.col0 = fd_del_tracks1_a5_0.col0 AND init_ins_tracks1_a5_1.col3  IS DISTINCT FROM  fd_del_tracks1_a5_0.col3 AND NOT EXISTS ( SELECT * 
FROM (SELECT DISTINCT tracks1_a5_1.TRACK AS col0, tracks1_a5_1.DATE AS col1, tracks2_a4_0.col1 AS col2, tracks1_a5_1.ALBUM AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0, public.tracks1 AS tracks1_a5_1 
WHERE tracks1_a5_1.ALBUM = tracks2_a4_0.col2 AND tracks1_a5_1.TRACK = tracks2_a4_0.col0 AND NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.QUANTITY IS NOT DISTINCT FROM tracks2_a4_0.col3 AND tracks1_a5.ALBUM IS NOT DISTINCT FROM tracks1_a5_1.ALBUM AND tracks1_a5.RATING IS NOT DISTINCT FROM tracks2_a4_0.col1 AND tracks1_a5.TRACK IS NOT DISTINCT FROM tracks1_a5_1.TRACK )  UNION SELECT DISTINCT tracks2_a4_0.col0 AS col0, 0 AS col1, tracks2_a4_0.col1 AS col2, tracks2_a4_0.col2 AS col3, tracks2_a4_0.col3 AS col4 
FROM (SELECT DISTINCT __temp__tracks2_a4_0.TRACK AS col0, __temp__tracks2_a4_0.RATING AS col1, __temp__tracks2_a4_0.ALBUM AS col2, __temp__tracks2_a4_0.QUANTITY AS col3 
FROM __temp__tracks2 AS __temp__tracks2_a4_0  ) AS tracks2_a4_0 
WHERE NOT EXISTS ( SELECT * 
FROM public.tracks1 AS tracks1_a5 
WHERE tracks1_a5.TRACK IS NOT DISTINCT FROM tracks2_a4_0.col0 ) ) AS init_ins_tracks1_a5 
WHERE init_ins_tracks1_a5.col3 IS NOT DISTINCT FROM fd_del_tracks1_a5_0.col3 ) ) AS fd_ins_tracks1_a5_0  ) AS __dummy__delta__insert__tracks1_a5_0  ) AS __dummy__delta__insert__tracks1_extra_alias; 

FOR temprec__dummy__delta__delete__tracks1 IN ( SELECT * FROM __dummy__delta__delete__tracks1) LOOP 
            DELETE FROM public.tracks1 WHERE (TRACK,DATE,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM  temprec__dummy__delta__delete__tracks1;
            END LOOP;
DROP TABLE __dummy__delta__delete__tracks1;

INSERT INTO public.tracks1 SELECT * FROM  __dummy__delta__insert__tracks1; 
DROP TABLE __dummy__delta__insert__tracks1;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.tracks2';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.tracks2 ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

CREATE OR REPLACE FUNCTION public.tracks2_materialization()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    IF NOT EXISTS (SELECT * FROM information_schema.tables WHERE table_name = '__temp__tracks2')
    THEN
        -- RAISE NOTICE 'execute procedure tracks2_materialization';
        CREATE TEMPORARY TABLE __temp__tracks2 WITH OIDS ON COMMIT DROP AS SELECT * FROM public.tracks2;
        CREATE CONSTRAINT TRIGGER __temp__peer1_public_trigger_delta_action
        AFTER INSERT OR UPDATE OR DELETE ON 
            __temp__tracks2 DEFERRABLE INITIALLY DEFERRED 
            FOR EACH ROW EXECUTE PROCEDURE public.tracks2_delta_action();
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.tracks2';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.tracks2 ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS tracks2_trigger_materialization ON public.tracks2;
CREATE TRIGGER tracks2_trigger_materialization
    BEFORE INSERT OR UPDATE OR DELETE ON
      public.tracks2 FOR EACH STATEMENT EXECUTE PROCEDURE public.tracks2_materialization();

CREATE OR REPLACE FUNCTION public.tracks2_update()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  BEGIN
    -- RAISE NOTICE 'execute procedure tracks2_update';
    IF TG_OP = 'INSERT' THEN
      INSERT INTO __temp__tracks2 SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM __temp__tracks2 WHERE (TRACK,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM OLD;
      INSERT INTO __temp__tracks2 SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      DELETE FROM __temp__tracks2 WHERE (TRACK,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM OLD;
    END IF;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or delete or update to source relations of public.tracks2';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.tracks2 ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
$$;

DROP TRIGGER IF EXISTS tracks2_trigger_update ON public.tracks2;
CREATE TRIGGER tracks2_trigger_update
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.tracks2 FOR EACH ROW EXECUTE PROCEDURE public.tracks2_update();

