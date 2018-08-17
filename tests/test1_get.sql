/*_____get datalog program_______
?- tracks2_prime(TRACK,RATING,ALBUM,QUANTITY).

tracks2_prime(TRACKS2_PRIME_A4_TRACK,TRACKS2_PRIME_A4_RATING,TRACKS2_PRIME_A4_ALBUM,TRACKS2_PRIME_A4_QUANTITY) :- tracks2_prime_med(TRACKS2_PRIME_A4_TRACK,TRACKS2_PRIME_A4_RATING,TRACKS2_PRIME_A4_ALBUM,TRACKS2_PRIME_A4_QUANTITY) , not __dummy__delta__insert__tracks1(TRACKS2_PRIME_A4_TRACK,_,TRACKS2_PRIME_A4_RATING,TRACKS2_PRIME_A4_ALBUM,TRACKS2_PRIME_A4_QUANTITY).

+tracks1(TRACK,2018,RATING,ALBUM,QUANTITY) :- tracks2_prime_med(TRACK,RATING,ALBUM,QUANTITY) , not tracks1(TRACK,_,RATING,ALBUM,QUANTITY).

tracks2_prime_med(TRACK,RATING,ALBUM,QUANTITY) :- tracks1(TRACK,_,RATING,ALBUM,QUANTITY).

______________*/

CREATE OR REPLACE VIEW public.tracks2_prime AS SELECT __dummy__.col0 AS TRACK,__dummy__.col1 AS RATING,__dummy__.col2 AS ALBUM,__dummy__.col3 AS QUANTITY FROM (SELECT tracks2_prime_a4_0.col0 AS col0, tracks2_prime_a4_0.col1 AS col1, tracks2_prime_a4_0.col2 AS col2, tracks2_prime_a4_0.col3 AS col3 FROM (SELECT tracks2_prime_med_a4_0.col0 AS col0, tracks2_prime_med_a4_0.col1 AS col1, tracks2_prime_med_a4_0.col2 AS col2, tracks2_prime_med_a4_0.col3 AS col3 FROM (SELECT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.RATING AS col1, tracks1_a5_0.ALBUM AS col2, tracks1_a5_0.QUANTITY AS col3 FROM public.tracks1 AS tracks1_a5_0  ) AS tracks2_prime_med_a4_0 WHERE NOT EXISTS ( SELECT * FROM (SELECT tracks2_prime_med_a4_0.col0 AS col0, 2018 AS col1, tracks2_prime_med_a4_0.col1 AS col2, tracks2_prime_med_a4_0.col2 AS col3, tracks2_prime_med_a4_0.col3 AS col4 FROM (SELECT tracks1_a5_0.TRACK AS col0, tracks1_a5_0.RATING AS col1, tracks1_a5_0.ALBUM AS col2, tracks1_a5_0.QUANTITY AS col3 FROM public.tracks1 AS tracks1_a5_0  ) AS tracks2_prime_med_a4_0 WHERE NOT EXISTS ( SELECT * FROM tracks1 AS tracks1_a5 WHERE tracks1_a5.QUANTITY = tracks2_prime_med_a4_0.col3 AND tracks1_a5.ALBUM = tracks2_prime_med_a4_0.col2 AND tracks1_a5.RATING = tracks2_prime_med_a4_0.col1 AND tracks1_a5.TRACK = tracks2_prime_med_a4_0.col0 ) ) AS __dummy__delta__insert__tracks1_a5 WHERE __dummy__delta__insert__tracks1_a5.col4 = tracks2_prime_med_a4_0.col3 AND __dummy__delta__insert__tracks1_a5.col3 = tracks2_prime_med_a4_0.col2 AND __dummy__delta__insert__tracks1_a5.col2 = tracks2_prime_med_a4_0.col1 AND __dummy__delta__insert__tracks1_a5.col0 = tracks2_prime_med_a4_0.col0 ) ) AS tracks2_prime_a4_0  ) AS __dummy__;
