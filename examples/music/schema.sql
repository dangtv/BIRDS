CREATE TABLE tracks1
(
  track VARCHAR NOT NULL PRIMARY KEY,
  date int,
  rating int,
  album VARCHAR,
  quantity int NOT NULL
);

INSERT INTO tracks1 values
(Trust       , 2018 ,      5  ,  Wish     ,       5),
( Lovesong    , 2018 ,      5 , Galore   ,       1),
 (Mysong      , 2018 ,      5 , Galore   ,        1),
 (Lullaby     ,2018 ,    3 , Show    ,      3)

-- CREATE TABLE tracks2
-- (
--   track VARCHAR NOT NULL PRIMARY KEY,
--   rating int,
--   album VARCHAR,
--   quantity int NOT NULL
-- );

-- CREATE TABLE tracks3
-- (
--   track VARCHAR NOT NULL PRIMARY KEY,
--   rating int,
--   album VARCHAR,
--   quantity int NOT NULL
-- );