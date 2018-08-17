CREATE TABLE albums
(
  album VARCHAR NOT NULL PRIMARY KEY,
  quantity int NOT NULL
);

CREATE TABLE tracks
(
  track VARCHAR NOT NULL PRIMARY KEY,
  date int,
  rating int,
  album VARCHAR
);

CREATE TABLE tracks1
(
  track VARCHAR NOT NULL PRIMARY KEY,
  date int,
  rating int,
  album VARCHAR,
  quantity int NOT NULL
);

CREATE TABLE tracks2
(
  track VARCHAR NOT NULL PRIMARY KEY,
  rating int,
  album VARCHAR,
  quantity int NOT NULL
);

CREATE TABLE tracks3
(
  track VARCHAR NOT NULL PRIMARY KEY,
  rating int,
  album VARCHAR,
  quantity int NOT NULL
);