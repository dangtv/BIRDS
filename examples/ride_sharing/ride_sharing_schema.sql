CREATE TABLE area
(
  original_node int NOT NULL PRIMARY KEY,
  area_node int NOT NULL
);

CREATE TABLE shortest
(
  source int NOT NULL,
  destination int NOT NULL,
  path_distance int NOT NULL,
  path_time interval NOT NULL,
  PRIMARY KEY(source, destination)
);

CREATE TABLE peer1_vehicle
(
  vehicle_id int NOT NULL PRIMARY KEY,
  current_location int NOT NULL,
  seat_count int NOT NULL,
  request_id int DEFAULT NULL,
  pickup_location int DEFAULT NULL,
  dropoff_location int DEFAULT NULL
);

CREATE TABLE peer2_vehicle
(
  vehicle_id int NOT NULL PRIMARY KEY,
  current_location int NOT NULL,
  seat_count int NOT NULL,
  request_id int DEFAULT NULL,
  pickup_location int DEFAULT NULL,
  dropoff_location int DEFAULT NULL
);

-- geographic data
INSERT INTO area
VALUES
  (0, 0), (50, 0), (100, 1), (200, 2), (300, 3), (400, 4), (500, 5), (600, 6), (700, 7), (800, 8);

INSERT INTO shortest
VALUES
(0, 5, 120, '00:24:00'),
(1, 5, 50, '00:10:00'),
(2, 5, 20, '00:04:00'),
(3, 5, 80, '00:16:00'),
(4, 5, 130, '00:26:00'),
(5, 5, 130, '00:00:00'),
(7, 5, 80, '00:16:00'),
(8, 5, 100, '00:20:00'),
(6, 5, 70, '00:14:00'),
(5, 8, 100, '00:20:00'),
(5, 7, 80, '00:16:00');


-- local source 1
INSERT INTO peer1_vehicle VALUES (1, 200, 0, 3, 200, 300);
INSERT INTO peer1_vehicle VALUES (2, 50, 4, NULL, NULL, NULL);
INSERT INTO peer1_vehicle VALUES (3, 100, 4, NULL, NULL, NULL);
INSERT INTO peer1_vehicle VALUES (4, 300, 4, NULL, NULL, NULL);
INSERT INTO peer1_vehicle VALUES (5, 500, 4, NULL, NULL, NULL);
INSERT INTO peer1_vehicle VALUES (6, 600, 4, NULL, NULL, NULL);
INSERT INTO peer1_vehicle VALUES (7, 400, 4, NULL, NULL, NULL);

-- local source 2
INSERT INTO peer2_vehicle VALUES (1, 600, 0, 5, 600, 700);
INSERT INTO peer2_vehicle VALUES (3, 300, 4, NULL, NULL, NULL);
INSERT INTO peer2_vehicle VALUES (10, 400, 4, NULL, NULL, NULL);
INSERT INTO peer2_vehicle VALUES (2, 800, 4, NULL, NULL, NULL);
INSERT INTO peer2_vehicle VALUES (11, 100, 4, NULL, NULL, NULL);
INSERT INTO peer2_vehicle VALUES (12, 50, 4, NULL, NULL, NULL);
INSERT INTO peer2_vehicle VALUES (13, 700, 4, NULL, NULL, NULL);
INSERT INTO peer2_vehicle VALUES (23, 200, 4, NULL, NULL, NULL);

