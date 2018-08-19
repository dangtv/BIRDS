---
layout: default
---

# BIRDS tutorial: examples on ride-sharing schema

Example: ride-sharing schema ([schema.sql]({{site.github.repository_url}}/tree/master/examples/ride_sharing/schema.sql))

area |
---------------|-----------
 original_node | area_node 
             0 |         0
            50 |         0
           100 |         1
           200 |         2
           300 |         3
           400 |         4
           500 |         5
           600 |         6
           700 |         7
           800 |         8

peer1_vehicle |
------------|------------------|------------|------------|-----------------|------------------
vehicle_id | current_location | seat_count | request_id | pickup_location | dropoff_location 
          1 |              200 |          0 |          3 |             200 |              300
          2 |               50 |          4 |            |                 |                 
          3 |              100 |          4 |            |                 |                 
          4 |              300 |          4 |            |                 |                 
          5 |              500 |          4 |            |                 |                 
          6 |              600 |          4 |            |                 |                 
          7 |              400 |          4 |            |                 |                 

peer2_vehicle |
------------|------------------|------------|------------|-----------------|------------------
 vehicle_id | current_location | seat_count | request_id | pickup_location | dropoff_location 
          1 |              600 |          0 |          5 |             600 |              700
          3 |              300 |          4 |            |                 |                 
         10 |              400 |          4 |            |                 |                 
          2 |              800 |          4 |            |                 |                 
         11 |              100 |          4 |            |                 |                 
         12 |               50 |          4 |            |                 |                 
         13 |              700 |          4 |            |                 |                 
         23 |              200 |          4 |            |                 |    

## Create a view peer1_public over peer1_vehicle and area

1. Write an update trategy on the view `peer1_public` by using Datalog ([real_peer1_public.dl]({{site.github.repository_url}}/tree/master/examples/ride_sharing/real_peer1_public.dl)):

    ```prolog
    %s:peer1_vehicle(VEHICLE_ID , CURRENT_LOCATION , SEAT_COUNT , REQUEST_ID , PICKUP_LOCATION , DROPOFF_LOCATION).
    %s:area(ORIGINAL_NODE , AREA_NODE).
    %v:peer1_public(VEHICLE_ID , CURRENT_AREA , SEAT_COUNT , REQUEST_ID , PICKUP_LOCATION , DROPOFF_LOCATION).

    -peer1_vehicle (V, L, S, R, P, D) :- peer1_vehicle (V, L, S, R, P, D), area (L,A), not peer1_public(V, A, S, R, P, D).
    +peer1_vehicle (V, L, S, R, P, D) :- peer1_public (V, A, S, R, P, D), peer1_vehicle (V,L,_, _, _, _), area (L,A), not peer1_vehicle(V, L, S, R, P, D).
    ```

2. Derive view definition and transform it with the update trategy to SQL statements saved in the file ([real_peer1_public.sql]({{site.github.repository_url}}/tree/master/examples/ride_sharing/real_peer1_public.sql)):
    ```bash
    birds -f examples/ride_sharing/real_peer1_public.dl -o examples/ride_sharing/real_peer1_public.sql
    ```

## Create a view peer2_public over peer2_vehicle and area

1. Write an update trategy on the view `peer2_public` by using Datalog ([real_peer2_public.dl]({{site.github.repository_url}}/tree/master/examples/ride_sharing/real_peer2_public.dl)):

    ```prolog
    %s:peer2_vehicle(VEHICLE_ID , CURRENT_LOCATION , SEAT_COUNT , REQUEST_ID , PICKUP_LOCATION , DROPOFF_LOCATION).
    %s:area(ORIGINAL_NODE , AREA_NODE).
    %v:peer2_public(VEHICLE_ID , CURRENT_AREA , SEAT_COUNT , REQUEST_ID , PICKUP_LOCATION , DROPOFF_LOCATION).

    -peer2_vehicle (V, L, S, R, P, D) :- peer2_vehicle (V, L, S, R, P, D), area (L,A), not peer2_public(V, A, S, R, P, D).
    +peer2_vehicle (V, L, S, R, P, D) :- peer2_public (V, A, S, R, P, D), peer2_vehicle (V,L,_, _, _, _), area (L,A), not peer2_vehicle(V, L, S, R, P, D).
    ```

2. Derive view definition and transform it with the update trategy to SQL statements saved in the file ([real_peer2_public.sql]({{site.github.repository_url}}/tree/master/examples/ride_sharing/real_peer2_public.sql)):
    ```bash
    birds -f examples/ride_sharing/real_peer2_public.dl -o examples/ride_sharing/real_peer2_public.sql
    ```

## Create a view all_vehicles over peer1_public and peer2_public

1. Write an update trategy on the view `all_vehicles` by using Datalog ([real_all_vehicles.dl]({{site.github.repository_url}}/tree/master/examples/ride_sharing/real_all_vehicles.dl)):

    ```prolog
    %s:peer1_public(VEHICLE_ID , CURRENT_AREA , SEAT_COUNT , REQUEST_ID , PICKUP_LOCATION , DROPOFF_LOCATION).
    %s:peer2_public(VEHICLE_ID , CURRENT_AREA , SEAT_COUNT , REQUEST_ID , PICKUP_LOCATION , DROPOFF_LOCATION).
    %v:all_vehicles(COMPANY_ID, VEHICLE_ID , CURRENT_AREA , SEAT_COUNT , REQUEST_ID , PICKUP_LOCATION , DROPOFF_LOCATION).
    +peer1_public(V, A, S, R, P, D) :- all_vehicles(C, V, A, S, R, P, D),C = 1, NOT peer1_public(V, A, S, R, P, D).
    -peer1_public(V, A, S, R, P, D) :- peer1_public(V, A, S, R, P, D), C = 1, NOT all_vehicles(C, V, A, S, R, P, D).
    +peer2_public(V, A, S, R, P, D) :- all_vehicles(C, V, A, S, R, P, D), C = 2, NOT peer2_public(V, A, S, R, P, D).
    -peer2_public(V, A, S, R, P, D) :- peer2_public(V, A, S, R, P, D), C = 2, NOT all_vehicles(C, V, A, S, R, P, D).
    ```

2. Derive view definition and transform it with the update trategy to SQL statements saved in the file ([real_all_vehicles.sql]({{site.github.repository_url}}/tree/master/examples/ride_sharing/real_all_vehicles.sql)):
    ```bash
    birds -f examples/ride_sharing/real_all_vehicles.dl -o examples/ride_sharing/real_all_vehicles.sql
    ```