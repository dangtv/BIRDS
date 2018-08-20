---
layout: default
---

# BIRDS tutorial: examples on music schema

Example: music schema ([schema.sql]({{site.github.repository_url}}/tree/master/examples/music/schema.sql))

tracks1 |
-------------|------|--------|----------|----------
  track      | date | rating |  album   | quantity 
 Trust       | 2018 |      5 | Wish     |        5
 Lovesong    | 2018 |      5 | Galore   |        1
 Mysong      | 2018 |      5 | Galore   |        1
 Lullaby     | 2018 |      3 | Show     |        3

## Create a view tracks2 over tracks1

1. Write an update trategy on the view `tracks2` by using Datalog ([tracks2.dl]({{site.github.repository_url}}/tree/master/examples/music/tracks2.dl)):

    ```prolog
    %s:tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY).
    %v:tracks2(TRACK,RATING,ALBUM,QUANTITY).
    +tracks1(TRACK,2018,RATING,ALBUM,QUANTITY) :- tracks2(TRACK,RATING,ALBUM,QUANTITY), not tracks1(TRACK,_,RATING,ALBUM,QUANTITY).
    -tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY) :- tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY), not tracks2(TRACK,RATING,ALBUM,QUANTITY),not tracks2(TRACK,RATING,ALBUM,QUANTITY).
    ```

2. Derive view definition and transform it with the update trategy to SQL statements saved in the file ([tracks2.sql]({{site.github.repository_url}}/tree/master/examples/music/tracks2.sql)):
    ```bash
    birds -f examples/music/tracks2.dl -o examples/music/tracks2.sql
    ```

## Create a view tracks3 over tracks2

1. Write an update trategy on the view `tracks3` by using Datalog ([tracks3.dl]({{site.github.repository_url}}/tree/master/examples/music/tracks3.dl)):

    ```prolog
    %v:tracks3(TRACK,RATING,ALBUM,QUANTITY).
    +tracks2(TRACK,RATING,ALBUM,QUANTITY) :- tracks3(TRACK,RATING,ALBUM,QUANTITY), not tracks2(TRACK,RATING,ALBUM,QUANTITY), QUANTITY > 2.
    -tracks2(TRACK,RATING,ALBUM,QUANTITY) :- tracks2(TRACK,RATING,ALBUM,QUANTITY), not tracks3(TRACK,RATING,ALBUM,QUANTITY), QUANTITY > 2.
    ```

2. Derive view definition and transform it with the update trategy to SQL statements saved in the file ([tracks3.sql]({{site.github.repository_url}}/tree/master/examples/music/tracks3.sql)):
    ```bash
    birds -f examples/music/tracks3.dl -o examples/music/tracks3.sql
    ```