---
layout: default
---

# Example: music schema

The music schema ([schema.sql]({{site.github.repository_url}}/tree/master/examples/music/schema.sql)) is from an example in the paper "[Relational lenses: a language for updatable views](https://dl.acm.org/citation.cfm?id=1142399)"

tracks1 

  track      | date | rating |  album   | quantity 
-------------|------|--------|----------|----------
 Trust       | 2018 |      5 | Wish     |        5
 Lovesong    | 2018 |      5 | Galore   |        1
 Mysong      | 2018 |      5 | Galore   |        1
 Lullaby     | 2018 |      3 | Show     |        3

## An update strategy for `tracks2`

An update strategy on the view `tracks2` by using Datalog ([tracks2.dl]({{site.github.repository_url}}/tree/master/examples/music/tracks2.dl)):

```prolog
% describe the schema of sources and views
source tracks1('TRACK':string,'DATE':int,'RATING':int,'ALBUM':string,'QUANTITY':int).
view tracks2('TRACK':string,'RATING':int,'ALBUM':string,'QUANTITY':int).

% rule for deletion from tracks1
-tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY) :- tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY),
    not tracks2(TRACK,RATING,ALBUM,QUANTITY).

+tracks1(TRACK,DATE,RATING,ALBUM,QUANTITY) :- tracks2(TRACK,RATING,ALBUM,QUANTITY),
    not tracks1(TRACK,_,RATING,ALBUM,QUANTITY), tracks1(TRACK,DATE,_,ALBUM,_).

+tracks1(TRACK,2018,RATING,ALBUM,QUANTITY) :- tracks2(TRACK,RATING,ALBUM,QUANTITY),
    not tracks1(TRACK,_,_,ALBUM,_).
```

Verifying and compiling the update strategy into SQL statements saved in the file ([tracks2.sql]({{site.github.repository_url}}/tree/master/examples/music/tracks2.sql)):

```bash
birds -v -f tracks2.dl -o tracks2.sql
```

## An update strategy for `tracks3`

An update trategy on the view `tracks3` over the view `tracks2` by using Datalog ([tracks3.dl]({{site.github.repository_url}}/tree/master/examples/music/tracks3.dl)):

```prolog
% describe the schema of sources and views
source tracks2('TRACK':string,'RATING':int,'ALBUM':string,'QUANTITY':int).
view tracks3('TRACK':string,'RATING':int,'ALBUM':string,'QUANTITY':int).

% constraints:
⊥() :- tracks3(T,R,A,Q), NOT Q>2.

% view definition:
% tracks3(T,R,A,Q) :- tracks2(T,R,A,Q),Q > 2.

% rule for insertion to tracks2
+tracks2(TRACK,RATING,ALBUM,QUANTITY) :- tracks3(TRACK,RATING,ALBUM,QUANTITY),
    not tracks2(TRACK,RATING,ALBUM,QUANTITY), QUANTITY > 2.

% rule for deletion from tracks2
-tracks2(TRACK,RATING,ALBUM,QUANTITY) :- tracks2(TRACK,RATING,ALBUM,QUANTITY),
    not tracks3(TRACK,RATING,ALBUM,QUANTITY), QUANTITY > 2.
```

Verifying and compiling the update strategy to SQL statements saved in the file ([tracks3.sql]({{site.github.repository_url}}/tree/master/examples/music/tracks3.sql)):
   
```bash
birds -v -f tracks3.dl -o tracks3.sql
```