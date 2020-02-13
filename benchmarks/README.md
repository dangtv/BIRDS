# Benchmark

## Views in the benchmark

```sql
activestudents(NAME, LOGIN, CLUB, SINCE) :- students(SID, NAME, LOGIN, AGE, GPA), clubs(CLUB, SINCE, NAME), GPA > 3.0.

bstudents(NAME, SID, COURSE) :- students(SID, NAME, LOGIN, AGE, GPA), enrolled(SID, COURSE, GRADE), GRADE='B'.

goodstudents(SID, GPA) :- students(SID, _, _, _, GPA), GPA > 3.0.

paramountmovies(TITLE, YEAR) :- movies(TITLE, YEAR, _, _, STUDIONAME, _), STUDIONAME='Paramount'.

newpc(MAKER, MODEL, SPEED, RAM, HD, PRICE) :- product(MAKER, MODEL, TYPE), pc(MODEL, SPEED, RAM, HD, PRICE), TYPE = 'pc'.

officeinfo(OFFICECODE, PHONE, CITY) :- offices(OFFICECODE, CITY, PHONE, ADDRESSLINE1, ADDRESSLINE2, STATE, COUNTRY, POSTALCODE, TERRITORY).

luxuryitems(ID, NAME, PRICE) :- items(ID, NAME, PRICE), PRICE > 700.0.

cars_master(CAR_ID, CAR_NAME) :- cars(CAR_ID, CAR_NAME, BRAND_ID).

all_cars(CAR_ID, CAR_NAME, BRAND_ID, BRAND_NAME) :- cars(CAR_ID, CAR_NAME, BRAND_ID), brands(BRAND_ID, BRAND_NAME).

vw_customers(NAME, ADDRESS, WEBSITE, CREDIT_LIMIT, FIRST_NAME, LAST_NAME, EMAIL, PHONE) :- customers(CUSTOMER_ID, NAME, ADDRESS, WEBSITE, CREDIT_LIMIT), contacts(CONTACT_ID, FIRST_NAME, LAST_NAME, EMAIL, PHONE, CUSTOMER_ID).

emp_view(EMPNAME, SALARY, MAX(STAMP)) :- emp(EMPNAME, SALARY), emp_audit(_, _, EMPNAME, _, STAMP).

usa_city(CITY_ID, CITY, COUNTRY_ID) :- city(CITY_ID, CITY, COUNTRY_ID, _), COUNTRY_ID = 103.

measurement(CITY_ID, LOGDATE, PEAKTEMP, UNITSALES) :- measurement_y2006m02(CITY_ID, LOGDATE, PEAKTEMP, UNITSALES).
measurement(CITY_ID, LOGDATE, PEAKTEMP, UNITSALES) :- measurement_y2006m03(CITY_ID, LOGDATE, PEAKTEMP, UNITSALES).

vw_brands(BRAND_NAME, 'Approved') :- brands(BRAND_ID, BRAND_NAME).
vw_brands(BRAND_NAME, 'Pending Approval') :- brand_approvals(BRAND_ID, BRAND_NAME).

tracks1(T,D,R,A,Q) :- tracks(T,D,R,A), albums(A,Q).

tracks2(T,R,A,Q) :- tracks1(T,_,R,A,Q).

tracks3(T,R,A,Q) :- tracks2(T,R,A,Q),Q > 2.

residents(E, B, G) :- male(E, B), G = 'M'.
residents(E, B, G) :- female(E, B), G = 'F'.
residents(E, B, G) :- others(E, B, G).

ced(E, D) :- ed(E, D), NOT eed(E, D).

residents1962(E,B,G) :- residents(E,B,G), ¬ B < '1962-01-01', ¬ B > '1962-12-31'.

employees(E,B,G) :- residents(E,B,G), ced(E,_).

researchers(E) :- residents(E,B,G), ced(E,D), D = 'Research'. 

retired(E) :- residents(E,_,_), ¬ced(E,_).

ukaz_lok(KOD_LOK, NAZEV, KATASTR, PRESNOST, 50.0, 14.0) :- lokalita(KOD_LOK, NAZEV, KATASTR, PRESNOST).

purchaseview(PURCHASE_ID, PRODUCT_NAME, WHEN_BOUGHT) :- purchase(PURCHASE_ID, PRODUCT_ID, WHEN_BOUGHT), product(PRODUCT_ID, PRODUCT_NAME).

message(TXTMESSAGEID, MESSAGETECH, MESSAGETYPE, MESSAGE, MESSAGEDATE, MESSAGEMOBILE, MESSAGEREAD, MESSAGESENDER) :- messagecentre(TXTMESSAGEID, MESSAGETYPE, MESSAGE, MESSAGEDATE, MESSAGEMOBILE, MESSAGEREAD, MESSAGESENDER), MESSAGETECH = 'S'.
message(MESSAGEID, MESSAGETECH, TYPE, TEXT, DATE, ADDRESS, READ, SENDER) :- messagecentreemail(MESSAGEID, TYPE, TEXT, DATE, ADDRESS, READ, SENDER), MESSAGETECH = 'E'.

outstanding_task(ID, PARENT_TASK_ID, CINEMA_ID, VENUE_ID, MOVIE_ID, EVENT_ID, RESULT, CONTEXT, GUIDE, STARTED_AT, ENDED_AT, CREATED_AT, UPDATED_AT) :- task(ID, PARENT_TASK_ID, CINEMA_ID, VENUE_ID, MOVIE_ID, EVENT_ID, RESULT, CONTEXT, GUIDE, STARTED_AT, ENDED_AT, CREATED_AT, UPDATED_AT), task(PARENT_TASK_ID, _, _, _, _, _, _, _, _, _, _, _, _).

products(ID, TITLE, DESCRIPTION, MANUFACTURER_ID, CREATED_AT, UPDATED_AT, MPN, VISIBLE, SUBSCRIPTIONS_COUNT) :- products_raw(ID, TITLE, DESCRIPTION, MANUFACTURER_ID, CREATED_AT, UPDATED_AT, MPN, VISIBLE), subscriptions_agg(ID, SUBSCRIPTIONS_COUNT).

poi_view(POI_ID, COL_A, COL_B, COL_D, COL_E) :- poi(POI_ID, COL_A, COL_B, COL_C), points(POI_ID, COL_D, COL_E, COL_F).

vw_company_phonelist('Customer', ID, NUMBER, LASTNAME, FISTNAME, PHONENO) :- customer(ID, NUMBER, LASTNAME, FISTNAME, PHONENO).
vw_company_phonelist('Supplier', ID, NUMBER, LASTNAME, FISTNAME, PHONENO) :- supplier(ID, NUMBER, LASTNAME, FISTNAME, PHONENO).
% vw_company_phonelist('Vendor', ID, NUMBER, LASTNAME, FISTNAME, PHONENO) :- vendor(ID, NUMBER, LASTNAME, FISTNAME, PHONENO).

vehicle_view(VEHICLE_ID, SIZE, NAME) :- vehicle(VEHICLE_ID, SIZE, BRAND_ID), brand(BRAND_ID, NAME).

vwemployees(PERSONID, EMPLOYEEID, FIRSTNAME, LASTNAME, TITLE) :- employees(EMPLOYEEID, PERSONID, TITLE), persons(PERSONID, FIRSTNAME, LASTNAME).

koncerty(NAZWA_KLUBU, ADRES_KLUBU, NAZWA_ZESPOLU, ILOSC_CZLONKOW_ZESPOLU, DATA_WYSTEPU) :- koncert(NAZWA_KLUBU, NAZWA_ZESPOLU, DATA_WYSTEPU), klub(NAZWA_KLUBU, ADRES_KLUBU), zespol(NAZWA_ZESPOLU, ILOSC_CZLONKOW_ZESPOLU).
```

## How to run the benchmark

### Validation time 

To run the validation for Datalog programs in the benchmark:

```bash
 bash benchmark.sh
```

The results are stored in the folder `results`
The validation time is stored in the file `putbx_time.csv`

To calculate the size of Datalog programs:

```bash
 bash datalog_cloc.sh
```

The result is in the file `datalog.csv`

To calculate the size of compiled SQL programs:

```bash
 bash sql_size.sh <result-folder>
```

The result is in the file `<result-folder>/sql_size.txt`

## View updating time

For each view, to measure the running time of an SQL statement that modifies the view, we first need to generate data for the underlying base tables by

```bash
bash dbgen.sh
```

That randomly generates data for the base tables with various numbers of tuples. The result is many CSV files with different numbers of records (tuples) for each base table. This data generation process may take time.

Then, we can start benchmarking by

```bash
bash benchmark.sh
```
