-- CREATE TYPE gender AS ENUM('M', 'F');

-- DROP TABLE IF EXISTS male_emp CASCADE;
CREATE TABLE male (
    emp_name text,
    birth_date date
);

-- DROP TABLE IF EXISTS female_emp CASCADE;
CREATE TABLE female (
    emp_name text,
    birth_date date
);

CREATE TABLE others (
    emp_name text,
    birth_date date,
    gender text
);

-- DROP TABLE IF EXISTS ed CASCADE;
CREATE TABLE ed (
    emp_name text,
    dept_name character varying(40)
);

CREATE TABLE eed (
    emp_name text,
    dept_name character varying(40)
);

CREATE TABLE blacklist (
    emp_name text
);

\echo 'LOADING male'
\i load_male.sql

\echo 'LOADING female'
\i load_female.sql

\echo 'LOADING others'
\i load_others.sql

\echo 'LOADING ed'
\i load_ed.sql

\echo 'LOADING eed'
\i load_eed.sql

\echo 'LOADING blacklist'
\i load_blacklist.sql