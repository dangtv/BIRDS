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

CREATE TABLE dates1962 (
    birth_date date
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

-- DROP TABLE IF EXISTS dept_mgr CASCADE;
CREATE TABLE dept_mgr (
    dept_name character varying(40),
    mgr_name text
);
