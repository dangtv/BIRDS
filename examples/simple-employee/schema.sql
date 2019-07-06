DROP TABLE IF EXISTS ed;
CREATE TABLE ed ( -- department of employees
    ename VARCHAR, -- employee name
    dept VARCHAR -- department name
);

DROP TABLE IF EXISTS dm;
CREATE TABLE dm ( -- department manager
    dept VARCHAR, -- department name
    mgr VARCHAR -- manager
);

DROP TABLE IF EXISTS esa;
CREATE TABLE esa ( -- employee inforamtion
    ename VARCHAR, -- employee name
    sal FLOAT, -- salary
    age INT -- age
);

-- data
DELETE FROM ed;
INSERT INTO ed VALUES
('Gauss', 'Math'),
('Laplace', 'Math'), 
('Newton', 'Physics'), 
('Einstein', 'Physics'),
('Russell', 'Philosophy'), 
('Galileo', 'Astronomy'), 
('Aiken', 'CS');

DELETE FROM dm;
INSERT INTO dm VALUES
('Math', 'Euclid'),
('Math', 'Aryabhata'), 
('Physics', 'Aristotle'), 
('Philosophy', 'Aristotle'), 
('Logic', 'Aristotle'), 
('Medicine', 'Hippocrates');

DELETE FROM esa;
INSERT INTO esa VALUES
('Gauss', 50000,25),
('Newton', 50000,35),
('Russell', 99000,50),
('Einstein', 75000,50);

-- views need created:
CREATE VIEW em AS 
    SELECT DISTINCT ed.ename, dm.mgr FROM ed, dm WHERE ed.dept = dm.dept;

CREATE VIEW edm AS 
    SELECT DISTINCT ed.ename, ed.dept, dm.mgr FROM ed,dm WHERE ed.dept=dm.dept;

CREATE VIEW eds AS 
    SELECT DISTINCT ed.ename, ed.dept, esa.sal FROM ed,esa WHERE ed.ename = esa.ename;

CREATE VIEW edptolemy AS
    SELECT DISTINCT ed.ename, ed.dept FROM ed,dm WHERE ed.dept=dm.dept AND dm.mgr = 'Ptolemy';

-- updates on the views


-- updates on the view em:
INSERT INTO em VALUES('Smith',  'Aristotle');
-- there are two ways for tranlating this to updates on source:
-- u1: [INSERT ED (ENAME = Smith, DEPT = Physics)]
-- u1': [INSERT ED (ENAME = Smith, DEPT = Philosophy)]
-- both u1 and u1'
UPDATE em set ename='Smith2' where ename='Smith';
INSERT INTO em VALUES('NEW E',  'NEW M');
update em set mgr='NEW M2' where mgr='NEW M';
update em set ename='NEW E2' where mgr='NEW M2';
INSERT INTO em VALUES('Gauss', 'Gauss manager'); 
INSERT INTO em VALUES('Euclid employee', 'Euclid');
update em set mgr='Aristotle2' where mgr='Aristotle';
delete from em where ename='Smith2';
delete from em where ename='Newton';
delete from em where mgr='Aristotle2';
delete from em where mgr='Aryabhata';

-- updates on the view edm:
INSERT INTO edm VALUES ( 'Kepler',  'Astronomy', 'Ptolemy'); -- invalid update on view because of  'Galileo', 'Astronomy' in the table ed
INSERT INTO edm VALUES('Smith', 'Philosophy', 'Aristotle');
INSERT INTO edm VALUES('NEW E', 'NEW D', 'NEW M');
update edm set mgr='NEW M2' where mgr='NEW M';
update edm set ename='NEW E2' where mgr='NEW M2';
UPDATE edm set ename='Smith2' where ename='Smith';
delete from edm where ename='Smith2';


