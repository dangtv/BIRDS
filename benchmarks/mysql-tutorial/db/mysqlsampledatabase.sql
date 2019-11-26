/*
*********************************************************************
http://www.mysqltutorial.org
*********************************************************************
Name: MySQL Sample Database classicmodels
Link: http://www.mysqltutorial.org/mysql-sample-database.aspx
Version 3.1
+ changed data type from DOUBLE to DECIMAL for amount columns
Version 3.0
+ changed DATETIME to DATE for some colunmns
Version 2.0
+ changed table type from MyISAM to InnoDB
+ added foreign keys for all tables 
*********************************************************************
*/


/*!40101 SET NAMES utf8 */;

/*!40101 SET SQL_MODE=''*/;

/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;
-- CREATE DATABASE /*!32312 IF NOT EXISTS*/`classicmodels` /*!40100 DEFAULT CHARACTER SET latin1 */;

-- USE `classicmodels`;

/*Table structure for table `offices` */

DROP TABLE IF EXISTS offices CASCADE;

CREATE TABLE offices (
  officeCode varchar(10) NOT NULL,
  city varchar(100) NOT NULL,
  phone varchar(100) NOT NULL,
  addressLine1 varchar(100) NOT NULL,
  addressLine2 varchar(100) DEFAULT NULL,
  state varchar(100) DEFAULT NULL,
  country varchar(100) NOT NULL,
  postalCode varchar(15) NOT NULL,
  territory varchar(100) NOT NULL,
  PRIMARY KEY (officeCode)
) ;

/*Table structure for table `employees` */

DROP TABLE IF EXISTS employees CASCADE;

CREATE TABLE employees (
  employeeNumber int NOT NULL,
  lastName varchar(50) NOT NULL,
  firstName varchar(50) NOT NULL,
  extension varchar(10) NOT NULL,
  email varchar(100) NOT NULL,
  officeCode varchar(10) NOT NULL,
  reportsTo int DEFAULT NULL,
  jobTitle varchar(50) NOT NULL,
  PRIMARY KEY (employeeNumber)
 ,
  CONSTRAINT employees_ibfk_1 FOREIGN KEY (reportsTo) REFERENCES employees (employeeNumber),
  CONSTRAINT employees_ibfk_2 FOREIGN KEY (officeCode) REFERENCES offices (officeCode)
) ;

CREATE INDEX reportsTo ON employees (reportsTo);
CREATE INDEX officeCode ON employees (officeCode);

/*Table structure for table `customers` */

DROP TABLE IF EXISTS customers CASCADE;

CREATE TABLE customers (
  customerNumber int NOT NULL,
  customerName varchar(50) NOT NULL,
  contactLastName varchar(50) NOT NULL,
  contactFirstName varchar(50) NOT NULL,
  phone varchar(50) NOT NULL,
  addressLine1 varchar(50) NOT NULL,
  addressLine2 varchar(50) DEFAULT NULL,
  city varchar(50) NOT NULL,
  state varchar(50) DEFAULT NULL,
  postalCode varchar(15) DEFAULT NULL,
  country varchar(50) NOT NULL,
  salesRepEmployeeNumber int DEFAULT NULL,
  creditLimit decimal(10,2) DEFAULT NULL,
  PRIMARY KEY (customerNumber)
 ,
  CONSTRAINT customers_ibfk_1 FOREIGN KEY (salesRepEmployeeNumber) REFERENCES employees (employeeNumber)
) ;

CREATE INDEX salesRepEmployeeNumber ON customers (salesRepEmployeeNumber);

/*Table structure for table `orders` */

DROP TABLE IF EXISTS orders CASCADE;

CREATE TABLE orders (
  orderNumber int NOT NULL,
  orderDate date NOT NULL,
  requiredDate date NOT NULL,
  shippedDate date DEFAULT NULL,
  status varchar(15) NOT NULL,
  comments text,
  customerNumber int NOT NULL,
  PRIMARY KEY (orderNumber)
 ,
  CONSTRAINT orders_ibfk_1 FOREIGN KEY (customerNumber) REFERENCES customers (customerNumber)
) ;

CREATE INDEX customerNumber ON orders (customerNumber);

/*Table structure for table `productlines` */

DROP TABLE IF EXISTS productlines CASCADE;

CREATE TABLE productlines (
  productLine varchar(50) NOT NULL,
  textDescription varchar(4000) DEFAULT NULL,
  htmlDescription text,
  image text,
  PRIMARY KEY (productLine)
) ;

/*Table structure for table `products` */

DROP TABLE IF EXISTS products CASCADE;

CREATE TABLE products (
  productCode varchar(15) NOT NULL,
  productName varchar(70) NOT NULL,
  productLine varchar(50) NOT NULL,
  productScale varchar(10) NOT NULL,
  productVendor varchar(50) NOT NULL,
  productDescription text NOT NULL,
  quantityInStock smallint NOT NULL,
  buyPrice decimal(10,2) NOT NULL,
  MSRP decimal(10,2) NOT NULL,
  PRIMARY KEY (productCode)
 ,
  CONSTRAINT products_ibfk_1 FOREIGN KEY (productLine) REFERENCES productlines (productLine)
) ;

CREATE INDEX productLine ON products (productLine);

/*Table structure for table `orderdetails` */

DROP TABLE IF EXISTS orderdetails CASCADE;

CREATE TABLE orderdetails (
  orderNumber int NOT NULL,
  productCode varchar(15) NOT NULL,
  quantityOrdered int NOT NULL,
  priceEach decimal(10,2) NOT NULL,
  orderLineNumber smallint NOT NULL,
  PRIMARY KEY (orderNumber,productCode)
 ,
  CONSTRAINT orderdetails_ibfk_1 FOREIGN KEY (orderNumber) REFERENCES orders (orderNumber),
  CONSTRAINT orderdetails_ibfk_2 FOREIGN KEY (productCode) REFERENCES products (productCode)
) ;

CREATE INDEX productCode ON orderdetails (productCode);

/*Table structure for table `payments` */

DROP TABLE IF EXISTS payments CASCADE;

CREATE TABLE payments (
  customerNumber int NOT NULL,
  checkNumber varchar(50) NOT NULL,
  paymentDate date NOT NULL,
  amount decimal(10,2) NOT NULL,
  PRIMARY KEY (customerNumber,checkNumber),
  CONSTRAINT payments_ibfk_1 FOREIGN KEY (customerNumber) REFERENCES customers (customerNumber)
) ;

-- load data

\echo 'LOADING offices.sql'
\i offices.sql

\echo 'LOADING employees.sql'
\i employees.sql

\echo 'LOADING customers.sql'
\i customers.sql

\echo 'LOADING orders.sql'
\i orders.sql

\echo 'LOADING productlines.sql'
\i productlines.sql

\echo 'LOADING products.sql'
\i products.sql

\echo 'LOADING orderdetails.sql'
\i orderdetails.sql

\echo 'LOADING payments.sql'
\i payments.sql

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
