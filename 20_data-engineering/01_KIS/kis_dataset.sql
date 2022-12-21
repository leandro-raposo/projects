use kis_db;

create table subject
(
	subject_id int,
	subject_name varchar(100),
	max_score int,
	lecturer varchar(100)
);

create table student
(
	student_id int,
	student_name varchar(100),
	city varchar(100),
	subject_id int
);

create table assignments
(
	student_id int,
	subject_id int,
	description varchar(100),
	score double,
	submission_date timestamp
);

INSERT INTO teste_ba.student (student_id, student_name, city, subject_id) VALUES (2001, 'Thurman Thorn', 'New York', 11);
INSERT INTO teste_ba.student (student_id, student_name, city, subject_id) VALUES (2002, 'Sharda Clemens', 'San Francisco', 12);
INSERT INTO teste_ba.student (student_id, student_name, city, subject_id) VALUES (2003, 'Buck Elkins', 'New York', 13);
INSERT INTO teste_ba.student (student_id, student_name, city, subject_id) VALUES (2004, 'Fabian Johns', 'Boston', 15);
INSERT INTO teste_ba.student (student_id, student_name, city, subject_id) VALUES (2005, 'Brad Cameron', 'Stanford', 11);
INSERT INTO teste_ba.student (student_id, student_name, city, subject_id) VALUES (2006, 'Sofia Roles', 'Boston', 16);
INSERT INTO teste_ba.student (student_id, student_name, city, subject_id) VALUES (2007, 'Rory Pietila', 'New Haven', 12);
INSERT INTO teste_ba.student (student_id, student_name, city, subject_id) VALUES (2008, 'Cicely Weish', 'Tulsa', 14);
INSERT INTO teste_ba.student (student_id, student_name, city, subject_id) VALUES (2011, 'Richard Curtin', 'Boston', 11);
INSERT INTO teste_ba.student (student_id, student_name, city, subject_id) VALUES (2012, 'Kassy Ledger', 'Stanford', 11);
INSERT INTO teste_ba.student (student_id, student_name, city, subject_id) VALUES (2013, 'Henry Ledger', 'Miami', 13);
INSERT INTO teste_ba.student (student_id, student_name, city, subject_id) VALUES (2014, 'Darius Fidzberg', 'San Francisco', 12);
INSERT INTO teste_ba.student (student_id, student_name, city, subject_id) VALUES (2015, 'Darcey Fiorillo', 'Chicago', 14);

INSERT INTO teste_ba.subject (subject_id, subject_name, max_score, lecturer) VALUES (11, 'Math', 130, 'Christena Solem');
INSERT INTO teste_ba.subject (subject_id, subject_name, max_score, lecturer) VALUES (12, 'Computer Science', 50, 'Jaime Pille');
INSERT INTO teste_ba.subject (subject_id, subject_name, max_score, lecturer) VALUES (13, 'Biology', 300, 'Carrol Denmark');
INSERT INTO teste_ba.subject (subject_id, subject_name, max_score, lecturer) VALUES (14, 'Geography', 220, 'Yuette Galang');
INSERT INTO teste_ba.subject (subject_id, subject_name, max_score, lecturer) VALUES (15, 'Physics', 110, 'Colton Rather');
INSERT INTO teste_ba.subject (subject_id, subject_name, max_score, lecturer) VALUES (16, 'Chemistry', 400, 'Nan Mongeau');

INSERT INTO teste_ba.assignments (student_id, subject_id, description, score, submission_date) VALUES (2001, 11, 'Assignment Q1', 8, '2021-04-11 20:05:12');
INSERT INTO teste_ba.assignments (student_id, subject_id, description, score, submission_date) VALUES (2011, 11, 'Assignment Q1', 3, '2021-04-11 19:05:22');
INSERT INTO teste_ba.assignments (student_id, subject_id, description, score, submission_date) VALUES (2001, 11, 'Assignment Q2', 9.5, '2021-05-11 20:05:32');