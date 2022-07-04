/*
Question 1.
Find all the employees who work in the ‘Human Resources’ department.
*/
SELECT *
FROM employees 
WHERE department = 'Human Resources';

/*
Question 2.
Get the first_name, last_name, and country of the employees who work in the ‘Legal’ department.
*/
SELECT
	first_name,
	last_name,
	country
FROM employees 
WHERE department = 'Legal';

/*Question 3.
Count the number of employees based in Portugal*/

SELECT 
	count(*) AS employees_in_portugal
FROM employees 
WHERE country = 'Portugal';


/*Question 4.
Count the number of employees based in either Portugal or Spain.*/
SELECT 
	count(*) AS number_employees
FROM employees 
WHERE country IN ('Portugal', 'Spain');


/*Question 5.
Count the number of pay_details records lacking a local_account_no.*/

SELECT 
	count(*) AS missing_account_no
FROM pay_details
WHERE local_account_no IS NULL ;


/*Question 6.
Are there any pay_details records lacking both a local_account_no and iban number?*/
SELECT
	local_account_no,
	iban 
FROM pay_details
WHERE local_account_no IS NULL AND iban IS NULL ;


/*
 * Question 7.
Get a table with employees first_name and last_name ordered alphabetically by last_name (put any NULLs last).
 * */
SELECT 
	first_name,
	last_name 
FROM employees 
ORDER BY last_name ASC NULLS LAST ;

/*Question 8.
Get a table of employees first_name, last_name and country, ordered alphabetically first by country and then by last_name (put any NULLs last).*/

SELECT
	first_name ,
	last_name ,
	country 
FROM employees 
ORDER BY
	country ASC NULLS LAST ,
	last_name ASC NULLS LAST ;


/*Question 9.
Find the details of the top ten highest paid employees in the corporation.*/

-- CHECK TO see IF MORE than 10 have SIMILAR pay
SELECT
	salary 
FROM employees 
ORDER BY salary DESC NULLS LAST 
LIMIT 100; 

SELECT *
FROM employees 
ORDER BY salary DESC NULLS LAST 
LIMIT 10;

/*Question 10.
Find the first_name, last_name and salary of the lowest paid employee in Hungary.*/
SELECT 
	first_name ,
	last_name ,
	salary ,
	country 
FROM employees
WHERE country = 'Hungary'
ORDER BY salary ASC NULLS LAST
LIMIT 1; -- ONLY 1 employee FROM Hungary so NOT necessarily needed

/*
Question 11.
How many employees have a first_name beginning with ‘F’?*/
SELECT 
	count(*) AS employees_with_F
FROM employees 
WHERE first_name ~ '^F'; -- USING regex

-- alternative
SELECT 
	count(*) AS employees_with_F
FROM employees 
WHERE first_name LIKE 'F%'; -- USING PICSIX


/*Question 12.
Find all the details of any employees with a ‘yahoo’ email address?*/
SELECT *
FROM employees 
WHERE email ILIKE '%yahoo%';


/*Question 13. 
 * Count the number of pension enrolled employees not based in either France or Germany.*/
SELECT 
	count(*)
FROM employees 
WHERE pension_enrol = TRUE AND country IN ('France', 'Germany');


/*Question 14.
What is the maximum salary among those employees in the ‘Engineering’ department who 
work 1.0 full-time equivalent hours (fte_hours)?*/
SELECT 
	max(salary) AS max_engineer_salary
FROM employees 
WHERE department = 'Engineering' AND fte_hours = 1;



/*
Question 15.
Return a table containing each employees first_name, last_name, full-time equivalent hours (fte_hours), salary, and a new column 
effective_yearly_salary which should contain fte_hours multiplied by salary.*/
SELECT 
	first_name ,
	last_name ,
	fte_hours ,
	salary ,
	(fte_hours * salary) AS effective_yearly_salary
FROM employees ;



--------------- EXTENSION HOMEWORK-------------------------------------
/*Question 16.
The corporation wants to make name badges for a forthcoming conference. Return a column 
badge_label showing employees’ first_name and last_name joined together with their department
in the following style: ‘Bob Smith - Legal’. Restrict output to only those employees with 
stored first_name, last_name and department.*/

SELECT 
	concat(first_name, ' ', last_name, ' - ', department) AS badge_label
FROM employees 
WHERE first_name IS NOT NULL AND last_name IS NOT NULL  AND department  IS NOT NULL ;

/*Question 17.
One of the conference organisers thinks it would be nice to add the year of the employees’ 
start_date to the badge_label to celebrate long-standing colleagues, in the following style 
‘Bob Smith - Legal (joined 1998)’. Further restrict output to only those employees with a stored 
start_date.
[If you’re really keen - try adding the month as a string: ‘Bob Smith - Legal (joined July 1998)’]*/
/*Do some research on the SQL EXTRACT() function.
For the extension to the extension (!), the PostgreSQL TO_CHAR() function might help.
To be honest, date and time handling is one of the areas in which the functionality of specific 
RDBMSs deviates most notably from the ANSI SQL standard.*/

SELECT
	concat(first_name, ' ', last_name, ' - ', department, ' (joined ', EXTRACT(YEAR FROM start_date), ')')
FROM employees
WHERE first_name IS NOT NULL AND last_name IS NOT NULL  AND department  IS NOT NULL AND start_date IS NOT NULL ;

-- Extra section with the month
-- replace() has been added due to additional spaces added with to_char() to get the month
SELECT
	concat(first_name, ' ', last_name, ' - ', department, '(joined ', REPLACE(to_char(start_date, 'Month'),' ',''), ' ',
	EXTRACT(YEAR FROM start_date), ')')
FROM employees
WHERE first_name IS NOT NULL AND last_name IS NOT NULL  AND department  IS NOT NULL AND start_date IS NOT NULL ;


/*Question 18.
Return the first_name, last_name and salary of all employees together with a new column called 
salary_class with a value 'low' where salary is less than 40,000 and value 'high' where salary is 
greater than or equal to 40,000.
Investigate how to use a SQL CASE statement to return the required values 'low' and 'high' based 
on the value of salary. Think carefully how to deal with NULL salaries.*/

SELECT 
	first_name,
	last_name ,
	salary ,
	CASE 
		WHEN salary < 40000 THEN 'low'
		WHEN salary >= 40000 THEN 'high'
		WHEN salary IS NULL THEN 'unknown'
	END AS salary_class	
FROM employees 
