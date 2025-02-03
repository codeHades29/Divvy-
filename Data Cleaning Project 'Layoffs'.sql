--Data Cleaning Project

create database world_layoffs;

--1. Remove Duplicates
--2. Standarize the data
--3. Null/blank Values
--4. Remove Columns

use world_layoffs
select * 
from dbo.layoffs;

--Copying data to keep raw data form
select * 
into layoffs_staging
from dbo.layoffs;

--CTE
WITH CTE_Filter AS
(
select *, 
ROW_NUMBER() OVER(
Partition by company, 'location', industry, total_laid_off, percentage_laid_off, 'date', stage, country, funds_raised_millions order by company) as row_num
from layoffs_staging
)
select *
from CTE_Filter
where row_num > 1;

--Checking the CTE
select *
from layoffs_staging
where company = 'Cazoo';

--Deleting the Duplicates
WITH CTE_Filter AS
(
select *, 
ROW_NUMBER() OVER(
Partition by company, 'location', industry, total_laid_off, percentage_laid_off, 'date', stage, country, funds_raised_millions order by company) as row_num
from layoffs_staging
)
delete
from CTE_Filter
where row_num > 1;

--Standarizing Data

select company, trim(company)
from layoffs_staging;
--If you like the above results then follow below
update layoffs_staging
set company = trim(company);

select distinct(industry)
from layoffs_staging
order by 1 --orders by column
--After running the above code I find crypto variations that
--need to be changed, as well as null/blank 

--Finds all industries that begin with crypto...
select *
from layoffs_staging
where industry like 'crypto%'

--The below fixes the issue
update layoffs_staging
set industry = 'Crypto'
where industry like 'crypto%'

--Found a duplicate country
select distinct(country)
from layoffs_staging
order by 1

--Get rid of it
update layoffs_staging
set country = 'United States'
where country like 'United States.'

--
--Convert date to timestamp, actually went back and imported the 
--data again to bring it in as a date


--
select *
from layoffs_staging
where industry = ' ';

select *
from layoffs_staging
where company = 'Airbnb';


--Airbnb has a null in industry which must be fixed to get accurate results
--below shows you side by side the duplicate companies. There is a copy within
--the example to make 4 rows even
select *
from layoffs_staging st
join layoffs_staging st2
	on st.company = st2.company 
	and st.location = st2.location
where (st.industry is null or st.industry = ' ')
and st2.industry is not null


--Showing columns side by side
select st.industry, st2.industry
from layoffs_staging st
join layoffs_staging st2
	on st.company = st2.company 
	and st.location = st2.location
where (st.industry is null or st.industry = ' ')
and st2.industry is not null

--You must use an alias in the from, I was required 
--to move the SET from below the JOIN to under the
--UPDATE
update st
set st.industry = st2.industry
from layoffs_staging st
join layoffs_staging st2
	on st.company = st2.company 
where (st.industry is null or st.industry = ' ')
and st2.industry is not null;

--Time to check, One value was written 'NULL' instead of null
select *
from layoffs_staging
where industry = 'NULL';
--Updated it to be null
update layoffs_staging
set industry = null
where industry ='NULL';
--Checking work
select *
from layoffs_staging
where company like 'Bally%';

select * from layoffs_staging
where industry is null;

--Looks good, Bally's doesn't have a duplicate with industry value filled out
--so there isn't any other row to attach it. We simply don't know what industry

select *
from layoffs_staging
where total_laid_off is null
and percentage_laid_off is null;
