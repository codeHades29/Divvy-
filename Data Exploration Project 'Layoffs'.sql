--Exploratory Data Analysis

select * 
from layoffs_staging;

--It was showing a value of NULL
select max(total_laid_off) as max_total
from layoffs_staging
where total_laid_off is not null; 

--I could not cast to INT due to the word NULL instead of null
select cast(total_laid_off as int)
from layoffs_staging;

--Changed value of NULL to being null, then cast, then select Max worked
update layoffs_staging
SET total_laid_off = null
where total_laid_off = 'NULL';

--
select max(total_laid_off) as max_total, max(percentage_laid_off) as max_percent
from layoffs_staging;

--
select * 
from layoffs_staging
where percentage_laid_off = 1
order by funds_raised_millions DESC;


--Changed Data type to decimal after removing 'NULLS' value
update layoffs_staging
SET funds_raised_millions = null
where funds_raised_millions = 'NULL';

--Time to check out the data
select company, sum(total_laid_off) as sum_total_laid_off
from layoffs_staging
group by company
order by 2 DESC;

select min(date), max(date), sum(total_laid_off)
from layoffs_staging
where country = 'United States';

--
select industry, sum(total_laid_off) as sum_total_laid_off
from layoffs_staging
group by industry
order by sum_total_laid_off DESC;

--
select country, sum(total_laid_off) as sum_total_laid_off
from layoffs_staging
group by country
order by sum_total_laid_off DESC;

--
select year(date), sum(total_laid_off) as sum_total_laid_off
from layoffs_staging
group by year(date)
order by 1 DESC;

--
select stage, sum(total_laid_off) as sum_total_laid_off
from layoffs_staging
group by stage
order by 2 DESC;

--Rolling Total Month
with new_cte as 
(
SELECT 
    DATEPART(YEAR, date) AS Year,
    DATEPART(MONTH, date) AS Month,
    sum(total_laid_off) as sum_total
FROM 
    layoffs_staging
	where DATEPART(MONTH, date) is not null
GROUP BY 
    DATEPART(YEAR, date),
    DATEPART(MONTH, date)
	)
select year, month, sum_total, sum(sum_total) OVER(ORDER BY Year, Month) as rolling_total
from new_cte

--Company lay offs
select company, DATEPART(YEAR, date) as year, sum(total_laid_off) as laid_off
from layoffs_staging
where total_laid_off is not null and DATEPART(YEAR, date) is not null
group by company, DATEPART(YEAR, date)
order by sum(total_laid_off) desc


--Looking to see what companies laid off the most per year(some had multiple lay offs)
with company_year as
(
select company, DATEPART(YEAR, date) as year, sum(total_laid_off) as laid_off
from layoffs_staging
where total_laid_off is not null and DATEPART(YEAR, date) is not null
group by company, DATEPART(YEAR, date)
)
, company_year_rank as
(
select *, DENSE_RANK() OVER( PARTITION BY year order by laid_off desc) as rank
from company_year
)
select *
from company_year_rank
where rank <= 5

--Same as above but industry
with industry_year as
(
select industry, DATEPART(YEAR, date) as year, sum(total_laid_off) as laid_off
from layoffs_staging
where total_laid_off is not null and DATEPART(YEAR, date) is not null
group by industry, DATEPART(YEAR, date)
)
, industry_year_rank as
(
select *, DENSE_RANK() OVER( PARTITION BY year order by laid_off desc) as rank
from industry_year
)
select *
from industry_year_rank
where rank <= 5