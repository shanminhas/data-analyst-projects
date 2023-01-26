--/****** Script for SelectTopNRows command from SSMS  ******/
--SELECT TOP (1000) [Name]
--      ,[Author]
--      ,[User_Rating]
--      ,[NumReviews]
--      ,[Price]
--      ,[Year]
--      ,[Genre]
--  FROM [CaseStudy3].[dbo].[bestsellers_with_categories]
---- OUTPUT: 550 Rows
---- The above told me that there are 550 entrys in this table
---- I loaded 1000, only got 550
---- FIrst thing I wanted to do was to find out how many books there are, seperated by Genre

-- confirm that the rows 'loaded rows' number is legitimate. New to SQL so I don't know if trusting that number implicitly is OK yet or not. 
-- What I mean by that is, maybe sometimes the rows loaded number only shows what is visibly there but the COUNT function would properly count the missing rows. Visual bugs happen too often f
SELECT COUNT(Genre)
FROM bestsellers_with_categories
-- OUTPUT: 550
-- ok phew, it really is 550
-- Really could have checked the Kaggle source for this, but I wanted to know how to quickly do this

SELECT COUNT(Genre)
FROM bestsellers_with_categories
WHERE Genre = 'Fiction'
-- OUTPUT: 240
-- so naturally there should be (550-240 =) 310 non-fiction books,
-- but should double check, 

SELECT COUNT(Genre)
FROM bestsellers_with_categories
WHERE Genre = 'Non Fiction'
-- OUTPUT: 310
-- this also confirmed there are no nulls... for this column. Now to check if there are any nulls in the other columns just in case


-- Quick googling has shown me that it is not simple to find null values in every column without typing out the column names, so I will list the columns manually.
-- Small dataset so not an issue, but might be later on
SELECT *
FROM bestsellers_with_categories
WHERE Name IS NULL AND Author IS NULL AND User_Rating IS NULL AND NumReviews IS NULL AND Price IS NULL AND Year IS NULL AND Genre IS NULL;
-- OUTPUT: 0 rows
-- Yay! no null values 

-- Lets check for duplicates. If it has the same name, rating, number of reviews, and year release
SELECT Name, Author, NumReviews, User_Rating, Year, COUNT(*) as NumDuplicates
FROM bestsellers_with_categories
GROUP BY Name, Author, Year, User_Rating, NumReviews
HAVING COUNT(*) > 1
-- OUTPUT: 3 books have duplicate records
-- learned HAVING

--double check
SELECT *
FROM bestsellers_with_categories
WHERE Name = 'The Fault in Our Stars' OR Name = 'The Help' OR Name = 'Unbroken: A World War II Story of Survival, Resilience, and Redemption'
-- OUTPUT: OK it seems the books are probably just different versions such as paperback or hardback or ebook
-- Should have put the Price column in the  duplicate search to account for this

--Lets start analysis

-- Starting with the Name column, I wanted to find the most repeated words 
-- i wanted to see how worlds in titles correlate to book rating. Mostly for fun as I know correlation is not causation and any possible trends would most likely be a coincidence
-- so lets start with Author. How many Authors in this dataset?
SELECT COUNT( DISTINCT Author)
From bestsellers_with_categories
-- OUTPUT: 248
-- wow a lot more than I expected. Not sure why I expect the top 550 to be dominated by a few.

-- Lets find how top 10 authors with most books in top550
-- I will find all unique values in the column and sort them by how many they have
SELECT TOP 10 Author, COUNT(*) as NumberOfBooks, ROUND(AVG(User_Rating),2)
From bestsellers_with_categories
GROUP BY Author
ORDER BY NumberOfBooks DESC;
-- OUTPUT:  cool to see a couple authors I know on this list (#3,#4,#6). Turns out I also know #1,#2 but just his books not their names.
-- Turns out TOP has different syntax depending on the RDBMS you use (usually LIMIT)
-- im not sure if AVG(User_Rating) worked as I inted so I want to double check by getting all books of a certain author on that list and doing the AVG manually. 
-- Paranoid but I need to know if I did it right


-- Target Author: Rick Riordan, 11 books, 4.77272740277377 average
SELECT Author, ROUND(User_Rating,2), Name
FROM bestsellers_with_categories
WHERE Author = 'Rick Riordan'

SELECT ROUND(AVG(User_Rating),2)
FROM bestsellers_with_categories
WHERE Author = 'Rick Riordan'
-- OK that worked, both doing it manually and doing it seperately produced the result of 4.77272740277377 like in the original query.
-- So AVG does work the way I suspected

-- These amount of decimals are annoying so I went back and retroactively added the newly learned ROUND() function to the averages.
-- I dont wanna actively round them in the dataset itself, losing precision.

-- Next, lets see the number of review that led to those ratings. Want to have a significant amount of ratings so that the rating is statisically reliable.
-- top 10 lowest number of reviews should be a decent check, maybe also average number of reviews
SELECT TOP 10 *
From bestsellers_with_categories
ORDER BY NumReviews ASC;
-- outside the lowest at 37, most of them have quite at least 200. I like to imagine it as, if I asked 37 people about a movie (to rate it 1-5), and the average score was 4.6, id most likely see it
-- now the difference lies in, that the 37 people I would ask are people I know or friends of friends. Whereas these 37 are online random people I do not know. So I definitely would want more reviews before buying that book.
-- now personally I am an avid reader, so I know what I like and don't like. Online reviews alone wouldn't make me buy a book I'm not intially interested,
-- Anyways back to the data
SELECT AVG(NumReviews)
FROM bestsellers_with_categories
-- Not much to say here. Using AVG on a large set without a filter like Author to help give context is kind of useless.
-- nearly 12000 reviews on average for the top550 books on amazon from 2009-2019. Thats it. This really doesn't tell us much. Nice to know but not really actionable at the moment.

-- For more stuff to work with, we can do something like the following
SELECT AVG(topX.NumReviews)
FROM (
		SELECT TOP 25 PERCENT NumReviews
		FROM bestsellers_with_categories
		ORDER BY NumReviews DESC) as topX
-- OUTPUT: top 25% = 27858
-- If we change DESC to ASC, we can get the bottom 25% (= 2341)
-- SUCH a big difference between the top 25% and the bottom 25%. This paints a better picture versus just the normal average alone.

---- We can do similar stuff with Price like we did with NumReviews
SELECT AVG(Price)
FROM bestsellers_with_categories
---- OUTPUT: 13
SELECT AVG(topX.Price)
FROM (
		SELECT TOP 25 PERCENT Price
		FROM bestsellers_with_categories
		ORDER BY User_Rating DESC) as topX
-- OUTPUT: 10
-- See this paints a picture. The top 25% of books by User Rating are priced at $10.


-- FUTURE GOALS: Find 2020-2021 (finding 2022 data online easily wouldn't be realistic, its Dec 27 2022 rn) data to see and combine with this to see how COVID possibly affected peoples reading habits
-- VISUALIZATION GOALS: average rating by author, genre by author, price by author, price by rating, author by year??
-- LEARNED: ROUND(), HAVING, TOP, Qualifiers/Identifiers
