#Shraddha Gupte, 26th January 2024 and ALY6000 Introduction to Analytics
library(janitor)
library(pacman)
library(lubridate)
library(tidyverse)
library(dplyr)
#cleaning the data set
books <- read.csv("books.csv")
books

#Question 1 - 'Janitor' package has been added above 
books <- clean_names(books)
books

#Question 2 - 'lubridate' package has been added above
first_publish_date<-mdy(books$first_publish_date)
first_publish_date

#Question 3 
books$year<-year(mdy(books$first_publish_date))
books$year

#Question 4 
books <- filter(books, year>=1990, year<=2020)
books

#Question 5
books <- subset(books, select = -c(publish_date, edition, characters, price, genres, setting, isbn))
books

#Question 6
books <- filter(books, pages<1200 )
books

#Question 7
books <-na.omit(books)
books

#Data Analysis

#Question 8
glimpse(books)

#Question 9
summary(books)

#Question 10
library(ggthemes)
ggplot(books, aes(x = rating))+geom_histogram(fill = "red", binwidth = 0.25)+labs(title = "Histogram of Book Ratings", x="Ratings", y="Number of Books")

#Question 11
ggplot(books, aes(x=pages))+geom_boxplot(fill = "magenta")+labs(title = "Box Plot of Page Counts")

#Question 12
by_year <- books %>%group_by(year) %>%summarise(total_books = n())
by_year

#Question 13
by_year<- filter(by_year, year>=1990, year<=2020)
by_year
ggplot(by_year, aes(x = year, y = total_books)) + geom_line() + labs(title = "Total Number of Books Rated Per Year")

#Question 14
book_publisher<-books%>%group_by(publisher)%>%summarise(book_count =n())
book_publisher

#Question 15
book_publisher <- filter(book_publisher, book_count>125)
book_publisher

#Question 16
book_publisher<-book_publisher[order(book_publisher$book_count,decreasing=TRUE),]
book_publisher

#Question 17
book_publisher <- book_publisher %>% mutate("cum_counts" = cumsum(book_count))
book_publisher

#Question 18
book_publisher<-book_publisher%>%mutate("rel_freq" = book_count/sum(book_count))
book_publisher

#Question 19
book_publisher<-book_publisher%>%mutate("cum_freq" = cumsum(rel_freq))
book_publisher

#Question 20
book_publisher$publisher <- factor(book_publisher$publisher, levels = book_publisher$publisher)
book_publisher

#Question 22
#Step 1 = Creating a new data frame on book formats having atleast above 4 rating and removing the books having unknown binding. 
book_format_df<- filter(books, book_format != 'Unknown Binding', rating>=4)
book_format_df
#Step 2 = Selecting the columns that are required for the analysis of the book_format_df data frame by using the 'subset' function
book_format_df<- subset(book_format_df, select= c(year, book_format, rating))
book_format_df
#Step 3 - selecting the trend 3 year from 2017 to 2020. 
book_format_df<-filter(book_format_df, year>=2017, year<=2020)
book_format_df
#Step 4 - Creating a column on unique_count by computing the book_format coulumn using the compute function. 
book_format_df <- book_format_df %>% group_by(book_format)%>% mutate(unique_count = n())
book_format_df
#Step 5 - Plotting the graph of the 'book_format_df' to get the number of book count as per the format they were printed in. 
ggplot(book_format_df, aes(x=book_format, y=unique_count))+geom_bar(stat = 'identity', fill = "blue")+labs(title="Total count as per the book format from 2017-2020")

#Question 23
#Answered in the Project Report
