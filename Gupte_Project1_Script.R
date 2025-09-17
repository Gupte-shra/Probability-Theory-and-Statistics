#Shraddha Gupte, 25th January 2024 and ALY6000 Introduction to Analytics
#Question 1
123 * 453
5^2 * 40
TRUE & FALSE
TRUE | FALSE
75 %% 10
75 / 10

#Question 2
first_vector <- c(17,12,-33,5)

#Question 3
counting_by_fives <- c(5,10,15,20,25,30,35)

#Question 4
second_vector <- (20:1)

#Question 5
counting_vector <-(5:15)

#Question 6
grades <- c(96,100,85,92,81,72)

#Question 7
bonus_points_added <-(grades + 3)

#Question 8
one_to_one_hundred <- c(1:100)

#Question 9
# + means "add"
second_vector + 20
# * means "multiply"
second_vector * 20
# >= means "less than equal to"
second_vector >= 20
# != means "not equal"
second_vector != 20

#Question 10
total <- sum(one_to_one_hundred)

#Question 11
average_value <- mean(one_to_one_hundred)

#Question 12
median_value <- median(one_to_one_hundred)

#Question 13
max_value <- max(one_to_one_hundred)

#Question 14
min_value <- min(one_to_one_hundred)

#Question 15
first_value <- second_vector[1]

#Question 16
first_three_values <- second_vector[1:3]

#Question 17
vector_from_brackets <- second_vector[c(1,5,10,11)]

#Question 18
vector_from_boolean_brackets <- first_vector[c(FALSE,TRUE,FALSE,TRUE)]
#vector_from_boolean_brackets picks values from first_vector and assigns them in a logical R objects. 
# FALSE: denotes not to take the corresponding values from the first_vector. i.e 17 and -33
# TRUE: denotes to take the corresponding values from the first_vector. i.e 12 and 5

#Question 19
second_vector >=10
#This operation took all the elements in second_vector that are greater than or equal to 10 and assigned them for ‘TRUE’.

#Question 20
one_to_one_hundred[one_to_one_hundred >= 20]
#This operation showing all the elements that are greater than or equal to 20 from the vector named ‘one_to_one_hundred’

#Question 21
lowest_grades_removed <- grades[grades >85]

#Question 22
middle_grades_removed <- grades[-c(3,4)]

#Question 23
fifth_vector <- second_vector[-c(5,10)]

#Question 24
set.seed(5)
random_vector <- runif(n=10, min = 0, max = 1000)

#Question 25
sum_vector <- sum(random_vector)

#Question 26
cumsum_vector <- cumsum(random_vector)

#Question 27
mean_vector <- mean(random_vector)

#Question 28
sd_vector <- sd(random_vector)

#Question 29
round_vector <- round(random_vector)

#Question 30
sort_vector <- sort(random_vector)

#Question 31
# For answer refer attached Project1 report

#Question 32
file_path <- "/Users/shraddha/Desktop/MPSA - SHRADDHA GUPTE/Introduction to Analytics ALY6000/Module 1/Gupte-Project1/ds_salaries.csv"
first_dataframe <- read.csv(file_path)

#Question 33
summary(first_dataframe)

