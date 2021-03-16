#reading dataset from the given URL link given.
link <- "https://github.com/SavioSal/datasets/raw/master/Bank%20Churn_Modelling.csv"
#reading/assigning data to variable called info
info <- read.csv(link)
#calling variable info
info
#calling installed libraries ggplot2 and dplyr
#ggplot2 is a data visualization package for the statistical programming language R.
#It is a general scheme for data visualization which breaks up graphs into semantic components such as scales and layers.
library(ggplot2)
#dplyr is a grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data manipulation challenges:
#mutate() adds new variables that are functions of existing variables
#select() picks variables based on their names.
#filter() picks cases based on their values.
#summarize() reduces multiple values down to a single summary.
#arrange() changes the ordering of the rows.
library(dplyr)
#using chain progrma(ctrl+shift+M). We are removing unwanted columns in the following steps.
#assigning info data from the top into new variable i.e. info_1
info_1<-info %>%
  dplyr::select(-RowNumber, -CustomerId, -Surname) %>% #remove unwanted column 
  mutate(Geography = as.factor(Geography),
         Gender = as.factor(Gender),
         HasCrCard = as.factor(HasCrCard),
         IsActiveMember = as.factor(IsActiveMember),
         Exited = as.factor(Exited),
         Tenure = as.factor(Tenure),
         NumOfProducts = as.factor(NumOfProducts))


#Develop answers to the following. Use dplyr wherever necessary: 

#A. What is the average credit score of females and males in France?
#we are given a task to find the average credit score of male and females in france.
#using chain operation we are selecting creditscore, male and female because we want them inorder to predict result
#as mentioned we are supposed to find the average in only france region so we used filter verb to do so.
#as we are asked to do average we used summarize verb here.
info_1 %>% select(CreditScore, Gender, Geography) %>% filter(Geography == "France") %>%
  dplyr::group_by(Gender) %>%
  dplyr::summarise(Gender_Average = mean(CreditScore))

#B. What is the average credit score of people in the age brackets 20-30,31-40,41-50?
#as we are supposed to give average credit score based on their age group we are assigning case to each group.
#We gave age group less than 30 in 1 and we assigned age group leess than 40 into 2 and less than 50 into 3
# we used and operator because we want every case to come true then only print average
info_1 %>% select(CreditScore, Age) %>% mutate(agegroup = case_when(Age >= 41  & Age <= 50 ~ '3', Age >= 31  & Age <= 40 ~ '2', Age >= 20  & Age <= 30 ~ '1'))%>%
  filter(agegroup == "1" | agegroup == '2' | agegroup == '3') %>%
  dplyr::group_by(agegroup) %>%
  dplyr::summarise(Age_Average = mean(CreditScore))

#C. What is the correlation between credit score and estimated salary? 
# as we are given a task to find correlation between credit score and estimated salary.
# We used correlation function after selecting Credit score and estimatedsalary columns.
info_1 %>% select(CreditScore, EstimatedSalary) %>% cor()

#D. Develop a statistical model to explain and establish a mathematical relationship between credit score (dependent) and gender, age, estimate salary.

# We are trying to build a mathematical relation between the given variables.
#Here creditscore is a dependent variable
#gender is a categorical variable
#age and estimated salary are independent variable
model <- lm(CreditScore ~Gender+Age+EstimatedSalary, data = info_1)

# printing and summarizing data
print(model)
summary(model)
