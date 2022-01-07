#setting work directory
setwd("D:/Msc Statistical Science/Computational Statistics")

#Question 1
#Use the file "Women.txt" from the course website and read this into R. What is the dimension of the data you have just read in R?
  
#Reading the .txt file into R,

women<- read.table("Women.txt", header=TRUE)

#Next, check the dimension of this table. We may also go further ahead and show the first and last three items from the table. We'll realise that the table has 17 rows and 3 columns. This data can be easily viewed

dim(women)
head(women,3)
tail(women,3)
View(women)

#Use the file "Women.txt" from the course website and read this into R. A new woman joined the study, she is 66" tall, 165lbs and 34 years. Append this information to your data.

new.woman<-data.frame(height="66",weight="165",age="34")
women1<-rbind(women,new.woman)
women1

#Use the file "Women.txt" from the course website and read this into R. How many women have a weight under 140?

with(women,women$weight<140) #From this, It returns the first 9 values as less than 140 and the rest as greater than. 

#We can obtain similar results by suing the below function, as the above one is quite tedious ovr manually counting the TRUE values from the output;

sum(women$weight<140)

#(iv) Use the file "Women.txt" from the course website and read this into R. There is a correction to the woman in row D, her age should be 39. Change the age in row D to 39. You're then required to sort your data by weight and store the results in a new data set.

women<- read.table("Women.txt", header=TRUE)
women2<-replace(my_data$age,4,39)
women2

#(v)Use apply to generate a summary report, with the mean, median, sd of height, weight and age.

#The question seeks the mean,median and standard deviation using apply function This function is mainly used to avoid loops. It takes three arguments; x-dataframe Margin-1 is for row operations and 2 column operations FUN- the function to be applied,e.g mean,median and sd

#But before applying the function, there is need to omit incomplete cases if they exist.

attach(women)
women2<-na.omit(women)
mean<-apply(women2,2,FUN = ("mean"))
median<-apply(women2,2,FUN="median")
sd<-apply(women2,2,FUN="sd")

#(vi)Use the apply function to get the mean, median and sd of the columns and create a matrix with row names, mean, median and sd.

t(data.frame(mean,median,sd))

#(vii)Write a function to calculate BMI.

attach(women)

BMI = function(height,weight){(0.45455*weight/(.0254*height)^2)}
Women_BMI<- BMI(height,weight)
Women_BMI

#(viii)Do the women have a BMI within a recommended range for their height (Normal =18.5-24.9)? You require an R code to answer this.

#Using the sum function and logic and (&) operator to get the count of values within the normal range.

sum(Women_BMI>18.5 & Women_BMI<24.90)

#This means all the 17 women are within the normal range.

#Question 2
#(i) You're required to load the library "ape" and then read the data bank as follows>bank<-table(read.GenBank(c("X94991.1"), as.character=TRUE)) You're then required to produce a pie char of the data bank in both 2D and 3D. Label your pie chart appropriately.

install.packages("ape")
install.packages("plotrix")
library(ape)
library(plotrix)
bank<-table(read.GenBank(c("X94991.1"), as.character=TRUE))
bank

colors = c('#4286f4','#bb3af2','#ed2f52','#efc023')
labels=c("A","B","C","D")
pie(bank, labels, main='GenBank', col=colors, init.angle=180, clockwise=TRUE)


colors = c('#4286f4','#bb3af2','#ed2f52','#efc023')
labels=c("A","B","C","D")
pie3D(bank, labels=labels, explode=0.1, height=0.05, main='Genbank 3D Piechart', col=colors)


#(ii) Use the file "gapminder.csv" from the course website and read this into R. You're then required to perform the following analysis:
setwd("D:/Msc Statistical Science/Computational Statistics/Data")
data_set<-read.csv("gapminder.csv")
str(data_set)

head(data_set)

#(a) Obtain data set for only the year 1982

data_1982<- subset(data_set,subset = year==1982)
head(data_1982)

str(data_1982)

#(b) Obtain data set for the American countries in 1997

data_America<-subset(data_set,continent == "Americas"& year==1997)
head(data_America)

#(c) Add a column to the data set loaded in R with the new column being gdp which is computed as gdp=popgdpPercap*

attach(data_set)
GDP<-function(pop,gdpPercap){pop*gdpPercap}

gdp<-GDP(pop,gdpPercap)
head(gdp)

data_set_1<- cbind(data_set,gdp)
str(data_set_1)
