#Create personal dataset
DATA <- read.csv("Guardian_University_Guide_2019_BMM.csv")

set.seed(202056726)    # replace yyyyabcde with your own student number.  You get your student #number when you log into Pegasus.  If you don’t know it but know your ds user number name

# which is like xyzabcde where abcde are digits then use 2021abcde

#this bit below adds a little random perturbation to the data to get your own data aset

z <- DATA[, 2:11]

z_cov <- cov(z)/100

#make sure you have the MASS library installed

library(MASS)

z_sim <- mvrnorm(n=nrow(z), mu=rep(0, nrow(z_cov)), Sigma=z_cov)

New_Data <- DATA

New_Data[,2:11] <- DATA[, 2:11] + z_sim  #adding the random perturbation

New_Data[,c(2,10)] <- round(New_Data[,c(2,10)], 1)  # rounding as in the original data

New_Data[,c(3:9,11)] <- round(New_Data[,c(3:9,11)], 0) # rounding as in the original data



#now we drop a few observations to give each student a different sized data set.

set.seed(202056726 + 1)    # replace yyyyabcde with your own student number, adding 1

n.sample.omit<- sample(5:10,1)

n.sample <- nrow(New_Data) - n.sample.omit

z.sel<- sample(1:nrow(New_Data),n.sample,replace=FALSE)  #get the row numbers for your data

#New_Data is the name of the data you read in and then modified - change the name if you did not use New_Data

my.data<- New_Data[z.sel,]  # you can use your own name for my.data here if you wish