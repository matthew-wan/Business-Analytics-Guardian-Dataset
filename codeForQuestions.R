#Load Libraries
library(ggplot2)
library(gridExtra)
library(cluster)
library(lattice)
############################################################################
#Part A)
#Create groups containing guardian score equal or above 70 and below 70
upperGuardianScore <- my.data[my.data$Guardian.score.100 >=70, ]
lowerGuardianScore <- my.data[my.data$Guardian.score.100 < 70, ]

#Calculate percentages for universities with score equal or above 70 and score below 70
totalUnis <- nrow(my.data)
numUpper <- nrow(upperGuardianScore)
numLower <- nrow(lowerGuardianScore)
percentUpper <- numUpper / totalUnis * 100 #Into percentage
percentLower <- numLower / totalUnis * 100
print(paste("The number of universities with a score of 70 and above is: ",numUpper,"Number of universities below 70 is: ",numLower))
print(paste("Percentage of universities with a score of 70 and above is: ",percentUpper,". Percentage of universities below 70 is: ", percentLower))


#Get proportion of Russell and non-Russell universities
over70uni <- my.data[my.data$Guardian.score.100 > 70, ] #Score above 70
propRussell <- table(over70uni$Russell) #(1 = Russell, 0 = Non-Russell)
propRussell
prop.table(propRussell) #Calculate proportions

#Create pie chart
pie(propRussell, main = "Proportion of Russell Universities with a score of 70+", labels = c(propRussell), col = rainbow(length(propRussell)))
legend("topright", c("Non-Russell University", "Russell University"), fill = rainbow(length(propRussell)))

############################################################################
#Part B)
#Histogram graph
hist(my.data$Average.Entry.Tariff, main = "Histogram of Average Entry Tariff", xlab = "Average Entry Tariff")
hist(my.data$Expenditure.per.student.FTE, main = "Histogram of Expenditure per Student FTE", xlab = "Expenditure per Student FTE")

#Numerical summary
dim(my.data)
names(my.data)
head(my.data)
class(my.data)
str(my.data)
summary(my.data$Expenditure.per.student.FTE)
sd(my.data$Expenditure.per.student.FTE)
summary(my.data$Average.Entry.Tariff)
sd(my.data$Average.Entry.Tariff)

#Create a scatter plot to show the association between Expenditure and Entry Tariff
ggplot(my.data, aes(x=Expenditure.per.student.FTE, y=Average.Entry.Tariff)) + geom_point() + 
  ggtitle("Association Between Average Expenditure and Average Entry Tariff") +
  xlab("Expenditure per student") + ylab("Average Entry Tariff") +
  geom_smooth(method=lm, color = "red")

############################################################################
#Part C)
a <- upperGuardianScore$Student.staff.ratio
b <- lowerGuardianScore$Student.staff.ratio
aboveAndRuss <- subset(my.data, Russell == 1 & Guardian.score.100 >70)
c <- aboveAndRuss$Student.staff.ratio
aboveAndNonRuss <- subset(my.data, Russell == 0 & Guardian.score.100 >70)
d <- aboveAndNonRuss$Student.staff.ratio
boxplot(a,b,c,d, main = "Student Staff Ratio", names = c("70 or above score","below 70","70+ Russell","70+ Non-Russ"))

e <- upperGuardianScore$Satisfied.with.course
f <- lowerGuardianScore$Satisfied.with.course
g <- aboveAndRuss$Satisfied.with.course
h <- aboveAndNonRuss$Satisfied.with.course
boxplot(e,f,g,h, main = "Satisfied With Course", names = c("70 or above score","below 70","70+ Russell","70+ Non-Russ"))

############################################################################
#Part D)
#Create subset for only Russell Universities
subsetRussell <- subset(my.data, Russell==1)

#Get number of unis in this subset
print(nrow(subsetRussell)) #There are 20 Russell Universities in this subset
over70Subset <- subset(subsetRussell, Guardian.score.100 > 70)
print(nrow(over70Subset)) #There are 9 Russell Universities with a Guardian Score above 70
notOver70Subset <- subset(subsetRussell, Guardian.score.100 < 71)
print(nrow(notOver70Subset)) #There are 11 Russell Universities with a Guardian Score NOT above 70

hist(over70Subset$Average.Entry.Tariff, col = "tomato", main = "Russell University over 70", xlab = "Average Entry Tariff")
hist(notOver70Subset$Average.Entry.Tariff, col = "turquoise", main = "Russell University below 70", xlab = "Average Entry Tariff")
hist(upperGuardianScore$Average.Entry.Tariff, col = "red", main = "All universities over 70", xlab = "Average Entry Tariff")
hist(lowerGuardianScore$Average.Entry.Tariff, col = "green", main = "All universities below 70", xlab = "Average Entry Tariff")

############################################################################
#Part E)
russellHierClustering <- subset(subsetRussell, select = -c(Name.Institution, Guardian.score.100, Country, Russell))
dimnames(russellHierClustering)[[1]] <- subsetRussell$Name.Institution
russellAgg = agnes(russellHierClustering, diss = FALSE, metric = "eucilidian", method = "single")
#Single Link Agglomerative Coefficient 0.68
#plot(russellAgg, main = "Single Link 9 Variables", which.plot = 2)
#plot(russellAgg, main = "Single Link 9 Variables", which.plot = 1, )
completeLink = agnes(russellHierClustering, diss = FALSE, metric = "eucilidian", method = "complete")
#Complete Link Agglomerative Coefficient 0.8
plot(completeLink, main = "Complete Link 9 Variables", which.plot = 2)
plot(completeLink, main = "Complete Link 9 Variables", which.plot = 1, )
avgLink = agnes(russellHierClustering, diss = FALSE, metric = "eucilidian", method = "average")
#Average Link Agglomerative Coefficient 0.75
#plot(avgLink, main = "Average Link 9 Variables", which.plot = 2)
#plot(avgLink, main = "Average Link 9 Variables", which.plot = 1, )

hcGroup <- cutree(completeLink, k = 3)
test <- as.data.frame(hcGroup)
russHCGrouped <- russellHierClustering
russHCGrouped <- cbind(russHCGrouped, hcGroup = test$hcGroup)


############################################################################
#Part F)
hcG1 <- subset(russHCGrouped, hcGroup == "1")
hcG2 <- subset(russHCGrouped, hcGroup == "2")
hcG3 <- subset(russHCGrouped, hcGroup == "3")
colMeans(hcG1)
colMeans(hcG2)
colMeans(hcG3)


############################################################################
#Part G)
dataKmeansPrep <-subset(my.data, select = -c(Name.Institution, Guardian.score.100, Country, Russell))

z.perc.between <- numeric(20)
for (i in 1:20) {
z <- kmeans(dataKmeansPrep, centers=i, nstart=10)
z.perc.between[i] <- 100*z$betweenss/z$totss
}

z.perc.between[1:20]

plot(1:20,z.perc.between[1:20],type="l",ylab="Percentage of variability between clusters",xlab="k")

dataKmeans <- kmeans(dataKmeansPrep, centers = 5, nstart = 10)

#Table for 70+ (True = above 70+, False = less than 70)
table(dataKmeans$cluster, my.data$Guardian.score.100 > 70)

#Table for Russell groups (1 = Russell, 0 = Non-Russell)
table(dataKmeans$cluster, my.data$Russell)

kmeansGroups <- dataKmeans$cluster
testK <- as.data.frame(kmeansGroups)

kmeansGrouped <- subset(my.data, select = -c(Name.Institution, Country, Russell))
dimnames(kmeansGrouped)[[1]] <- my.data$Name.Institution
kmeansGrouped <- cbind(kmeansGrouped, Kmeans.Group = testK$kmeansGroups)

kmeanG1 <- subset(kmeansGrouped, Kmeans.Group == "1")
kmeanG2 <- subset(kmeansGrouped, Kmeans.Group == "2")
kmeanG3 <- subset(kmeansGrouped, Kmeans.Group == "3")
kmeanG4 <- subset(kmeansGrouped, Kmeans.Group == "4")
kmeanG5 <- subset(kmeansGrouped, Kmeans.Group == "5")

colMeans(kmeanG1)
colMeans(kmeanG2)
colMeans(kmeanG3)
colMeans(kmeanG4)
colMeans(kmeanG5)

############################################################################
#Part H)
plot(dataKmeansPrep[,c("Average.Entry.Tariff","Satisfied.with.Assessment")],xlab="Average Entry Tariff",ylab="Satisfied with Assessment",type="n")

points(dataKmeansPrep[,c("Average.Entry.Tariff","Satisfied.with.Assessment")],pch=16,col=dataKmeans$cluster)
legend("topleft",legend=paste("Cluster",1:5),  pch=16,col=1:5)

points(dataKmeans$centers[,c(1,5)],pch="+",col=1:5,cex=2) 
legend("topright",legend=paste("Cluster Mean",1:5),  pch="+",col=1:5)
text(my.data[,c("Average.Entry.Tariff", "Satisfied.with.Assessment" )], labels = my.data$Name.Institution, col = dataKmeans$cluster, cex = .6)

############################################################################
#Part I)
mean(kmeanG1$Guardian.score.100) #Guardian score mean for group 1
mean(kmeanG2$Guardian.score.100) #Guardian score mean for group 2
mean(kmeanG3$Guardian.score.100) #Guardian score mean for group 3
mean(kmeanG4$Guardian.score.100) #Guardian score mean for group 4
mean(kmeanG5$Guardian.score.100) #Guardian score mean for group 5

bwplot(Kmeans.Group~Guardian.score.100, data = kmeansGrouped, main = "Distribution of Guardian Score across K means clusters")
bwplot(Kmeans.Group~Satisfied.with.Assessment, data = kmeansGrouped, main = "Distribution of Satisfaction with assessment across K means Clusters")
bwplot(Kmeans.Group~Average.Entry.Tariff, data = kmeansGrouped, main = "Distribution of Entry Tariff across K means clusters")
