# Assignment 3 Week 2
# Holford, Megan
# 2020-09-12

# Import the scores.csv data.
scores_df <- read.csv("data/scores.csv")

# Create one variable to hold a subset of your data set that contains only the 
# Regular Section and one variable for the Sports Section.
sports_sect <- subset(scores_df, Section=="Sports")
reg_sect <- subset(scores_df, Section=="Regular")

#Use the Plot function to plot each Sections scores and the number of students achieving that score. 
Score1=sports_sect[,2]
Count1=sports_sect[,1]
Score2=reg_sect[,2]
Count2=reg_sect[,1]
par(mfrow=c(2,1))
plot(Score1, Count1, xlab="Score", ylab="Number of Students", main="Sports Section", type = "p", col = "blue")
plot(Score2, Count2, xlab="Score", ylab="Number of Students", main="Regular Section", type="p", col="red")

plot(Score1, Count1, xlab="Score", ylab="Number of Students", main="Sports Section", type = "l", col = "blue")
plot(Score2, Count2, xlab="Score", ylab="Number of Students", main="Regular Section", type="l", col="red")

mean(Score1)
mean(Score2)

median(Score1)
median(Score2)

