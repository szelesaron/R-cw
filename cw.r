#read in file
data = read.csv("C:/Users/Áron/Google Drive/UNI/Y4S1/Statistical Modelling and Analysis/CW/filesize.csv")

##########QUESTION 1##########
#plots
hist(data$x, freq = FALSE, breaks = 1000, 
     main = "Distribition of values (whole range)  with break = 1000", xlab = "File size (kB)")

hist(data$x, freq = FALSE, breaks = 1000, xlim=c(1000,10000), 
     main = "Distribition of values (range 1000- 20000) with break = 1000", xlab = "File size (kB)")

boxplot(data, outline = FALSE, col = "YELLOW", xlab = "File size (kB)", horizontal = TRUE,
        main = "Boxplot of values - outliers filtered", notch = TRUE)

boxplot(data, outline = TRUE, col = "YELLOW", xlab = "File size (kB)", horizontal = TRUE,
        main = "Boxplot of values - outliers not filtered", notch = TRUE)

#basic data summary - min, 1st q, median, mean, 3rd q, max, std
summary(data$x)
sprintf("Standard deviation: %f",sapply(data, sd))



##########QUESTION 5##########
library(Pareto)

pareto <- dPareto(data$x, 1000 ,2.7931)
hist(pareto)

for (i in 1:1000) {
  pareto <- dPareto(data$x, 1000 ,2.7931)
  hist(pareto)
}

mean(pareto)
sum(log(data$x))





