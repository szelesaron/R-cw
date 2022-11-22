library(Pareto)
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
sprintf("Standard deviation: %f",sd(data$x))





##########QUESTION 5##########
#pareto_sim <- rPareto(1000, 1000, 2.7931)
#hist(pareto_sim, breaks = 500, main = "Simulated Pareto distribution")


#run it 1000 times and record 
y_prime = c()
for (i in 1:10000) {
  x_i_prime <- rPareto(1000, 1000, 2.7931)
  y_prime <- append(y_prime, mean(x_i_prime))
}

#plot hist
x <- y_prime
hist(x, freq = FALSE, breaks = 100, main = "Histogram of distribution of Y' with fitted curve.") 
curve(dnorm(x, mean = mean(y_prime), sd = sd(y_prime)), add = TRUE, col = "blue", lwd = 2)

#get statistics
summary(y_prime)
sprintf("Standard deviation: %f",sd(y_prime))



##########QUESTION 6##########
#cdf <- pPareto(1700, 1000, 2.793)
upper_interval_bound = -1

for (i in 1:5000) {
  cdf <- pPareto(1000+i, 1000, 2.793)
  if (cdf >= 0.99)
  {
    upper_interval_bound = 1000+i
    break
  }
} 
sprintf("Upper interval bound: %d",upper_interval_bound)
sprintf("Confidence interval: %f",pPareto(upper_interval_bound, 1000, 2.793))




