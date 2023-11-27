library(readr)
diabetes <- read_csv("Downloads/diabetes.csv")
View(diabetes)
set.seed(42)
sample_data <- diabetes[sample(nrow(diabetes), 25), ]
sample_mean_glucose <- mean(sample_data$Glucose)
sample_highest_glucose <- max(sample_data$Glucose)
population_mean_glucose <- mean(diabetes$Glucose)
population_highest_glucose <- max(diabetes$Glucose)
barplot(
    c(sample_mean_glucose, sample_highest_glucose, population_mean_glucose, population_highest_glucose),
      names.arg = c("Sample Mean", "Sample Highest", "Population Mean", "Population Highest"),
      col = c("blue", "orange", "green", "red"),
      ylim = c(0, max(sample_highest_glucose, population_highest_glucose) + 20),
      ylab = "Glucose Value",
      main = "Comparison of Glucose Statistics"
   )
legend("topright",
                legend = c("Sample Mean", "Sample Highest", "Population Mean", "Population Highest"),
                fill = c("blue", "orange", "green", "red")
         )

sample_98th_percentile <- quantile(sample_data$BMI, 0.98)
population_98th_percentile <- quantile(diabetes$BMI, 0.98)
barplot(c(sample_98th_percentile, population_98th_percentile), 
                  names.arg = c("Sample 98th Percentile", "Population 98th Percentile"),
                  col = c("blue", "green"),
                  main = "Comparison of BMI 98th Percentile",
                  ylab = "BMI 98th Percentile")
 
  
bootstrap_sampling <- function(diabetes, n_samples=500, sample_size=150) {
        sample_statistics <- matrix(nrow=n_samples, ncol=3)
        
         for (i in 1:n_samples) {
                sample <- sample(diabetes, size=sample_size, replace=TRUE)
                sample_statistics[i, 1] <- mean(sample)
                sample_statistics[i, 2] <- sd(sample)
                sample_statistics[i, 3] <- quantile(sample, 0.5)
            }
        
          return(sample_statistics)
    }
blood_pressure_population <- diabetes$BloodPressure
bootstrap_samples <- bootstrap_sampling(blood_pressure_population)
population_mean_bp <- mean(blood_pressure_population)
population_std_bp <- sd(blood_pressure_population)
population_median_bp <- quantile(blood_pressure_population, 0.5)
par(mfrow=c(1, 3))
hist(bootstrap_samples[,1], main="Mean Comparison", xlab="Blood Pressure Mean", col="blue")
abline(v=population_mean_bp, col="red")

hist(bootstrap_samples[,2], main="Standard Deviation Comparison", xlab="Blood Pressure Std", col="green")
abline(v=population_std_bp, col="red")
 
hist(bootstrap_samples[,3], main="Median Comparison", xlab="Blood Pressure Median", col="orange")
abline(v=population_median_bp, col="red")