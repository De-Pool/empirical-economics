install.packages("arsenal")
install.packages("AER")
library(arsenal)
library(AER)

# Read data
data <- read.table("./Assignment2/textingbans.csv", header = TRUE,
                   sep = ",")
str(data)
summary(data)
