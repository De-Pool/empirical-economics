install.packages("arsenal")
library(arsenal)
# Read data
data <- read.table("./Assignment1/data_assignment1.csv", header = TRUE,
                   sep = ",")
str(data)
summary(data)

# Opdracht 1
data$default_option <- as.factor(data$default_option)
# sp_never > 0, therefore not all caseworkers complied with the default option
average_sp_never <- mean(data[data$default_option == "never", "searchperiod"])
average_sp_normal <- mean(data[data$default_option == "normal", "searchperiod"])

# Opdracht 2
data_normal_sp0 <- data[data$default_option == "normal" & data$searchperiod == 0, c("female", "age", "partner", "children", "years_education", "suminc_before_application")]
data_normal_sp1 <- data[data$default_option == "normal" & data$searchperiod == 1, c("female", "age", "partner", "children", "years_education", "suminc_before_application")]
summary(data_normal_sp0)
summary(data_normal_sp1)

# Opdracht 3
balancing_table <- tableby(default_option ~ female + age + partner + children + years_education + suminc_before_application, data=data)
summary(balancing_table, text=TRUE)
