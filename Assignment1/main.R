install.packages("arsenal")
install.packages("AER")
library(arsenal)
library(AER)
# install.packages("ivreg")
# library(ivreg)
# Read data
data <- read.table("./Assignment1/data_assignment1.csv", header = TRUE,
                   sep = ",")
str(data)
summary(data)

# Opdracht 1
# default_dummy takes on 1 if default_option = normal, else it takes on a value of 0
data$defaultdummy <- ifelse(data$default_option == "normal", 1, 0)
# sp_never > 0, therefore not all caseworkers complied with the default option
average_sp_never <- mean(data[data$defaultdummy == 0, "searchperiod"])
average_sp_normal <- mean(data[data$defaultdummy == 1, "searchperiod"])

# Opdracht 2
data_normal_sp0 <- data[data$defaultdummy == 1 & data$searchperiod == 0, c("female", "age", "partner", "children", "years_education", "suminc_before_application")]
data_normal_sp1 <- data[data$defaultdummy == 1 & data$searchperiod == 1, c("female", "age", "partner", "children", "years_education", "suminc_before_application")]
summary(data_normal_sp0)
summary(data_normal_sp1)

# Opdracht 3
balancing_table <- tableby(default_option ~ female + age + partner + children + years_education + suminc_before_application, data = data)
summary(balancing_table, text = TRUE)

# Opdracht 5
# Y = benefits
# Zi = default_option (instrumental variable for searchperiod)
# X = searchperiod (we want to assess what effect having a search period has on Y -> benefits = a + B1*searchperiod + u)
# We use 2SLS to estimate B1
ols_12SLS <- lm(searchperiod ~ default_option, data = data)
summary(ols_12SLS)

# Opdracht 7
x_hat <- ols_12SLS$fitted.values
ols_22SLS <- lm(totweeksbenefits26 ~ x_hat, data = data)
summary(ols_22SLS)

# Direct IV estimation using 2SLS, with all control variables
iv_estimation <- ivreg(totweeksbenefits26 ~ searchperiod + female + age + partner + children + years_education + suminc_before_application |
      default_dummy + female + age + partner + children + years_education + suminc_before_application, data = data)
summary(iv_estimation)
# iv_estimation <- ivreg(totweeksbenefits26 ~ searchperiod | default_dummy, data = data)
# summary(iv_estimation)
