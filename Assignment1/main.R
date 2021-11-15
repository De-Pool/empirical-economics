install.packages("arsenal")
install.packages("AER")
library(arsenal)
library(AER)

# Read data
data <- read.table("./Assignment1/data_assignment1.csv", header = TRUE,
                   sep = ",")
str(data)
summary(data)

# Opdracht 1
# default_dummy takes on 1 if default_option = normal, else it takes on a value of 0
data$default_dummy <- ifelse(data$default_option == "normal", 1, 0)
# sp_never > 0, therefore not all caseworkers complied with the default option
average_sp_never <- mean(data[data$default_dummy == 0, "searchperiod"])
average_sp_normal <- mean(data[data$default_dummy == 1, "searchperiod"])
cor.test(data$default_dummy, data$searchperiod)

# Opdracht 2
data_normal_sp0 <- data[data$default_dummy == 1 & data$searchperiod == 0, c("female", "age", "partner", "children", "years_education", "suminc_before_application")]
data_normal_sp1 <- data[data$default_dummy == 1 & data$searchperiod == 1, c("female", "age", "partner", "children", "years_education", "suminc_before_application")]
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
ols_12SLS <- lm(searchperiod ~ default_dummy, data = data)
summary(ols_12SLS)

# Opdracht 7
# 2nd stage using the fitted values for searchperiod from the first stage
x_hat <- ols_12SLS$fitted.values
ols_22SLS <- lm(totweeksbenefits26 ~ x_hat, data = data)
summary(ols_22SLS)

# Direct IV estimation using 2SLS, with and without control variables
iv_est_controls <- ivreg(totweeksbenefits26 ~ searchperiod + female + age + partner + children + years_education + suminc_before_application |
      default_dummy + female + age + partner + children + years_education + suminc_before_application, data = data)
summary(iv_est_controls)

iv_est_no_controls <- ivreg(totweeksbenefits26 ~ searchperiod | default_dummy, data = data)
summary(iv_est_no_controls)

# Opdracht 10
# 4 components, E[Yi|Zi=1], E[Yi|Zi=0], E[Xi|Zi=1], E[Xi|Zi=0]
E_Yi_Zi_1 <- mean(data[data$default_dummy == 1, "totweeksbenefits26"])
E_Yi_Zi_0 <- mean(data[data$default_dummy == 0, "totweeksbenefits26"])
E_Xi_Zi_1 <- mean(data[data$default_dummy == 1, "searchperiod"])
E_Xi_Zi_0 <- mean(data[data$default_dummy == 0, "searchperiod"])
B_wald <- (E_Yi_Zi_1 - E_Yi_Zi_0) / (E_Xi_Zi_1 - E_Xi_Zi_0)

# Opdracht 12
iv_est_week_job <- ivreg(totweekswage26 ~ searchperiod | default_dummy, data=data)
summary(iv_est_week_job)

iv_est_wage <- ivreg(totincome26 ~ searchperiod | default_dummy, data=data)
summary(iv_est_wage)

