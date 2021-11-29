install.packages("arsenal")
install.packages("AER")
install.packages("plm")
install.packages("knitr")
install.packages("broom")
install.packages("dyplr")
install.packages("ggplot2")
install.packages("mfx")
install.packages("robustbase")
install.packages("margins")
library(robustbase)
library(mfx)
library(arsenal)
library(AER)
library(plm)
library(knitr)
library(broom)
library(dplyr)
library(ggplot2)
library(margins)

# Read data
data <- read.table("./Assignment 2/textingbans.csv", header = TRUE,
                   sep = ",")
str(data)
summary(data)

######## Part 1: Panel Data Models ########
######## Exercise 1 ########

#pooled model for log(accident)
accident_pooled1 <- plm(I(log(accident)) ~ txmsban, index = c("state", "time"), model = "pooling", data = data)

#look at the coefficients of the model
kable(tidy(accident_pooled1), digits = 3, caption = "Pooled model")
summary(accident_pooled1)

# What happens to β1 if you estimate the same regression controlling for log population size
accident_pooled2 <- plm(I(log(accident)) ~ txmsban + I(log(pop)), model = "pooling", data = data)
summary(accident_pooled2)

#As we can see, with introduction of control, the intercept changes.

######## Exercise 2 ########

#fixed effects model
accident_FE <- plm(I(log(accident)) ~ txmsban, index = c("state", "time"), model = "within", effect = "time", data = data)
summary(accident_FE)

######## Exercise 3 ########
######## Exercise 4 ########
######## Exercise 5 ########
######## Exercise 6 ########
######## Exercise 7 ########
######## Part 2: Difference in Differences ########

#look at the data for states with sort off the same population size
data %>%
  group_by(state, treated) %>%
  summarize(mean_pop = mean(pop))

# investigate the chosen groups
# State that never introduced a texting ban = state 45 (control state)
# State that introduced a texting ban at some point during the observation period = state 22 (treatment state)

data_states <- data %>%
  filter(state == 22 | state == 45) %>%
  group_by(state)

######## Exercise 8 ########

data %>%
  filter(state == 22 | state == 45) %>%
  group_by(state) %>%
  summarize(mean_pop = mean(pop), mean_permale = mean(permale), mean_unemp = mean(unemp), mean_rgastax = mean(rgastax))

data %>%
  filter(state == 22 | state == 45) %>%
  group_by(state) %>%
  summarize(pop, permale, unemp, rgastax)

######## Exercise 9 ########
# plot
sp <- ggplot(data = data_states, aes(x = time, y = I(log(accident))))
sp +
  geom_vline(xintercept = 19) +
  geom_point(aes(colour = factor(state)), size = 2)

# bar chart
ggplot(data_states, aes(fill = factor(state), y = I(log(accident)), x = time)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_vline(xintercept = 19)

######## Exercise 10 ########
treat_state <- ifelse(data$state == 22, 1, 0)
post <- ifelse(data$txmsban == 0, 0, 1)

#treat_state <- ifelse(data_states$state == 22, 1, 0)
#post <- ifelse(data_states$txmsban == 0, 0, 1)


######## Part 3: Binary choice models ########
######## Exercise 11 ########
data_time <- data %>% filter(time == 1)

treated_ols <- lm(treated ~ I(log(pop)) + I(log(accident)), data_time)
summary(treated_ols)

# "Make sure to use the right standard errors" --> robust??
treated_ols_robust <- lmrob(treated ~ I(log(pop)) + I(log(accident)), data_time)
summary(treated_ols_robust)


######## Exercise 12 ########
# Generate the predicted values for the first model and discuss them.
treated_ols$fitted.values
# Next, use Probit and Logit to estimate the same model specification as in question (11)
# Probit model
probit <- glm(treated ~ I(log(pop)) + I(log(accident)), family = binomial(link = "probit"), data_time)
summary(probit)
confint(probit)
# Marginal effect of each coefficient at the mean
marg_probit <- margins(probit, atmeans=T)
summary(marg_probit)

# Logit model
logit <- glm(treated ~ I(log(pop)) + I(log(accident)), family = "binomial", data_time)
summary(logit)
confint(logit)
# Marginal effect of each coefficient at the mean
marg_logit <- margins(logit, atmeans=T)
summary(marg_logit)

######## Exercise 13 ########
# Marginal effect Probit
probitmfx(treated ~ I(log(pop)) + I(log(accident)), data_time)
margins(probit)

# Marginal effect Logit
logitmfx(treated ~ I(log(pop)) + I(log(accident)), data_time)
margins(logit)

######## Exercise 14 ########
######## Exercise 15 ########
