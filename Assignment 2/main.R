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
install.packages("sandwich")
library(sandwich)
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
summary(accident_pooled1)

#look at the coefficients of the model
kable(tidy(accident_pooled1), digits = 3, caption = "Pooled model")

# What happens to β1 if you estimate the same regression controlling for log population size
accident_pooled2 <- plm(I(log(accident)) ~ txmsban + I(log(pop)), model = "pooling", data = data)
summary(accident_pooled2)

#As we can see, with introduction of control, the intercept changes.

######## Exercise 2 ########

#fixed effects model
accident_FE <- plm(I(log(accident)) ~ txmsban, index = c("state", "time"), model = "within", effect = "individual", data = data)
summary(accident_FE)

######## Exercise 3 ########
FE <- plm(I(log(accident)) ~ txmsban + I(log(unemp)) + I(log(permale)) + I(log(rgastax)), index = c("state", "time"), model = "within", effect = "individual", data = data)
summary(FE)

######## Exercise 4 ########
FE <- plm(I(log(accident)) ~ txmsban + log(unemp) + log(permale) + log(rgastax) + as.numeric(time), index = c("state", "time"), model = "within", effect = "individual", data = data)
summary(FE)

######## Exercise 5 ########
# we can see a seasonal pattern, therefore a linear time trend is not enough to control for time.
average_accidents <- list()
for (t in 1:48) {
  average_accidents <- append(average_accidents, mean(data[data$time == t, "accident"]))
}
plot(1:48, average_accidents, xlab = "time", ylab = "Mean of accidents")

######## Exercise 6 ########
# we try to control for this seasonal pattern by including a dummy variable for each time period.
FE <- plm(I(log(accident)) ~ txmsban + log(unemp) + log(permale) + log(rgastax) + factor(time), index = c("state", "time"), model = "within", effect = "individual", data = data)
summary(FE)

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
sp <- ggplot(data = data_states, aes(x = time, y = I(log(accident)), colour = factor(state)))
sp +
  geom_vline(xintercept = 19) +
  geom_point(size = 2)

sp +
  geom_vline(xintercept = 19) +
  stat_summary(geom = 'line')

# bar chart
ggplot(data_states, aes(fill = factor(state), y = I(log(accident)), x = time)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_vline(xintercept = 19)

######## Exercise 10 ########
treat_state <- ifelse(data$state == 22, 1, 0)
post <- ifelse(data$txmsban == 0, 0, 1)

did_model <- lm(I(log(accident)) ~ treat_state + post + treat_state*post, data=data)
summary(did_model)

######## Part 3: Binary choice models ########
######## Exercise 11 ########
data_time <- data %>% filter(time == 1)

treated_ols_model <- lm(formula = treated ~ I(log(pop)) + I(log(accident)), data = data_time)
coeftest(treated_ols_model, vcov = vcovHC(treated_ols_model))

######## Exercise 12 ########
# Generate the predicted values for the first model and discuss them.
treated_ols_model$fitted.values
plot(data_time[data_time$state != 2 & data_time$state != 11, "treated"], treated_ols_model$fitted.values, xlab = "treated", ylab = "probability")
# Next, use Probit and Logit to estimate the same model specification as in question (11)
# Probit model
probit <- glm(treated ~ I(log(pop)) + I(log(accident)), family = binomial(link = "probit"), data_time)
summary(probit)
confint(probit)

# Marginal effect of each coefficient at the mean
probitmfx(probit, data_time, atmean = TRUE)

# Logit model
logit <- glm(treated ~ I(log(pop)) + I(log(accident)), family = binomial(link = "logit"), data_time)
summary(logit)
confint(logit)
# Marginal effect of each coefficient at the mean
logitmfx(logit, data_time, atmean = TRUE)

######## Exercise 13 ########
# Calculating average marginal effects of our Probit model (AME)
summary(margins(probit))
# Calculating  marginal effects of our Logit model (AME)
summary(margins(logit))

