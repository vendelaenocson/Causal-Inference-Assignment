# Causal-Inference-Assignment
---
title: "Causal Assignment 1"
format: html
editor: visual
---

1.2 Empirical Analysis: Preparation and Data InspectionWe will perform an empirical analysis of the effect of free school meals based on simulated data. This may seem strange to you, but working with simulated data is important to understand 3the behaviour of estimators and to compare different methods. Before we test our methods on real data, we want to know how well they work in a controlled environment.

a\) Please generate the following dataset using random number generators in R and save it as a data frame or tibble:

• set the seed to 123 set.seed(123) so that we all work with the same data.

• 500 observations

• a binary treatment variable D that is randomly assigned to 20% of the observations

• a covariate family_income that is normally distributed with a mean of 50,000 and a standard deviation of 10,000. The mean of family_income is 20,000 lower for the treated group.

• a covariate parent_education that is normally distributed with a mean of 12 and a standard deviation of 3. The mean of parent_education is 4 lower for the treated group.

• an error term 𝜀 that is normally distributed with a mean of 0 and a standard deviation of 5.

• Based on these variables, generate an outcome variable health as health = 50 + 5 ⋅ 𝐷 + 0.01 ⋅ family_income+ 0.5 ⋅ parent_education− 0.0005 ⋅ family_income ⋅ parent_education + 𝜀

```{r}
library(tidyverse)

set.seed(123)

n <- 500

# Treatment: 20% treated
D <- rbinom(n, 1, 0.2)

# Covariates
family_income <- rnorm(
  n,
  mean = 50000 - 20000 * D,
  sd = 10000
)

parent_education <- rnorm(
  n,
  mean = 12 - 4 * D,
  sd = 3
)

# Error term
epsilon <- rnorm(n, mean = 0, sd = 5)

# Outcome
health <- 50 +
  5 * D +
  0.01 * family_income +
  0.5 * parent_education -
  0.0005 * family_income * parent_education +
  epsilon

# Data frame / tibble
df <- tibble(
  D,
  family_income,
  parent_education,
  epsilon,
  health
)

view(df)
```

b\) Let’s first inspect the data. Produce two plots:

1\) a scatter plot of family income against health (y-axis), with separate regression lines and colours for treated and untreated observations

```{r}
ggplot(df, aes(x = family_income, y = health, color = factor(D))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Family income",
    y = "Health",
    color = "Treatment",
    title = "Health vs Family Income by Treatment Status"
  ) +
  theme_minimal()
```

There appears to be some common support between 20-50k there is no common support below 20k and above 50k.

2\) a scatter plot of parent education against health (y-axis), with separate regression lines and colours for treated and untreated observations. For each covariate, comment on common support.

```{r}
ggplot(df, aes(x = parent_education, y = health, color = factor(D))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Parent education",
    y = "Health",
    color = "Treatment",
    title = "Health vs Parent Education by Treatment Status"
  ) +
  theme_minimal()
```

Below 4 and above 17 there is no common support.

c\) Now produce separate density plots of each covariate, whereby each plot shows the distribution of the covariate for the treated and untreated group. Comment briefly on the difference between the distributions.

```{r}
ggplot(df, aes(x = family_income, fill = factor(D))) +
  geom_density(alpha = 0.4) +
  labs(
    x = "Family income",
    fill = "Treatment",
    title = "Distribution of Family Income by Treatment Status"
  ) +
  theme_minimal()
```

The density plot shows that the treated group (D = 1) has lower family income than the untreated group (D = 0). The distribution for treated observations is shifted to the left, indicating lower average income. However, the two distributions still overlap, meaning there is some common support between the groups. Both resemble a bell shape.

```{r}
ggplot(df, aes(x = parent_education, fill = factor(D))) +
  geom_density(alpha = 0.4) +
  labs(
    x = "Parent education",
    fill = "Treatment",
    title = "Distribution of Parent Education by Treatment Status"
  ) +
  theme_minimal()
```

The treated group (D = 1) is shifted to the left, indicating that parents of treated children have lower education levels on average. The untreated group (D = 0) has higher parental education on average. However, there is still some overlap between the two distributions, suggesting that there is some common support between treated and untreated observations. From the graph you can see that the treated group is right skewed. The mode of parental education for the untreated group is higher.

d\) Using a logit model, estimate the propensity score for each unit. Plot the distributions of the propensity scores for the treated and untreated group in a density plot. Comment briefly on the difference between both distributions.

```{r}
ps_model <- glm(
  D ~ family_income + parent_education,
  data = df,
  family = binomial(link = "logit")
)

df$pscore <- predict(ps_model, type = "response")
```

```{r}
ggplot(df, aes(x = pscore, fill = factor(D))) +
  geom_density(alpha = 0.4) +
  labs(
    x = "Propensity score",
    fill = "Treatment",
    title = "Distribution of Propensity Scores by Treatment Status"
  ) +
  theme_minimal()
```

There is clear difference between treated and control indicating unbalance between covariates.

1.3 The control group is very unlikely to be treated, whereas using characteristics of the treated group they are more likely to be treated. This comes from having very weak common support. Many observations would need to be trimmed during matching.

Empirical Analysis: Regression and Matching a) Run the following regressions and report the results in a regression table:

1\. A simple regression of health on the treatment variable D.

2.  A regression of health on the treatment variable D and the covariate family_income.
3.  A regression of health on the treatment variable D and the covariate parent_education.
4.  A regression of health on the treatment variable D, the covariate family_income and the covariate parent_education.
5.  A regression of health on the treatment variable D, the covariate family_income, the covariate parent_education and the interaction term between family_income and parent_education.

```{r}
reg1 <- lm(health ~ D, data=df)
reg2 <- lm(health ~ D + family_income, data=df)
reg3 <- lm(health ~ D + parent_education, data=df)
reg4 <- lm(health ~ D + family_income + parent_education, data=df)
reg5 <- lm(health ~ D + family_income + parent_education + I(family_income*parent_education), data=df)

library(stargazer)
stargazer(reg1, reg2, reg3, reg4, reg5,
          title="Results",
          type="text",
          out="table1.text",
          header=FALSE,
          no.space=TRUE)
```

b)  Consider the difference in the coefficient of the treatment between regressions 1 and 2. Using an appropriate formula, explain why the coefficients differ.

Because family income is a confounder, so by not including it in the first regression the OLS estimator on the treatment variable is biased, because it omits the family income. Assuming that the second regression is the true model and the bias in the first regression follows the OVB formula.

$$
\tilde{\beta_D}=\beta_D+\beta_{family \text{ } inome}*\frac{Cov(D,\text{ } family\text{ }income)}{Var(D)}
$$

```{r}
cov(df$D, df$family_income)
```

Based on the covariance we have calculated the coefficient in the first regression for the treatment is biased downward, which is consistent with what we observed in the results.

c\. Across all specifications, the coefficient of the treatment variable is closest to the assumed coefficient 𝛽 = 5 in regression 5. Explain why this is the case.

The interaction term modifies the effect of income and education and makes the marginal effect lower as the confounders increase.

![](http://127.0.0.1:46052/chunk_output/CAB610492120629f/96B3E761/cvr3udb7pv1iu/000013.png)

```{r}
# library(marginaleffects)
# 
# plot_slopes(reg5,
# variables = ,"parent_education",
# slope = "dydx",
# condition = c("family_income"))

```

d\. Perform propensity score matching (nearest neighbour) and obtain a matched dataset. Perform balancing tests for the covariates family_income and parent_education and comment on the results. I recommend the MatchIt package in R for this task.

```{r}
install.packages("MatchIt")
library(MatchIt)

matched_df <- matchit(
 D ~ pscore,
  data = df,
  method = "nearest",
 distance = "mahalanobis"
)
summary(matched_df)
```

e\. Inspect the covariate and propensity score balance visually. To do this, create density plots for each covariate and for the propensity score that show the distributions for treated and untreated units in the matched sample. Compare these to the distributions from 1.2 c) and 1.2 d).

f\. Using the matched dataset, estimate the ATT and compare your result to the results from the regressions.

```{r}
reg1 <- lm(health )
```
