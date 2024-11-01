library(ggplot2)
library(tidyverse)
library(haven)

setwd('/Users/jermaineatkins/Desktop/R')
getwd()
load("/Users/jermaineatkins/Desktop/R/Acs.2021.couples.RData")
summary(acs2021_couples)

"This data includes people who report they are a spouse or partner 
(latter category includes “partner, friend, or visitor”). That can be seen with 
`summary(acs2021_couples$RELATE)’. You might choose to restrict your analysis to 
one or the other."

"I’ve created a variable with the difference in age, which is the age of the 
spouse minus the age of the head:"
summary(acs2021_couples$RELATE)
acs2021_couples$age_diff <- acs2021_couples$AGE - acs2021_couples$h_age


"Check your understanding – if that’s negative, then which spouse is older? Head of Household


It’s worth noting that Census does not give any particular direction to 
respondents about choosing a reference person for the household (details), 
but does it look like people tend to make assumptions about that? 
You should run this code and discuss about what you can infer from the result:"

summary(acs2021_couples$age_diff[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Male")])

"when comparing ages between spouses and head of households, the min = -90 meaning the there is a 
90 age difference "

summary(acs2021_couples$age_diff[(acs2021_couples$SEX == "Male")&(acs2021_couples$h_sex == "Female")])

summary(acs2021_couples$age_diff[(acs2021_couples$SEX == "Male")&(acs2021_couples$h_sex == "Male")])
summary(acs2021_couples$age_diff[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Female")])

"Compare with this summary (for the first group)"

summary(acs2021_couples$AGE[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Male")])
summary(acs2021_couples$h_age[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Male")])

"(Maybe also check if you see some outliers…)

While it’s easy enough to work with age difference, it might also be interesting
to look at educational differences. Create a simple (but perhaps effective) measure
of years of education, then create educ_diff in an analogous way to age_diff."



acs2021_couples$educ_numeric <- fct_recode(acs2021_couples$EDUC,
                                           "0" = "N/A or no schooling",
                                           "2" = "Nursery school to grade 4",
                                           "6.5" = "Grade 5, 6, 7, or 8",
                                           "9" = "Grade 9",
                                           "10" = "Grade 10",
                                           "11" = "Grade 11",
                                           "12" = "Grade 12",
                                           "13" = "1 year of college",
                                           "14" = "2 years of college",
                                           "15" = "3 years of college",
                                           "16" = "4 years of college",
                                           "17" = "5+ years of college")

acs2021_couples$educ_numeric <- as.numeric(levels(acs2021_couples$educ_numeric))[acs2021_couples$educ_numeric]

acs2021_couples$h_educ_numeric <- fct_recode(acs2021_couples$h_educ,
                                             "0" = "N/A or no schooling",
                                             "2" = "Nursery school to grade 4",
                                             "6.5" = "Grade 5, 6, 7, or 8",
                                             "9" = "Grade 9",
                                             "10" = "Grade 10",
                                             "11" = "Grade 11",
                                             "12" = "Grade 12",
                                             "13" = "1 year of college",
                                             "14" = "2 years of college",
                                             "15" = "3 years of college",
                                             "16" = "4 years of college",
                                             "17" = "5+ years of college")

acs2021_couples$h_educ_numeric <- as.numeric(levels(acs2021_couples$h_educ_numeric))[acs2021_couples$h_educ_numeric]

acs2021_couples$educ_diff <- acs2021_couples$educ_numeric - acs2021_couples$h_educ_numeric


"Please run a regression where the dependent variable is the age difference and the independent
variables include education (of both partners) and their ages (including polynomial terms). 
Show some good complex hypothesis tests."

lmodel <- lm(age_diff ~ educ_numeric + h_educ_numeric + poly(h_age, degree = 2) 
             + poly(AGE, degree = 2), data = acs2021_couples)
summary(lmodel)

#Visualize Model

#Create a new data frame for predictions
prediction_data <- acs2021_couples %>%
  mutate(predicted_age_diff = predict(lmodel)) 

# Create the plot
ggplot(prediction_data, aes(x = h_age, y = predicted_age_diff, color = educ_numeric)) +
  geom_point(aes(y = age_diff), alpha = 0.3) + # Actual points
  geom_line() + # Predicted line
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Predicted Age Difference vs. Head of Household Age",
       x = "Head of Household Age",
       y = "Predicted Age Difference",
       color = "Partner's Education") +
  theme_minimal()

"the overall trend showsa decrease in the predicted age difference as 
the head of the household's age goes up. also educational attainment of the partner/spouse 
is associated wtih smaller age difference. This gives us the idea that older individuals 
prefer younger partners "

# Hypothesis test for coefficients
summary(lmodel)  # This provides p-values for coefficients

# Example: Testing if the coefficient of educ_numeric is significantly different from zero
# Null Hypothesis (H0): β1 = 0 (no effect)
# Alternative Hypothesis (H1): β1 ≠ 0 (effect exists)

# Extract p-value for educ_numeric
p_value_educ <- summary(lmodel)$coefficients["educ_numeric", "Pr(>|t|)"]

if (p_value_educ < 0.05) {
  result_educ <- "Reject the null hypothesis: educational attainment has a significant effect."
} else {
  result_educ <- "Fail to reject the null hypothesis: educational attainment does not have a significant effect."
}

# Repeat for h_educ_numeric
p_value_h_educ <- summary(lmodel)$coefficients["h_educ_numeric", "Pr(>|t|)"]

if (p_value_h_educ < 0.05) {
  result_h_educ <- "Reject the null hypothesis: head of household's education has a significant effect."
} else {
  result_h_educ <- "Fail to reject the null hypothesis: head of household's education does not have a significant effect."
}

# Output results
result_educ
result_h_educ

"The p value is alot smaller than the significance level of .05 indicating evidence against the null.This tells us
that the educational attainment of the spouse/partner or the head of the household is likely a significant predictor of the age difference 
between the two."

"Articles 1 :Roubaud, David, and Muhammad Shahbaz. “FINANCIAL DEVELOPMENT, ECONOMIC GROWTH, AND ELECTRICITY DEMAND: A SECTOR ANALYSIS OF AN EMERGING ECONOMY.” The Journal of Energy and Development, 
vol. 43, no. 1/2, 2017, pp. 47–98. JSTOR, https://www.jstor.org/stable/26539568. Accessed 1 Nov. 2024.

 Article 2 : Metcalf, Gilbert E. “An Empirical Analysis of Energy Intensity and Its Determinants at the State Level.” The Energy Journal, 
 vol. 29, no. 3, 2008, pp. 1–26. JSTOR, http://www.jstor.org/stable/41323167. Accessed 1 Nov. 2024."









