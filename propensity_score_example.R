
# Estimating propensity scores with logistic regression

# load packages
library(tidyverse)
library("devtools")
devtools::install_github("alanbrookhart/NAMCS")
library(NAMCS)
library(tableone)
library(survey)
library(knitr)
library(geepack)
library(broom)

# Viewing the NSAID data

View(ns)


# Create a simple Table 1

factor_vars = c("year", "region", "arthritis", "asthma", "cancer", "cerebrovascular_disease", "chronic_kidney_disease", "heart_failure", "chronic_pulmonary_disease", "depression", "diabetes", "hyperlipidemia", "hypertension", "coronory_artery_disease", "osteoporosis",  "anti_hypertensive_use", "statin_use", "h2_antagonist_use", "ppi_use", "aspirin_use", "anti_coagulant_use", "corticosteroid_use", "sex", "race", "incident_pud")

table1 = CreateTableOne(data = ns, vars = c("age", factor_vars), factorVars = factor_vars,
                        strata = "cox2_initiation", test = FALSE, smd = TRUE)

print(table1, smd = TRUE, printToggle = FALSE) %>% kable()


# Fit a propensity score model and display parameter estimates

ps_model = glm(cox2_initiation ~ age + race + sex + arthritis + aspirin_use + anti_coagulant_use + corticosteroid_use + arthritis, data = ns, family = binomial)
summary(ps_model)

# Add the propensity score to a new analytic data set

ns_analy = ns
ns_analy$ps <- predict(ps_model, type = "response")


# Plot the distribution of PS, by treatment group

ggplot(data = ns_analy, aes(x = ps, group = cox2_initiation, fill = cox2_initiation)) +
  geom_histogram(
    aes(y = ..density..),
    color = "white",
    alpha = 0.5,
    binwidth = 0.01,
    position = position_dodge(width = 0.01/2)) + theme_bw() +
  guides(colour = FALSE,
         linetype = FALSE)


# Fit a logistic regression of the outcome

glm_unweighted = glm(incident_pud ~ cox2_initiation, data = ns_analy, family = binomial)
summary(glm_unweighted)
tidy(glm_unweighted)


# Compute IPTW

ns_analy$iptw = I(ns[["cox2_initiation"]] == "Yes") / ns_analy$ps +
  (I(ns[["cox2_initiation"]] == "No")) / (1 - ns_analy$ps)


# Summarize weights by treatment groups

summary(ns_analy$iptw[ns_analy$cox2_initiation == "Yes"],)
summary(ns_analy$iptw[ns_analy$cox2_initiation == "No"],)


# Create weighted table 1
To determine if the IPTWs have appropriately balanced covariates, we can construct a weighted Table 1.  We see that the variables included in the PS model appear to be well balanced.

ns.svy = svydesign(ids = ~ 1, data = ns_analy,
                   weights = ~ iptw)

ipw_table1 = svyCreateTableOne(vars = factor_vars, factorVars = factor_vars,
                               strata = "cox2_initiation",
                               data = ns.svy, test = FALSE, smd = TRUE)

print(ipw_table1, smd = TRUE)


# Estimate IPTW regression of outcome on treatment

glm_weighted = glm(incident_pud ~ cox2_initiation, data = ns_analy, family = binomial,
                   weights = iptw)
summary(glm_weighted)



# Compute SMRW

ns_analy$smrw= I(ns[["cox2_initiation"]] == "Yes") +
  (1 - I(ns[["cox2_initiation"]] == "Yes")) * ns_analy$ps / (1-ns_analy$ps)


# Summarize weights by treatment groups

summary(ns_analy$smrw[ns_analy$cox2_initiation == "Yes"],)
summary(ns_analy$smrw[ns_analy$cox2_initiation == "No"],)


## Compute weighted table 1

ns.svy = svydesign(ids = ~ 1, data = ns_analy,
                   weights = ~ smrw)

smrw_table1 = ipw_table1 = svyCreateTableOne(vars = factor_vars, factorVars = factor_vars,
                                             strata = "cox2_initiation",
                                             data = ns.svy, test = FALSE, smd = TRUE)

print(smrw_table1, smd = TRUE)


# Estimate an SMRW regression of the outcome on treatment

glm_weighted = glm(incident_pud ~ cox2_initiation, data = ns_analy, family = binomial,
                   weights = smrw)
summary(glm_weighted)


# Getting correct confidence intervals for IPTW / SMRW estimates

ns_analy$.id = seq(1, nrow(ns_analy))
ns_analy$y = ifelse(ns$incident_pud == "Yes", 1, 0)
gee_weighted = geeglm(y ~ cox2_initiation, data = ns_analy, weights = smrw, id=.id, family = binomial)
summary(gee_weighted)

