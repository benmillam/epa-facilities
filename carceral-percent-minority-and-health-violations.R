#---
#title: Carceral SWDA - how to represent percent minority and health violations
#author: Ben Millam
#date: August 12, 2021
#description: I was asked to create a graphic to illustrate the group's finding that percent minority 
#   is associated with an increase in odds of a SWDA health violation.
#   First, though, understand the data and attempt to recreate the finding.
#
#   Initial results show finding not confirmed; escalating to group.
#   
#references:
#   SDWA_Violations_Site_Visits_lag_collapse.csv accessed 2021-08-12 from ECHO Data Analysis > !GG! > Spreadsheets > 2021-02 Notebook
#   
#---


setwd('C:\\Users\\bmillam\\Downloads\\carceral')

swda <- read_csv('SDWA_Violations_Site_Visits_lag_collapse.csv', col_types = cols(.default = "c"))

#[1] "FISCAL_YEAR"             "FAC_ID"                 
#[3] "POPULATION_SERVED_COUNT" "REGISTRY_ID"            
#[5] "FAC_PERCENT_MINORITY"    "FAC_POP_DEN"            
#[7] "ACUTE_yes"               "HEALTH_yes"             
#[9] "MONITOR_yes"             "PUBLIC_yes"             
#[11] "VISIT_yes"               "STATE_NUM"              
#[13] "VISIT_Lag" 

#initial vars of interest: FAC_PERCENT_MINORITY, HEALTH_yes

#unit of observation: year x SWDA ID for carceral facilities
#unit of analysis:    carceral facility by FRS ID
    # --Q1->  Unsure if group's analysis accounted for this 'mismatch' in UoO vs UoA, e.g. 
    #         there are many FRS IDs with > 1 SWDA ID, view here:
View (
  swda %>% 
    group_by (
      REGISTRY_ID,
      FAC_ID
    ) %>% 
    tally() %>% 
    arrange(
      REGISTRY_ID
    )
)

#again, initial vars of interest: FAC_PERCENT_MINORITY, HEALTH_yes

#assess missingness, relative to our UoA (FRS ID...)
    # --Q2->  We find 0.45 of FRS IDs are missing percent_minority... ....consult with group for sanity check
swda %>% 
  group_by(
    REGISTRY_ID
  ) %>% 
  summarize(
    per_minority = max(FAC_PERCENT_MINORITY)  #var should already be at REGISTRY_ID level, but drop the years
                                              #this will pick up NAs, max() applied at group level
  ) %>% 
  with(
    .,
    mean(
    is.na(.$per_minority)
    )
  ) #0.452

#no missing values for HEALTH_yes
swda %>% 
  group_by(
    REGISTRY_ID
  ) %>% 
  summarize(
    yes_health = if_else(
                    condition = sum(as.numeric(HEALTH_yes)) > 0, #HEALTH_yes is 0/1 at SWDA ID level, sum to get FRS ID level
                    true      = 1,
                    false     = 0
                  )
  ) %>% 
  with(
    .,
    mean(
      is.na(.$yes_health)
    )
  ) #0; good, expected


    # --Q3->  which variables were included in the group's logistic model? 
    #         does the finding hold when we start with only % min and HEALTH_yes?

#convert unit of observation (row) to FRS carceral facility
per_min_and_yes_health <- swda %>% 
  group_by(
    REGISTRY_ID
  ) %>% 
  summarize(
    per_minority  = max(FAC_PERCENT_MINORITY),  #var should already be at REGISTRY_ID level, but drop the years
    #this will pick up NAs, max() applied at group level
    yes_health    = if_else(
                      condition = sum(as.numeric(HEALTH_yes)) > 0, #HEALTH_yes is 0/1 at SWDA ID level, sum to get FRS ID level
                      true      = 1,
                      false     = 0
                    )
  ) %>% 
  filter(
    !is.na(per_minority) #drop records missing per_minority
  ) %>% 
  mutate(
    per_minority  = as.numeric(per_minority), #convert from character
    yes_health    = factor(yes_health, levels = c("0","1"), labels = c("Yes","No")), #convert from character
  )

#check n and class imbalance (fairly balanced)
dim(per_min_and_yes_health) 
#[1] 197   3

per_min_and_yes_health %>% 
  group_by(
    yes_health
  ) %>% 
  tally()
#yes_health n
#   <dbl>   <int>
#   0       121
#   1       76

#visual inspection of a boxplot doesn't seem to suggest an association
ggplot(
    data = per_min_and_yes_health
  ) +
  geom_boxplot(
    mapping = aes(x = yes_health, y = per_minority)
  ) +
  ggtitle("Yes Health Violation vs % Minority", subtitle = "Carceral Facilities with SWDA Permits and % Minority Data") +
  ylab("Percent Minority") +
  xlab("Health Violation (Fiscal Year 2011-2020)")

#I'm uncomfortable suggesting this is a valid setup for inference, but let's
#   use the linear model approach to replicate original analysis and describe
#   a potential relationship

#(logistic model on a single categorical variable doesn't add useful info over means comparisons,
# but since this was initially suggested)
summary(glm(yes_health ~ per_minority, data = per_min_and_yes_health, family = "binomial"))
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.1883  -0.9853  -0.9103   1.3407   1.4935  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)   
# (Intercept)  -0.760717   0.269340  -2.824  0.00474 **
# per_minority  0.008547   0.006444   1.326  0.18471   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 262.73  on 196  degrees of freedom
# Residual deviance: 260.97  on 195  degrees of freedom
# AIC: 264.97
# 
# Number of Fisher Scoring iterations: 4


