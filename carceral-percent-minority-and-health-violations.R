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
    #         there are a few FRS IDs with > 1 SWDA ID:
swda %>% 
  group_by (
    REGISTRY_ID,
    FAC_ID
  ) %>% 
  tally() %>% #like summarize, tally() will strip a grouping level, so we're rolling up
  tally() %>% 
  arrange(
    desc(n)
  )

# # A tibble: 361 x 2
#    REGISTRY_ID      n
#    <chr>        <int>
#  1 110005364658     2
#  2 110021747822     2
#  3 110060258858     2
#  4 110068077191     2
#  5 110000527190     1
#  6 110000711285     1
#  7 110000736099     1
#  8 110000887489     1
#  9 110000943409     1
# 10 110000946148     1
# # ... with 351 more rows


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
    #         (was STATE_NUM included as numeric? if categorical, cell sizes likely very small)
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
    yes_health    = factor(yes_health, levels = c("0","1"), labels = c("No","Yes")), #convert from character
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

#logistic model on a single categorical variable
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

#doesn't look promising, 
#   I'll be asked about practical interpretation, so let's check practical significance 
#   against 80% of our data range:
b0 <- -0.760717 
b1 <-  0.008547

#calculate odds and probabilities
odds_and_prob <- function(per_minority) {
  
  odds          <- exp(b0+b1*per_minority)
  prob          <- odds/(1+odds) #equivalent for 1/1+exp(-logit)
  result        <- c(odds,prob)
  names(result) <- c("odds","probability")
  
  return(result)
  
}

per_minority  <- per_min_and_yes_health$per_minority
yes_health    <- per_min_and_yes_health$yes_health 

boxplot(per_minority)
quantile(per_minority, probs = seq(0,1,0.1))
#  0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
#  0.0  7.6 12.2 17.0 23.4 30.0 37.0 47.2 55.0 66.4 92.0 

qqnorm(per_minority, pch = 1, frame = FALSE)
qqline(per_minority, col = "steelblue", lwd = 2)

#let's check practical significance against 80% of our data range
odds_and_prob(.076)
#      odds probability 
# 0.4676349   0.3186316 
 

odds_and_prob(.664)
#      odds probability 
# 0.4699910   0.3197237 

#finding not supported, escalate to group
