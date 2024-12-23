### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Author: Augusto Ferraris, MD - UBA / MPH(C) - University of Washington, Seattle
# Contact information: aferra@uw.edu
#
# Date: Dec 2024
# R version 4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"
#
# Background: Mantel Haenszel stratified analysis is a classic method to adjust 
# for confounding in epidemiology. 
# While its use has decreased in the last decades, the results yielded by this 
# method may be very similar to those obtained with regression models. Furthermore, 
# it represents a landmark procedure to be conducted by epidemiologist in-training 
# as a didactic exercisse. 
# Purpose: to provide a tool to perform Mantel Haenszel stratified 
# analysis with >=1 adjusting variable, with an automatized approach.  
# Program
# 1. Loading libraries
# 2. Rationale
# 3. Example

# loading libraries
library(epiR)

# We will use the epi.2by2(), which uses 3 dimensions to estimate OR trough stratification
# exposure and outcomes MUST be coded 1=yes, 2=no. just binaries

# stratifying var 1==1, stratifying var 2==0
#                   Outcome yes (1) | outcome no (2)
# Exposure yes(1)|     XX                XX
# Exposure No (2)|     XX                XX

# The dimensions are 2 by 2 tables with K number of tables, 
# where K is the number of strata defined by the different combinations 
# of confounding variables. 
# code starts ----

data_mh <- your_dataframe
outcome_c <- as.character(your_outcome_name)
exposure_c <- as.character(your_exposure_name)
 
# enter the names of the adjustment variables as they appear in the dataset. 
adjustment_c <- as.character("adj1", "adj2", "adj3", "adjk")
# now, getting array of tables with function xtabs
mh_array <- data_mh[, c(outcome_c, exposure_c, adjustment_c)]

adjustment_formula <- paste0("~ ", outcome_c, " + ", 
                             exposure_c, " + ")

for(i in adjustment_c){
  adjustment_formula <- paste0(adjustment_formula, i, " + ")
}

# we expect that the number of dimensions would be 2 (carb) and 3 (gears) = 6. 
n_tables <- 1 # basic starting point 

for(i in adjustment_c){
  dimensions <- as.numeric(
    length(
      levels(as.factor(data_mh[[i]])
      )
    )
  )
  
  n_tables <- n_tables*dimensions
  
  rm(dimensions)
}

n_tables

# preparing data for analysis----
adjustment_formula <- substr(adjustment_formula, start = 1, 
                             stop = nchar(adjustment_formula) - 3)
adjustment_formula # check that everything looks ok. 

mh_array <- xtabs(as.formula(adjustment_formula), 
                  data = mh_array) # stratifying 
mh_array # visualize tables for errors. Outcome should be on the column side, 
# exposure on the rows side. 

mh_array <- array(mh_array, 
                  dim = c(2, 2, n_tables), # this creates a k(n_tables) 2by2 tables. 
                  list(exposure = c("exposure_yes", "exposure_no"), 
                       outcomes = c("outcome_yes", "outcome_no"), 
                       confounders = 1:n_tables))

(epi.2by2(mh_array, method = 'cohort.count'))
# compare with  regression model
# re-shape exposure and outcome to 0/1 format

data_mh$your_outcome[data_mh$your_outcome == 2] <- 0
data_mh$your_exposure[data_mh$your_exposure == 2] <- 0

adjustment_formula <- paste0(outcome_c, " ~ ", 
                             exposure_c, " + ")

for(i in adjustment_c){
  adjustment_formula <- paste0(adjustment_formula, i, " + ")
}

adjustment_formula <- substr(adjustment_formula, start = 1, 
                             stop = nchar(adjustment_formula) - 2)

regression <- glm(as.formula(adjustment_formula), 
                  data = data_mh, 
                  family = poisson)

exp(regression$coefficients[[2]])

## Example using the mtcars dataset. ----
# crash = 1/2 variable created for this example. 
# am = automatic or manual, 0 and 1 respectively.
# stratifying variables: carb (number of carburetors, we will dicotomize in >3 or <=3)
# gears = number of forward gears in the transmision. 3, 4 or 5. 

# first, we transform exposure and outcome vars
set.seed(788622) # see world cup champion for these years. '78, '86, '22

data_mh <- rbind(mtcars, mtcars, mtcars, mtcars)
data_mh <- rbind(data_mh, data_mh)
rownames(data_mh) <- NULL

data_mh$crash <- sample(1:2, nrow(data_mh), replace = TRUE, 
                        prob = c(0.25, 0.75)) # outcome example

data_mh$vs[data_mh$vs == 0] <- 2 # exposure example

data_mh$carb[data_mh$carb <= 3] <- 1 # categorizing 
data_mh$carb[data_mh$carb > 3] <- 2 # categorizing

outcome_c <- as.character("crash")
exposure_c <- as.character("vs")
adjustment_c <- c("carb", "gear") 

# now, getting array of tables with function xtabs
mh_array <- data_mh[, c(outcome_c, exposure_c, adjustment_c)]

adjustment_formula <- paste0("~ ", outcome_c, " + ", 
                             exposure_c, " + ")

for(i in adjustment_c){
  adjustment_formula <- paste0(adjustment_formula, i, " + ")
}

# we expect that the number of dimensions would be 2 (carb) and 3 (gears) = 6. 
n_tables <- 1 # basic starting point 

for(i in adjustment_c){
  dimensions <- as.numeric(
    length(
      levels(as.factor(data_mh[[i]])
      )
    )
  )
  
  n_tables <- n_tables*dimensions
  
  rm(dimensions)
}

n_tables

# preparing data for analysis----
adjustment_formula <- substr(adjustment_formula, start = 1, 
                             stop = nchar(adjustment_formula) - 3)
adjustment_formula # check that everything looks ok. 

mh_array <- xtabs(as.formula(adjustment_formula), 
                  data = mh_array) # stratifying 
mh_array # visualize tables for errors. Outcome should be on the column side, 
         # exposure on the rows side. 

mh_array <- array(mh_array, 
                  dim = c(2, 2, n_tables), # this creates a k(n_tables) 2by2 tables. 
                  list(exposure = c("exposure_yes", "exposure_no"), 
                       outcomes = c("outcome_yes", "outcome_no"), 
                       confounders = 1:n_tables))

(epi.2by2(mh_array, method = 'cohort.count'))

# compare with  regression model

data_mh$crash[data_mh$crash == 2] <- 0
data_mh$vs[data_mh$vs == 2] <- 0

adjustment_formula <- paste0(outcome_c, 
                             " ~ ", 
                             exposure_c, " + ")

for(i in adjustment_c){
  adjustment_formula <- paste0(adjustment_formula, i, " + ")
}

adjustment_formula <- substr(adjustment_formula, start = 1, 
                             stop = nchar(adjustment_formula) - 2)

regression <- glm(as.formula(adjustment_formula), 
                  data = data_mh, 
                  family = poisson)

exp(regression$coefficients[[2]])

# take into account that if you have tables without observations, MH wont run or may be inaccurrate. 
