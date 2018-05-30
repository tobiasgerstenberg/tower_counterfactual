# Load packages -------------------------------------------------------------------------------
library(Hmisc)
# library(MASS)
library(tidyjson)
library(tidyverse)

# Misc functions ------------------------------------------------------------------------------

#RMSE 
rmse = function(x,y){
  return(sqrt(mean((x-y)^2)))
}

#simple regression
func_regression = function(x,formula){
  return(lm(formula,data=x)$fitted.values)
}

# ~~~~~~~~~~~~~~~~~ ---------------------------------------------------------------------------


# EXP1: PREDICTION, SELECTION, RESPONSIBILITY (EXP1 IN COGSCI PAPER) --------------------------




# EXP1: Load data ------------------------------------------------------------
load("exp1_prediction.RData")
load("exp1_responsibility.RData")
load("exp1_selection.RData")
load("exp1_info.RData")


# EXP1: Model predictions  --------------------------------------------------------------------



# ~~~~~~~~~~~~~~~~~ ---------------------------------------------------------------------------

