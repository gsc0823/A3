#===================================
# Problem Set A2 - Gregory Campbell
#===================================

#==========================================
# Exercise 1: Links to Datasets
#==========================================

# Preliminaries

install.packages("plm") # using plm to check work
install.packages("moments")

library(moments)
library(plm)
library(readr)
library(tidyverse)

crime_long = read.csv("~/Stata/crime_long.csv")
officers = read_csv("Stata/officers (1).csv")
population = read.csv("~/Stata/population.csv")

#==========================================
# Exercise 2: Data Manipulation
#==========================================

# Beginning by converting dates in the crimes, population, and officer datasets to a format R understands

crime_long$crime_month = as.Date(crime_long$crime_month, "%m/%d/%Y")

population$month = as.Date(population$month, "%m/%d/%Y")

officers$month = as.Date(officers$month, "%m/%d/%Y")

# Calculating Total crimes per month

Total_crime_month = crime_long %>% group_by(crime_month) %>% summarise(Total_crime = sum(crimes))

Total_crime_month = Total_crime_month %>% arrange(crime_month)

head(Total_crime_month,20)

# Plotting crime time series

plot(Total_crime_month$Total_crime~Total_crime_month$crime_month, ylab = "Total Crimes", xlab = "Time") 

# Merging population and crime datasets

# Computing total crime per district per month

crime_district = crime_long %>% group_by(crime_month,district) %>% summarise(Total_Crime_Dist = sum(crimes))

# Merging crime per district with original crime dataset

crime_new = left_join(crime_long,crime_district)

# Renaming column crime_month in crime dataset to month so that it matches column name for population dataset

crime_new = crime_new %>% rename(month = crime_month)

# Putting crime dataset into wide format

crime_wide = crime_new %>% pivot_wider(names_from = crime_type,values_from = crimes,values_fn = sum)

# Merging my crime dataset with the population dataset

pop_crime_merg = left_join(population,crime_wide)

# Constructing panel dataset

# Making variables for panel dataset:

per_resident = pop_crime_merg %>% group_by(month,district) %>% summarise(total_crime_res = Total_Crime_Dist/tot_pop,
                                                                         violent_crime_res = violent/tot_pop,
                                                                         prop_crime_res = property/tot_pop,
                                                                         share_black = tot_black/tot_pop,
                                                                         share_hisp = tot_hisp/tot_pop,
                                                                         share_white = tot_white/tot_pop)

# Adding these variables to my dataset and selecting variables I need

panel_data = left_join(pop_crime_merg,per_resident)

panel_data = panel_data %>% select(1,2:3,8:9,14:19)

# Panel dataset

panel_data = unique(panel_data)

head(panel_data,10)

#==========================================
# Exercise 3: Panel Data: Introduction
#==========================================

# Renaming unit column in officers dataset to match what I have in my panel dataset

new_officers = officers %>% rename(district = unit)

new_officers = new_officers %>% arrange(month,district,NUID)

# Merging the two datasets

panel_data_2 = left_join(new_officers,panel_data)

# Normal OLS regression. Also, note that by total crimes, I assume that the professor wants us to use the 
# total crimes per resident variable that we constructed in the previous exercise. 

first_reg = lm(arrest ~ tenure + total_crime_res + p50_inc + share_black + share_hisp + share_white, data = panel_data_2)

# Estimated coefficients

summary(first_reg)


#==========================================
# Exercise 4: Panel Data: More Controls
#==========================================

# Dummay variable approach

second_reg = lm(arrest ~ tenure + total_crime_res + p50_inc + share_black + share_hisp + share_white
                 + factor(month) + factor(district), data = panel_data_2)

summary(second_reg)

#===================================================
# Exercise 5: Panel Data: Individual fixed effects
#===================================================

# Within Estimator

# Finding mean of variables of interest 

officer_means = panel_data_2 %>% group_by(NUID) %>% summarise(tenure_mean = mean(tenure), 
                                                                   crime_res_mean = mean(total_crime_res),
                                                                   p50_inc_mean = mean(p50_inc),
                                                                   share_black_mean = mean(share_black),
                                                                   share_hisp_mean = mean(share_hisp),
                                                                   share_white_mean = mean(share_white),
                                                                   arrest_mean = mean(arrest))

within_data = left_join(panel_data_2, officer_means)

# Creating demeaned variables

within_data = within_data %>% mutate(within_tenure = tenure - tenure_mean, within_crime_res = total_crime_res - crime_res_mean,
                                     within_p50 = p50_inc - p50_inc_mean, within_black = share_black - share_black_mean,
                                     within_hisp = share_hisp - share_hisp_mean, within_white = share_white - share_white_mean,
                                     within_arrest = arrest - arrest_mean)

within_reg = lm(data = within_data, within_arrest ~ within_tenure + within_crime_res + within_p50 + within_black + within_hisp + within_white)

summary(within_reg)

# Between Estimator 

between_reg = lm(data = within_data, arrest_mean ~ tenure_mean + crime_res_mean + p50_inc_mean + share_black_mean + share_hisp_mean + share_white_mean)

summary(between_reg)

# First difference 

# Making dataset for first difference

first_diff_data = panel_data_2 %>% select(1:2,4:5,7,9,12:14)

first_diff_data = first_diff_data %>% arrange(NUID,month)

# Finding first month that each officer began service

month_min = first_diff_data %>% group_by(NUID) %>% summarise(month=min(month))

month_min = month_min %>% mutate(drop=1) # Adding column of indicators

# Creating dataset of lagged variables

first_diff = first_diff_data[,3:9]-lag(first_diff_data[,3:9])

first_diff = cbind(first_diff_data[,1:2],first_diff)

first_diff = left_join(first_diff,month_min)

# Eliminating rows where NUID changes

first_diff = first_diff %>% filter(is.na(drop))

first_diff = first_diff %>% select(-drop)

# First Difference regression

first_diff_reg = lm(arrest ~ tenure + total_crime_res + p50_inc + share_black + share_hisp + share_white, data = first_diff)

summary(first_diff_reg)

# Looking at the outputs from my regressions, we see that the coefficient estimates are fairly different
# for each variable across all models. However, what does seem to be consistent is the sign of these coefficients. For all models,
# an increase in the share of black, white, or Hispanic residents decreases the number of arrests. Also, an increase in the median
# income seems to result in less arrest as well for the Within and First Difference models.Another thing that sticks out 
# is the significance of the coefficient estimates. For the Within and First Difference models, none of the coefficient estimates 
# for our independent variables are statistically significant. However, if we look at the output for out Between
# estimator regression, we see that the coefficients in front of the independent variables for the means of tenure, total crime
# per resident, median income, the share of black residents, and the share of whites are all statistically significant at the
# 5% level. This might be an indicator that individual affects and characteristics might be causing the change in arrest
# rates within districts. 

# Individual variation instead of time variation! 

# Rescaling NUID and months

nuid_rescale = panel_data_2 %>% select(NUID) %>% unique() %>% arrange(NUID)

nuid_rescale = nuid_rescale %>% mutate(id_officer = 1:nrow(nuid_rescale))

month_rescale = panel_data_2 %>% select(month) %>% unique() %>% arrange(month)

month_rescale = month_rescale %>% mutate(id_month = 1:nrow(month_rescale))

# Making my gmm dataset

gmm_data = left_join(panel_data_2,nuid_rescale)

gmm_data = left_join(gmm_data,month_rescale)

gmm_data = gmm_data %>% drop_na()


# Running GMM model. However, I will note that running this code takes forever on my computer, so the below
# code is what I would do for this problem. 

GMM_model = function(param,gmm_data){
  
  # Coefficients
  
  ni = max(gmm_data$id_officer)
  alpha = param[1:ni]
  beta = param[ni+1]
  gamma = param[(ni+2):(ni+6)]
  phi = param[(ni+7):(ni+31)]
  kappa = param[(ni+32):(ni+31+max(gmm_data$id_month))]
  

  #Variables
  
  arrest = gmm_data$arrest
  tenure = gmm_data$tenure
  tot_crimes_res = gmm_data$total_crime_res
  median_income = gmm_data$p50_inc
  s_black= gmm_data$share_black
  s_white= gmm_data$share_white
  s_hisp= gmm_data$share_hisp
  
  # Loop to compute estimates
  
  est=mat.or.vec(nrow(gmm_data),1)
  
  for (ii in 1:nrow(gmm_data)){
    est[ii] = alpha[gmm_data$id_officer[ii]]+beta*tenure[ii]+
      gamma[1]*tot_crimes_res[ii] + gamma[2]*median_income[ii]+
      gamma[3]*s_black[ii]+gamma[4]*s_white[ii]+gamma[5]*s_hisp[ii]
     + phi[gmm_data$district[ii]]+kappa[gmm_data$id_month[ii]] 
    
    
  }
  
  moments_est=all.moments(est,order.max=2)[-1]
  moments_arrest=all.moments(arrest,order.max=2)[-1]
  
  like = sum((moments_est - moments_arrest)^2)
  return(like)
}

small_gmm_data = gmm_data[1:1000,] # making the number of individuals looked at smaller so that the code runs

param=runif(max(small_gmm_data$id_officer)+6+max(small_gmm_data$id_month))/10000

GMM_model(param,small_gmm_data)

set.seed(123)

res  = optim(param,fn=GMM_model,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),gmm_data=small_gmm_data)

results = res$par

head(results,20)
