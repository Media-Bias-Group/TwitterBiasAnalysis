library(merTools)
library(lme4)
library(jtools)
library(lmerTest)
library(reghelper)
library(dplyr)
library(Hmisc)
library(tidyverse)
library(corrplot)
library(texreg)
library(stargazer)


# read data
final_dataset <- read.csv(file='../Python Script/academic/data/final_dataset.csv', encoding='UTF-8', colClasses=c('id'='character', 'tweet_id'='character'))

# prepare dataset 
# remove 'neg_score_hate', 'pos_score_sentiment', neg_score_sentiment'
final_dataset <- subset(final_dataset, select=-c(neg_score_hate, pos_score_sentiment, neg_score_sentiment))


# rename 'pos_score_hate'
final_dataset <- final_dataset %>% rename(
  hate_score = pos_score_hate,
)




#---------------------#
# 1. Data Preparation #
#---------------------#

# create values on article-level (group by article_url and get mean for hate_score and polarity_strength)
data <- final_dataset %>%
  group_by(article_url, outlet) %>% 
  summarise_at(vars(hate_score, polarity_strength, bias_score, reliability_score, overall_bias, overall_reliability), mean)

# take absolute values of bias_score and overall_bias => bias ranges from 0 (unbiased) to 42 (heavily biased)
data$bias_score_abs <- abs(data$bias_score)
data$overall_bias_abs <- abs(data$overall_bias)



##################################################
# Correlation Matrix - selected attributes       #
##################################################
# aggregate at outlet-level
data_num_outlet <- data %>% 
  group_by(outlet) %>%
  summarise_at(vars(hate_score, 
                    polarity_strength, 
                    overall_reliability, 
                    overall_bias_abs, 
                    bias_score_abs, 
                    reliability_score), mean)

# remove non-numeric & bias scores (keep absolute bias scores)
data_num_outlet <- subset(data_num_outlet, select=-c(outlet)) # remove 'outlet'
data_num <- subset(data, select=-c(outlet, article_url, bias_score, overall_bias)) # remove 'outlet'


# calculate correlation matrix - pearson: article level
corr_data <- cor(data_num, method=c('pearson'))
round(corr_data, 4)
corrplot(corr_data, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 1.5)
# calculate correlation matrix - pearson: outlet level
corr_data <- cor(data_num_outlet, method=c('pearson'))
round(corr_data, 4)
corrplot(corr_data, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 1.5)




##################################################
# Multilevel Model 1 - selected attributes       #
# bias_score as dependent variable               #
# bias_score and overall_bias to absolute values #
##################################################

# variables:
# bias_score_abs: Y (dependent variable [0, 42])
# hate_score: X_1 (explanatory level-1 variable) => group-mean adjusted (hate_cwc) or grand-mean adjusted (hate_cgm)
# polarity_strength: X_2 (explanatory level-1 variable) group-mean adjusted (polarity_cwc) or grand-mean adjusted (polarity_cgm)
# overall_bias_abs: Z_1 (explanatory level-2 variable [0, 42]) => grand-mean adjusted (overall_bias_cgm)
# overall_reliability: Z_2 (explanatory level-2 variable) => grand-mean adjusted (overall_reliability_cgm)
# hate_score_CGM: Z_3 => grand-mean centered group  means of hate_score
# polarity_strength_CGM: Z_4 => grand-mean centered group means of polarity_strength
# outlet: level-2 identifier

#-----------------------------------#
# MLM 1: Pure effects on bias_score #
#-----------------------------------#
# requires CWC for level-1 predictors => subtract from each x value its group mean 
# take CGM for level-2 predictors to keep intercept within interpretable range
# add grand mean centered group means of level-1 predictors => group mean - grand mean

# get group means for the two X variables
group_means <- data %>%
  group_by(outlet) %>%
  summarise_at(vars(hate_score, polarity_strength), mean)

# rename 'hate_score' and 'polarity_strength
group_means <- group_means %>% rename(
  gmean_hate = hate_score,
  gmean_polarity = polarity_strength
)

# merge data with group_means
data_1 <- merge(data, group_means)

# hate_score - gmean_hate and polarity_strength - gmean_polarity
data_1$hate_cwc <- (data_1$hate_score - data_1$gmean_hate)#
data_1$polarity_cwc <- (data_1$polarity_strength - data_1$gmean_polarity)


# get grand means for the two Y variables
grand_means_lv2 <- data %>%
  group_by() %>%
  summarise_at(vars(overall_bias_abs, overall_reliability), mean)

# rename 'hate_score' and 'polarity_strength
grand_means_lv2 <- grand_means_lv2 %>% rename(
  cmean_overall_bias = overall_bias_abs ,
  cmean_overall_reliability = overall_reliability
)

# merge data_1 with grand_means_lv2
data_1 <- merge(data_1, grand_means_lv2)

# hate_score - cmean_hate and polarity_strength - cmean_polarity
data_1$overall_bias_cgm <- (data_1$overall_bias_abs - data_1$cmean_overall_bias)#
data_1$overall_reliability_cgm <- (data_1$overall_reliability - data_1$cmean_overall_reliability)


# get grand means for the two X variables
grand_means <- data %>%
  group_by() %>%
  summarise_at(vars(hate_score, polarity_strength), mean)

# rename 'hate_score' and 'polarity_strength
grand_means <- grand_means %>% rename(
  cmean_hate = hate_score ,
  cmean_polarity = polarity_strength
)

# merge data_1 with grand_means
data_1 <- merge(data_1, grand_means)

# hate_score - cmean_hate and polarity_strength - cmean_polarity
data_1$gmean_hate_cgm <- (data_1$gmean_hate - data_1$cmean_hate)#
data_1$gmean_polarity_cgm <- (data_1$gmean_polarity - data_1$cmean_polarity)



model_1 <- lmer(bias_score_abs ~ 1+hate_cwc+polarity_cwc+overall_bias_cgm+overall_reliability_cgm
                +gmean_hate_cgm+gmean_polarity_cgm+(1+hate_cwc+polarity_cwc|outlet), data=data_1, REML=FALSE) 
summary(model_1) 
summ(model_1) # ICC 0.07
ranova(model_1)

# significant relationship between hate_score and bias_score => hence observe interaction effects
# is variance of X1 predictors significant? is yes, keep slopes varying; if no, make slopes fixed (aus Klammer herausnehmen)



#----------------------------#
# MLM 2: Interaction Effects #
#----------------------------#
# observe interaction effects of relationship between hate_score and article bias
model_2 <- lmer(bias_score_abs ~ 1+hate_cwc+polarity_cwc+overall_bias_cgm+overall_reliability_cgm
                +hate_cwc:overall_bias_cgm+hate_cwc:overall_reliability_cgm
                +(1+hate_cwc+polarity_cwc|outlet), data=data_1, REML=FALSE) 
summary(model_2) 
# significant interaction effects




##################################################
# Multilevel Model 2 - selected attributes       #
# reliability_score as dependent variable        #
# bias_score and overall_bias to absolute values #
##################################################

#-----------------------------------------#
# MLM 1: Pure effect on reliability score #
#-----------------------------------------#

model_3 <- lmer(reliability_score ~ 1+hate_cwc+polarity_cwc+overall_bias_cgm+overall_reliability_cgm
                +gmean_hate_cgm+gmean_polarity_cgm+(1+hate_cwc+polarity_cwc|outlet), data=data_1, REML=FALSE) 
summary(model_3) 
summ(model_3) # ICC 0.07
ranova(model_3)
# is variance of X1 predictors significant? is yes, keep slopes varying; if no, make slopes fixed
# shows that hate and polarity have both no significant variance, hence they have no varying slopes

model_3A <- lmer(reliability_score ~ 1+hate_cwc+polarity_cwc+overall_bias_cgm+overall_reliability_cgm
                +gmean_hate_cgm+gmean_polarity_cgm+(1|outlet), data=data_1, REML=FALSE) 
summary(model_3A) 
summ(model_3A) # ICC 0.07
ranova(model_3A)
# significant relationship between hate_score and bias_score => hence observe interaction effects

#----------------------------#
# MLM 2: Interaction Effects #
#----------------------------#
# observe interaction effects of relationship between hate_score and article bias
model_4 <- lmer(reliability_score ~ 1+hate_cwc+polarity_cwc+overall_bias_cgm+overall_reliability_cgm
                +hate_cwc:overall_bias_cgm+hate_cwc:overall_reliability_cgm
                +(1|outlet), data=data_1, REML=FALSE) 
summary(model_4) 
# no significant interaction effects





##################
# Export Results #
##################
class(model_1) <- "lmerMod"
class(model_3A) <- "lmerMod"
stargazer(model_1, model_3A, title="Results for Level-1 and Level-2 Effects", align=TRUE, single.row=TRUE)

class(model_2) <- "lmerMod"
class(model_4) <- "lmerMod"
stargazer(model_2, model_4, title="Results for Interaction Effects", align=TRUE, single.row=TRUE)



stargazer(model_1, model_2, model_3A, model_4, title="Regression Results", align=TRUE)
