# Activity: STAT 6021 Project 1 - Diamonds
# Team Members: Brigitte Hogan (bwh5v), Sherry Kausch, Melissa Phillips, Jason Tiezzi
# Source of Data: BlueNile.com
# Deadline: Friday, July 19th @0900


# Each project should feature:
# - Clear central analytic goals and/or questions to answer 
#   (the more interesting/ challenging/ practical, the better)
# - a "substantial" computational component for the analysis
# - analytic methods developed in this course

# The deliverables are
# 1. Report (.doc, .docx, or .pdf)
#    - less than 10 pages, and include:
#    - an execsum describing the high-level goals/questions of the project
#    - the nature/characteristics of the data used in the analysis
#    - the results of the analysis, including any recommendations
#    - make use of graphics wherever appropriate.
#    - use correct grammar, clear explanations, and professional presentation
#    - audience: both clients (no experience analyzing data) & data scientists/
#      statisticians (given the report for a second opinion)
#
# 2. Reproducible R-code (.r file)
#    - commented/annotated R code
#
# 3. Presentation document (.pdf, .ppt, etc.)
#    - each team will give an 8-minute presentation
#    - not everyone needs to speak, but all team members should make equal
#      contributions to the project as a whole.
#    - audience: understandable by anyone familiar with the course material, but
#      who has not read your project report yet


#__________________###########################################################
# IMPORT LIBRARIES ####
library(here) # for setting working directory
library(tidyverse) # for ggplot2 and piping
library(GGally) # for interaction plots
library(treemap) # for plotting
library(MASS)
library(car)

# to get p-value of model without "peeking" at t-tests
getPmodel <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

#__________________############################################################
# READ IN DATA ####
diam <- read.csv("clean_diamond_data.csv")
head(diam)

#__________________#############################################################
# CLEAN DATA ####
str(diam)

# Re-leveling clarity
# actual order: (worst) SI2 < SI1 < VS2 < VS1 < VVS2 < VVS1 < IF < FL (best)
diam$clarity <- factor(diam$clarity,
                       levels = c("SI2","SI1","VS2","VS1",
                                  "VVS2","VVS1","IF","FL"))
contrasts(diam$clarity) # how clarity will be coded in lm

# Re-leveling color
# actual order: J (worst) to D (best)
diam$color <- factor(diam$color, levels=c("J","I","H","G","F","E","D"))
contrasts(diam$color) # how color will be coded in lm

# re-leveling cut
# actual order: (worst) Good < Very Good < Ideal < Astor Ideal (good)
# our focus is on "Astor Ideal vs "Ideal" so we move "Ideal" into 1st position to use as a reference point for comparison
diam$cut <- factor(diam$cut, 
                   levels = c("Ideal", "Good", "Very Good", "Astor Ideal"))
contrasts(diam$cut) # how cut will be coded in lm
# save actual order for plotting
diam$cut_plotting <- factor(diam$cut, 
                            levels = c("Good", "Very Good", "Ideal", "Astor Ideal"))

#__________________#############################################################
# DESCRIPTIVE STATS ####
summary(diam)

# y (dependent)
range(diam$price) # large range from 229 to 2,000,000+
mean(diam$price)  # mean of $5540
quantile(diam$price) %>% plot()

# carat - continuous
range(diam$carat) # large range from 0.23 to 20.45
mean(diam$carat)  # 0.76
quantile((diam$carat)) %>% plot()

# clarity - discrete
table(diam$clarity) # fewer high-clarity diamonds

# cut - discrete
table(diam$cut) # most diamonds are ideal, followed by Very Good & Good

# color - discrete
table(diam$color) # few poor color diamonds, but otherwise even distribution

## Descriptive Plots ####

diam$freq <- 1 # creates count variable for plotting
diam$cut_plotting2 <- as.character(diam$cut) # another plotting option
diam$cut_plotting2 <- replace(diam$cut_plotting2, diam$cut_plotting2 == "Astor Ideal", "AI")
diam$cut_plotting2 <- factor(diam$cut_plotting2, 
                             levels = c("Good", "Very Good", "Ideal", "AI"))
# RColorBrewer::display.brewer.all() # color palettes
mapColor <- hcl.colors(8, palette="Blues")

# General map based on number of values
treemap(diam,
        index=c("cut_plotting2"),
        vSize="freq", 
        title="",
        type="value",
        # graphic options
        palette = rev(mapColor),
        fontsize.labels = c(26), bg.labels=0, 
        fontface.labels = c("bold"),
        fontcolor.labels = c("red3"),
        border.col = c("black"),
        border.lwds = c(7))

# Nested by color & clarity
treemap(diam,
        index=c("cut_plotting2","color","clarity"),
        vSize="freq", 
        title="",
        type="categorical", vColor="color",
        # graphic options
        palette = mapColor,
        fontsize.labels = c(26, 20, 8), bg.labels=0, 
        fontface.labels = c("bold", "bold", "plain"),
        fontcolor.labels = c("red3", "gray40", "gray10"),
        border.col = c("black", "black", "white"),
        border.lwds = c(7,3,1))

#__________________#############################################################
# CHECKING ASSUMPTIONS ####

# 1.  Testing for a linear relationship between price and carat ----

partial_mod1 <- lm(price ~ carat, data = diam)
summary(partial_mod1)

# Basic Scatterplot
diam %>% ggplot(aes(carat, price, alpha=0.5)) + 
  geom_point() +
  xlab("Carat") +
  ylab("BlueNile List Price in US Dollars") +
  theme(legend.position="none")
# relationship between carat & price does not look linear

# externally studentized residuals
ext_s_resids <- studres(partial_mod1)
qqnorm(ext_s_resids)
qqline(ext_s_resids)

# Fitting another linear model to the new data
# we know from descriptive stats that they both have a wide range and there is very little data in the upper quantile
# to adjust the scale, we try log-transformation of y value
diam$logPrice <- log10(diam$price) # log base 10 transform
diam %>% ggplot(aes(carat, logPrice)) + geom_point() # scatterplot
# scatterplot shows data more evenly distributed in y direction but not x
# try similar transformation for carat
diam$logCarat <- log10(diam$carat)

# fitting the new model
partial_mod2 <- lm(logCarat ~ logPrice, data = diam)
summary(partial_mod2)

#externally studentized residuals
ext_s_resids <- studres(partial_mod2)

#qq-plot
qqnorm(ext_s_resids)
qqline(ext_s_resids)

# new scatterplot
diam %>% ggplot(aes(logCarat, logPrice)) + geom_point()
# relationship now appears linear and points are more evenly spread
# checking dependent variable
hist(diam$price)
hist(diam$logPrice) 
# the log transformation has improved skew in data, but still some R skew
diam %>% ggplot(aes(price)) + 
  geom_histogram(bins = 30) +
  ylab("Number of Diamonds") +
  xlab("Price")
diam %>% ggplot(aes(logPrice)) + 
  geom_histogram(bins = 30) +
  ylab("Number of Diamonds") +
  xlab("log(Price)")

# 2. Checking Predictors ----

# A. Carat (continuous) ----
hist(diam$carat)
hist(diam$logCarat)
diam %>% ggplot(aes(carat)) + 
  geom_histogram(bins = 30) +
  ylab("Number of Diamonds") +
  xlab("Carat")

diam %>% ggplot(aes(logCarat)) + 
  geom_histogram(bins = 30) +
  ylab("Number of Diamonds") +
  xlab("log(Carat)")

# B. Clarity (discrete, 8 levels) ----
diam %>% ggplot(aes(clarity)) + geom_bar() + 
  ylab("Number of Diamonds") +
  xlab("Clarity")

# C. Color (discrete, 7 levels) ----
diam %>% ggplot(aes(color)) + geom_bar() + 
  ylab("Number of Diamonds") +
  xlab("Color")

# D. Cut (discrete, 4 levels) ----
diam %>% ggplot(aes(cut_plotting)) + geom_bar() + 
  ylab("Number of Diamonds") +
  xlab("Cut")

## Interactions
# All pair plots ----
ggpairs(diam)
diam %>% ggplot(aes(logCarat, logPrice)) + geom_point()
diam %>% ggplot(aes(clarity, logPrice)) +  geom_boxplot()
diam %>% ggplot(aes(color, logPrice)) +  geom_boxplot()
diam %>% ggplot(aes(cut, logPrice)) +  geom_boxplot()


#__________________#############################################################
# MODELING ####

# untransformed data, full model, for reference
mod0 <- lm(price ~ ., data = diam)
extern_s_resids0 <- studres(mod0)
qqnorm(extern_s_resids0)
qqline(extern_s_resids0)

# Fitting the complete linear model
# linear model 1
mod1 <- lm(logPrice ~ logCarat + clarity + color + cut, data = diam)

## RECHECKING ASSUMPTIONS ----

#find externally studentized residuals
extern_s_resids1 <- studres(mod1)

# QQ Plot ####
qqnorm(extern_s_resids1)
qqline(extern_s_resids1) # still an issue at tails, but much improved

# Normality of Residuals ####
plot(fitted.values(mod1), extern_s_resids1) # looks random
#plot looks quite linear suggesting normal distribution of residuals, though there is some indication of fat tails

# Another check for normality of residuals - Histogram ####
hist(extern_s_resids1, probability = T, ylim = c(0,.5))

# Homoskedastitcity ####
plot(fitted.values(mod1), extern_s_resids1)

## This plot shoes that the residuals are unpredictable in relation to the fitted values, so the variance is random.

# Value of Variables ####
# Added variable plots
#av_plots <- avPlots(mod1)
# commented out, takes time to run

#Hypothesis testing to test H0: B2 = B3 = B4 = B5 = 0
anova(partial_mod2, mod1, data = diam)

# Checking for Correlations #### 
vif(mod1)

# Going FWD with model 1


#__________________#############################################################
# Question: Is Astor Ideal important? ----

summary(mod1)
summary(mod1)$r.squared # 0.9813
summary(mod1)$fstatistic # 651,746 huge!
getPmodel(mod1) # pretty much 0
aov(mod1)

# Confidence Intervals ####

confint(mod1, parm = "cutAstor Ideal", level = .95)

# Predictions ####
# Setting up a data frame to store variables for predictions
# You can experiment by changing the cut from "Astor Ideal" to "Ideal" given various combinations of the other variables to see how much our estimated price would change for any given set of circumstances. In all cases, the predicted price for "Astor Ideal" is 8.57% more than the predicted price of an otherwise comparable Ideal diamond.

predict_data <- data.frame(clarity = 'SI2', color = 'J', cut = 'Astor Ideal', carat = .28 )   
predict(mod1, predict_data, interval = "confidence", level = .95)

#__________________#############################################################
# x vs y Plots ----

diam %>%
  ggplot(aes(x=logCarat, y=logPrice, alpha=rev(color), color=clarity)) +
  scale_color_brewer(type = 'div') + 
  geom_point()

diam %>%
  ggplot(aes(x=carat, y=logPrice, alpha=0.1, shape=cut, 
             color=clarity, size=color)) +
  scale_color_brewer(type = 'div') + 
  geom_point()

