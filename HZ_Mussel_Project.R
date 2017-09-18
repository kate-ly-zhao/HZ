##################################################################################################
# Project: Added Value of Mussels
# Group:   Building with Nature
# Name:    Kate (Lingyu) Zhao: University of Waterloo
##################################################################################################
# Regression: Mussel morphology parameters against AFDW/CI
# ANOVA: Substrate/protection against AFDW/CI


# ******************************************* Initializing packages
install.packages("xlsx")
install.packages("openxlsx")
install.packages("ggplot2")
library(xlsx)
library(openxlsx)
library(ggplot2)

# ******************************************* Setting work directory
# workDir <- "/Users/wangfangyumeng/Downloads" # Kate's MAC
workDir <- "F:/Mussel Project" # HZ Computer
setwd(workDir)

# ******************************************* Reading in data files
filelist <- list.files(path = workDir, pattern = glob2rx("Combined Final*.xlsx"))
mussel_parameter <- read.xlsx(filelist, sheet = 5, startRow = 2, rows = 2:1975)
mussel_length <- mussel_parameter$`length.(mm)`
mussel_width <- mussel_parameter$width
mussel_height <- mussel_parameter$height
substrate <- mussel_parameter$`substrate(sand,oyster,net)`
wavebreaker <- mussel_parameter$`Wavebreaker.(1/0)`
AFDW <- mussel_parameter$`AFDW.(mg)`
CI <- mussel_parameter$CI

mussel_df <- data.frame(mussel_length, mussel_width, mussel_height, substrate, wavebreaker, AFDW, CI)

# # ******************************************* Sorting data
# 
# # Sorting by substrate
# bare_pos <- grep(pattern = "bare", x = substrate)
# oyster_pos <-grep(pattern = "oyster", x = substrate) # Sort if necessary
# net_pos <- grep(pattern = "Net", x = substrate) # Sort if necessary
# 
# substrate_vec <- gsub(pattern = "bare", replacement = 1, x = substrate)
# substrate_vec <- gsub(pattern = "net", replacement = 2, x = substrate_vec)
# substrate_vec <- gsub(pattern = "oyster", replacement = 3, x = substrate_vec)
# 
# substrate_vec <- as.double(substrate_vec)
# 
# # Sorting by protection
# wavebreaker <- mussel_parameter$`Wavebreaker.(1/0)`


# ******************************************* Full model means

# # Means for AFDW
# plot(AFDW[order(substrate_vec)], pch = order(substrate_vec), main = "full model")
# for (j in 1:3) {
#   w <- which(order(substrate_vec) == j)
#   lines(c(min(w), max(w)), c(mean(AFDW[order(substrate_vec)][w]), mean(AFDW[order(substrate_vec)][w])))
#   for (i in 1:length(w)) {
#     lines(c(w[i], w[i]), c(AFDW[order(substrate_vec)][w[i]], mean(AFDW[order(substrate_vec)][w])), lty = 2)
#   }
# }

# Means for CI

# ******************************************* Plotting results
install.packages("gplots")
library(gplots)

# Boxplot
boxplot(AFDW ~ substrate, data = mussel_df,   # Substrate vs. AFDW
        xlab = "substrate", ylab = "AFDW")
boxplot(CI ~ substrate, data = mussel_df,     # Substrate vs. CI
        xlab = "substrate", ylab = "CI")
boxplot(AFDW ~ wavebreaker, data = mussel_df, # Wavebreaker vs. AFDW
        xlab = "wavebreaker", ylab = "AFDW")
boxplot(CI ~ wavebreaker, data = mussel_df,   # Wavebreaker vs. CI
        xlab = "wavebreaker", ylab = "CI")

# Using gplots
plotmeans(AFDW ~ substrate, data = mussel_df)
plotmeans(CI ~ substrate, data = mussel_df)
plotmeans(AFDW ~ wavebreaker, data = mussel_df)
plotmeans(CI ~ wavebreaker, data = mussel_df)

# ******************************************* Detecting outliers
install.packages("mvoutlier")
library(mvoutlier)
outliers <- aq.plot(mussel_df[c("mussel_length", "mussel_width", "mussel_height", "AFDW", "CI")])
outliers # Shows list of outliers

# ******************************************* Testing for univariate normality

# **Shapiro-Wilk Test
AFDW_normality <- shapiro.test(AFDW)
CI_normality <- shapiro.test(CI)

# Q-Q Plot -> Shows data is clearly not normal
attach(mussel_df)
qqnorm(CI)
qqline(CI)

qqnorm(AFDW)
qqline(AFDW)

# Additional Tests: Use nortest package

# ******************************************* Testing for multivariate normality (Ignoring this for now)
# mshapiro.test(M)
# 
# # Graphical Assessment
# mussel_matrix <- as.matrix(mussel_df) # n x p numeric matrix
# center <- colMeans(mussel_matrix) # Centroid
# n <- nrow(mussel_matrix); p <- ncol(mussel_matrix); cov <- cov(mussel_matrix);
# d <- mahalanobis(mussel_matrix, center, cov) # Distances
# qqplot(qchisq(ppoints(n), df = p), d, 
#        main = "QQ Plot Assessing Multivariate Normality", ylab = "Mahalanobis D2")
# abline(a = 0, b = 1)

# ******************************************* Testing for homogeneity of variances

# **Bartlett Test of Homogeneity of Variances
AFDW_var <- bartlett.test(AFDW ~ mussel_length, data = mussel_df)

# Figner-Killeen Test of Homogeneity of Variances
fligner.test(AFDW~mussel_length, data = mussel_df)

# Homogeneity of Variance Plot based on Brown-Forsynth
install.packages("HH")
library(HH)
hov(AFDW ~ mussel_length, data = mussel_df)
hovPlot(AFDW ~ mussel_length, data = mussel_df)

# ******************************************* One-Way ANOVA: Untransformed Data

CI_ANOVA <- aov(CI ~ factor(substrate))
summary(CI_ANOVA)

# Alternatively...
AFDW_ANOVA <- lm(AFDW ~ factor(substrate))
summary(AFDW_ANOVA)
lm(formula = AFDW ~ factor(substrate))
anova(AFDW_ANOVA) # Performing F-test

# ******************************************* One-Way ANOVA: Tukey-Transformed Data
# Note: anova - Type I tests (variables added in sequential order)
#       Anova - Type II/III tests (Type II tests each variable after all the others)
install.packages("rcompanion", "DescTools", "car")
library(rcompanion) 
library(DescTools)
library(car)

# ***************** AFDW
mussel_df$AFDW_tuk <- transformTukey(mussel_df$AFDW, plotit = FALSE)

# Boxplots of AFDW based on substrate & wavebreaker
plot.new()
boxplot(AFDW_tuk ~ substrate, data = mussel_df, 
        ylab = "Tukey-Transformed AFDW", xlab = "Substrate Type")
boxplot(AFDW_tuk ~ wavebreaker, data = mussel_df,
        ylab = "Tukey-Transformed AFDW", xlab = "Wavebreaker")


AFDWtuk_model <- lm(AFDW_tuk ~ substrate, data = mussel_df)
Anova(AFDWtuk_model, type = "II")

summary(aov(mussel_df$AFDW_tuk ~ factor(substrate))) # ANOVA

AFDWtuk_residuals = residuals(AFDWtuk_model) # Residuals
plotNormalHistogram(AFDWtuk_residuals)

qqnorm(residuals(AFDWtuk_model), ylab = "Sample Quantiles for Residuals")
qqline(residuals(AFDWtuk_model), col = "red")
plot(fitted(AFDWtuk_model), residuals(AFDWtuk_model))

# ***************** CI
mussel_df$CI_tuk <- transformTukey(mussel_df$CI, plotit = FALSE)

# Boxplots of AFDW based on substrate & wavebreaker
boxplot(CI_tuk ~ substrate, data = mussel_df, 
        ylab = "Tukey-Transformed CI", xlab = "Substrate Type")
boxplot(CI_tuk ~ wavebreaker, data = mussel_df,
        ylab = "Tukey-Transformed CI", xlab = "Wavebreaker")


CItuk_model <- lm(CI_tuk ~ substrate, data = mussel_df)
Anova(CItuk_model, type = "II")

summary(aov(mussel_df$CI_tuk ~ factor(substrate))) # ANOVA

CItuk_residuals = residuals(CItuk_model) # Residuals
plotNormalHistogram(CItuk_residuals)

qqnorm(residuals(CItuk_model), ylab = "Sample Quantiles for Residuals")
qqline(residuals(CItuk_model), col = "red")
plot(fitted(CItuk_model), residuals(CItuk_model))

# # ******************************************* One-Way ANOVA: Box-Cox Transformed Data
# install.packages("MASS")
# library(MASS)
# 
# AFDW_box <- boxcox(AFDW ~ substrate, data = mussel_df,
#                    lambda = seq(-6, 6, 0.1))
# AFDW_cox <- data.frame(AFDW_box$x, AFDW_box$y)
# AFDW_cox2 <- AFDW_cox[with(AFDW_cox, order(-AFDW_cox$AFDW_box.y)),]
# AFDW_cox2[1,]
# 
# AFDW_lambda = AFDW_cox2[1, "AFDW_box.x"]
# mussel_df$AFDW_box <- (mussel_df$AFDW ^ AFDW_lambda - 1)/AFDW_lambda
# 
# boxplot(AFDW_box ~ substrate, data = mussel_df,
#         ylab = "Box-Cox Transformed AFDW", 
#         xlab = "Mussel Length")
# 
# AFDWbox_model <- lm(AFDW_box ~ substrate, data = mussel_df)
# 
# Anova(AFDWbox_model, type = "II")
# 
# AFDWbox_residuals <- residuals(AFDWbox_model)
# plotNormalHistogram(AFDWbox_residuals)
# 
# qqnorm(residuals(AFDWbox_model), ylab = "Sample Quantities for Residuals")
# qqline(residuals(AFDWbox_model), col = "red")
# 
# plot(fitted(AFDWbox_model), residuals(AFDWbox_model))


# **************************************************************************************
# **************************************************************************************


# ******************************************* Multiple (Linear) Regression
# http://tutorials.iq.harvard.edu/R/Rstatistics/Rstatistics.html 
# https://www.analyticsvidhya.com/blog/2015/08/comprehensive-guide-regression/ 

# ******************************************* Linear Regression - AFDW
# Examining the data
sts.m.afdw <- subset(mussel_df, select = c("mussel_length", "mussel_width", "mussel_height", "AFDW"))
summary(sts.m.afdw) # Summary of results
cor(sts.m.afdw) # Correlation btwn mussel length & AFDW

# Plotting the data
plot(sts.m.afdw)

# Fitting linear model
m.afdw.mod <- lm(AFDW ~ mussel_length + mussel_width + mussel_height, data = mussel_df) #Regression formula
summary(m.afdw.mod) # Regression coefficients table
coef(summary(m.afdw.mod)) # Model Coefficients
confint(m.afdw.mod, level = 0.95) # CIs for model parameters
# fitted(m.afdw.mod) # Predicted values
# residuals(m.afdw.mod) # Residuals
anova(m.afdw.mod) # Anova table
vcov(m.afdw.mod) # Covariance matrix for model parameters
# influence(m.afdw.mod) # Regression diagnostics
# class(m.afdw.mod)
# names(m.afdw.mod)
# methods(class = class(m.afdw.mod))[1:9]

# Check assumptions through plotting (normal distribution, homoscedastic)
plot(m.afdw.mod)

# ******************************************* Linear Regression - ML/AFDW
# Examining the data
sts.ml.afdw <- subset(mussel_df, select = c("mussel_length", "AFDW"))
summary(sts.ml.afdw) # Summary of results
cor(sts.ml.afdw) # Correlation btwn mussel length & AFDW

# Plotting the data
plot(sts.ml.afdw)

# Fitting linear model
ml.afdw.mod <- lm(AFDW ~ mussel_length, data = mussel_df) #Regression formula
summary(ml.afdw.mod) # Regression coefficients table
coef(summary(ml.afdw.mod)) # Model Coefficients
confint(ml.afdw.mod, level = 0.95) # CIs for model parameters
# fitted(ml.afdw.mod) # Predicted values
# residuals(ml.afdw.mod) # Residuals
anova(ml.afdw.mod) # Anova table
vcov(ml.afdw.mod) # Covariance matrix for model parameters
# influence(ml.afdw.mod) # Regression diagnostics
# class(ml.afdw.mod)
# names(ml.afdw.mod)
# methods(class = class(ml.afdw.mod))[1:9]

# Check assumptions through plotting (normal distribution, homoscedastic)
plot(ml.afdw.mod)

# ******************************************* Linear Regression - MW/AFDW
# Examining the data
sts.mw.afdw <- subset(mussel_df, select = c("mussel_width", "AFDW"))
summary(sts.mw.afdw) # Summary of results
cor(sts.mw.afdw) # Correlation btwn mussel width & AFDW

# Plotting the data
plot(sts.mw.afdw)

# Fitting linear model
mw.afdw.mod <- lm(AFDW ~ mussel_width, data = mussel_df) #Regression formula
summary(mw.afdw.mod) # Regression coefficients table
coef(summary(mw.afdw.mod)) # Model Coefficients
confint(mw.afdw.mod, level = 0.95) # CIs for model parameters
# fitted(mw.afdw.mod) # Predicted values
# residuals(mw.afdw.mod) # Residuals
anova(mw.afdw.mod) # Anova table
vcov(mw.afdw.mod) # Covariance matrix for model parameters
# influence(mw.afdw.mod) # Regression diagnostics
# class(mw.afdw.mod)
# names(mw.afdw.mod)
# methods(class = class(mw.afdw.mod))[1:9]

# Check assumptions through plotting (normal distribution, homoscedastic)
plot(mw.afdw.mod)

# ******************************************* Linear Regression - MH/AFDW
# Examining the data
sts.mh.afdw <- subset(mussel_df, select = c("mussel_height", "AFDW"))
summary(sts.mh.afdw) # Summary of results
cor(sts.mh.afdw) # Correlation btwn mussel height & AFDW

# Plotting the data
plot(sts.mh.afdw)

# Fitting linear model
mh.afdw.mod <- lm(AFDW ~ mussel_height, data = mussel_df) #Regression formula
summary(mh.afdw.mod) # Regression coefficients table
coef(summary(mh.afdw.mod)) # Model Coefficients
confint(mh.afdw.mod, level = 0.95) # CIs for model parameters
# fitted(mh.afdw.mod) # Predicted values
# residuals(mh.afdw.mod) # Residuals
anova(mh.afdw.mod) # Anova table
vcov(mh.afdw.mod) # Covariance matrix for model parameters
# influence(mh.afdw.mod) # Regression diagnostics
# class(mh.afdw.mod)
# names(mh.afdw.mod)
# methods(class = class(mh.afdw.mod))[1:9]

# Check assumptions through plotting (normal distribution, homoscedastic)
plot(mh.afdw.mod)

# ******************************************* Linear Regression - CI (separate with each physical parameter)
# Examining the data
sts.m.ci <- subset(mussel_df, select = c("mussel_length", "mussel_width", "mussel_height", "CI"))
summary(sts.m.ci) # Summary of results
cor(sts.m.ci) # Correlation btwn mussel length & CI

# Plotting the data
plot(sts.m.ci)

# Fitting linear model
m.ci.mod <- lm(CI ~ mussel_length + mussel_width + mussel_height, data = mussel_df) #Regression formula
summary(m.ci.mod) # Regression coefficients table
coef(summary(m.ci.mod)) # Model Coefficients
confint(m.ci.mod, level = 0.95) # CIs for model parameters
# fitted(m.ci.mod) # Predicted values
# residuals(m.ci.mod) # Residuals
anova(m.ci.mod) # Anova table
vcov(m.ci.mod) # Covariance matrix for model parameters
# influence(m.ci.mod) # Regression diagnostics
# class(m.ci.mod)
# names(m.ci.mod)
# methods(class = class(m.ci.mod))[1:9]

# Check assumptions through plotting (normal distribution, homoscedastic)
plot(m.ci.mod)
