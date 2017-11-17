##################################################################################################
# Project: Added Value of Mussels
# Group:   Building with Nature
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
workDir <- "F:/Mussel Project"
setwd(workDir)

# ******************************************* Reading in data files
filelist <- list.files(path = workDir, pattern = glob2rx("Combined Final*.xlsx"))
mussel_seed <- read.xlsx(filelist, sheet = 3)
mussel_survival <- read.xlsx(filelist, sheet = 4)
crab_data <- read.xlsx(filelist, sheet = 6)
mussel_parameter <- read.xlsx(filelist, sheet = 5, startRow = 2, rows = 2:2572)

# Outlier removal condition 1 -> CI < 6
outrem1 <- mussel_parameter$CI < 6
mussel_parameter <- mussel_parameter[outrem1,]

# Outlier removal condition 2 -> range for Length/AFDW
plot(x = mussel_parameter$`AFDW.(mg)`, y = mussel_parameter$`length.(mm)`, log = "xy")
outlier1 <- mussel_parameter$`AFDW.(mg)` < 5
mussel_parameter <- mussel_parameter[!outlier1,]
outlier2 <-  mussel_parameter$`AFDW.(mg)` < 20 &
  mussel_parameter$`length.(mm)` > 30 
mussel_parameter <- mussel_parameter[!outlier2,]
outlier3 <-  mussel_parameter$`AFDW.(mg)` > 300 & mussel_parameter$`length.(mm)` < 35
mussel_parameter <- mussel_parameter[!outlier3,]
outlier4 <- mussel_parameter$`AFDW.(mg)` < 100 &
  mussel_parameter$`AFDW.(mg)`> 80 & 
  mussel_parameter$`length.(mm)` < 21 &
  mussel_parameter$`length.(mm)` > 20
mussel_parameter <- mussel_parameter[!outlier4,]


mussel_length <- mussel_parameter$`length.(mm)`
mussel_width <- mussel_parameter$width
mussel_height <- mussel_parameter$height
substrate <- mussel_parameter$`substrate(sand,oyster,net)`
wavebreaker <- mussel_parameter$`Wavebreaker.(1/0)`
AFDW <- mussel_parameter$`AFDW.(mg)`
CI <- mussel_parameter$CI
plotnr <- mussel_parameter$plotnr
squarecode <- mussel_parameter$squarecode

mussel_df <- data.frame(mussel_length, mussel_width, mussel_height, 
                        substrate, wavebreaker, AFDW, CI, plotnr, squarecode)

subtidal_mussels <- read.xlsx(filelist, sheet = 8, startRow = 2)
subtidal_df <- data.frame(subtidal_mussels$`length.(mm)`, subtidal_mussels$width, subtidal_mussels$height,
                          subtidal_mussels$`AFDW.(mg)`, subtidal_mussels$CI)

# ******************************************* Basic Stats
seed_length <- mussel_seed$length.mm
seed_AFDW <- mussel_seed$AFDW
seed_CI <- mussel_seed$Condition.index
seed_weight <- 0.612467 # Average weight of 1 mussel seeded
seed_amount <- 10751.6 # Average amount of mussels per m^2
  
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

# ******************************************* Basic Plot Comparison of Intertidal vs. Subtidal Mussels
plot(AFDW, type = "l", col = "red")
lines(subtidal_mussels$`AFDW.(mg)`, col = "blue")

# ******************************************* Basic Comparison of Before/After for Mussels

# Find difference in treatments in morphology - if none, compare against all new mussels
# If there is difference, compare separately
# One-way anova


# ******************************************* Plotting results 
install.packages("gplots")
library(gplots)

# Boxplot
boxplot(AFDW ~ substrate, data = mussel_df,   # Substrate vs. AFDW
        xlab = "substrate", ylab = "AFDW")
AFDW_sub_mean <- aggregate(AFDW ~ substrate, mussel_df, mean)
boxplot(CI ~ substrate, data = mussel_df,     # Substrate vs. CI
        xlab = "substrate", ylab = "CI")
CI_sub_mean <- aggregate(CI ~ substrate, mussel_df, mean)
boxplot(AFDW ~ wavebreaker, data = mussel_df, # Wavebreaker vs. AFDW
        xlab = "wavebreaker", ylab = "AFDW")
AFDW_wb_mean <- aggregate(AFDW ~ wavebreaker, mussel_df, mean)
boxplot(CI ~ wavebreaker, data = mussel_df,   # Wavebreaker vs. CI
        xlab = "wavebreaker", ylab = "CI")
CI_wb_mean <- aggregate(CI ~ wavebreaker, mussel_df, mean)

# Using gplots
plotmeans(AFDW ~ substrate, data = mussel_df)
plotmeans(CI ~ substrate, data = mussel_df)
plotmeans(AFDW ~ wavebreaker, data = mussel_df)
plotmeans(CI ~ wavebreaker, data = mussel_df, n.label = FALSE) #n.label removes # of counts

# ******************************************* Detecting outliers
install.packages("mvoutlier")
library(mvoutlier)
outliers <- aq.plot(mussel_df[c("mussel_length", "mussel_width", "mussel_height", "AFDW", "CI")])
outliers # Shows list of outliers

# ******************************************* Creating function for outlier removal
# https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset 

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < qnt[1] - H] <- NA
  y[x > qnt[2] + H] <- NA
  # y
}

# https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/ 
# outlierKD <- function(dt, var) {
#   var_name <- eval(substitute(var), eval(dt))
#   na1 <- sum(is.na(var_name))
#   m1 <- mean(var_name, na.rm = TRUE)
#   par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
#   boxplot(var_name, main = "With outliers")
#   hist(var_name, main = "With outliers", xlab = NA, ylab = NA)
#   outlier <- boxplot.stats(var_name)$out
#   mo <- mean(outlier)
#   var_name <- ifelse(var_name %in% outlier, NA, var_name)
#   boxplot(var_name, main = "Without outliers")
#   hist(var_name, main = "Without outliers", xlab = NA, ylab = NA)
#   title("Outlier Check", outer = TRUE)
#   na2 <- sum(is.na(var_name))
#   cat("Outliers identified: ", na2 - na1, "\n")
#   cat("Proportion (%) of outliers: ", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "\n")
#   cat("Mean of the outliers: ", round(mo, 2), "\n")
#   m2 <- mean(var_name, na.rm = TRUE)
#   cat("Mean without removing outliers: ", round(m1, 2), "\n")
#   cat("Mean with removing outliers: ", round(m2, 2), "\n")
#   response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
#   if(response == "y" | response == "yes"){
#     dt[as.character(substitute(var))] <- invisible(var_name)
#     assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
#     cat("Outliers successfully removed", "\n")
#     return(invisible(dt))
#   } else{
#     cat("Nothing changed", "\n")
#     return(invisible(var_name))
#   }
# }

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
AFDW_sub_var <- bartlett.test(AFDW ~ substrate, data = mussel_df)
CI_sub_var <- bartlett.test(CI ~ substrate, data = mussel_df)
AFDW_wb_var <- bartlett.test(AFDW ~ wavebreaker, data = mussel_df)
CI_wb_var <- bartlett.test(CI ~ wavebreaker, data = mussel_df)

# Figner-Killeen Test of Homogeneity of Variances (Secondary Choice)
fligner.test(AFDW ~ substrate, data = mussel_df)
fligner.test(CI ~ substrate, data = mussel_df)
fligner.test(AFDW ~ wavebreaker, data = mussel_df)
fligner.test(CI ~ wavebreaker, data = mussel_df)

# Homogeneity of Variance Plot based on Brown-Forsynth
install.packages("HH")
library(HH)
hov(AFDW ~ substrate, data = mussel_df)
hovPlot(AFDW ~ substrate, data = mussel_df)
hov(CI ~ substrate, data = mussel_df)
hovPlot(CI ~ substrate, data = mussel_df)
# hov(AFDW ~ wavebreaker, data = mussel_df)
# hovPlot(AFDW ~ wavebreaker, data = mussel_df)
# hov(CI ~ wavebreaker, data = mussel_df)
# hovPlot(CI ~ wavebreaker, data = mussel_df)

# ******************************************* One-Way ANOVA: Untransformed Data

CI_ANOVA <- aov(CI ~ factor(substrate))
summary(CI_ANOVA)

# Alternatively...slightly higher precision
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

# Type I ANOVA
summary(aov(mussel_df$AFDW_tuk ~ factor(substrate))) # ANOVA
summary(aov(mussel_df$AFDW_tuk ~ factor(wavebreaker)))

# Type II ANOVA (Or switch to Type III)
AFDWtuk_model <- lm(AFDW_tuk ~ substrate, data = mussel_df)
Anova(AFDWtuk_model, type = "II")

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

# Type I ANOVA
summary(aov(mussel_df$CI_tuk ~ factor(substrate))) # ANOVA
summary(aov(mussel_df$CI_tuk ~ factor(wavebreaker)))

# Type II ANOVA
CItuk_model <- lm(CI_tuk ~ substrate, data = mussel_df)
Anova(CItuk_model, type = "II")

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

# ******************************************* Linear Regression - ML/CI
# Examining the data
sts.ml.ci <- subset(mussel_df, select = c("mussel_length", "CI"))
summary(sts.ml.ci) # Summary of results
cor(sts.ml.ci) # Correlation btwn mussel length & CI

# Plotting the data
plot(sts.ml.ci)

# Fitting linear model
ml.ci.mod <- lm(CI ~ mussel_length, data = mussel_df) #Regression formula
summary(ml.ci.mod) # Regression coefficients table
coef(summary(ml.ci.mod)) # Model Coefficients
confint(ml.ci.mod, level = 0.95) # CIs for model parameters
# fitted(ml.ci.mod) # Predicted values
# residuals(ml.ci.mod) # Residuals
anova(ml.ci.mod) # Anova table
vcov(ml.ci.mod) # Covariance matrix for model parameters
# influence(ml.ci.mod) # Regression diagnostics
# class(ml.ci.mod)
# names(ml.ci.mod)
# methods(class = class(ml.ci.mod))[1:9]

# Check assumptions through plotting (normal distribution, homoscedastic)
plot(ml.ci.mod)

# ******************************************* Linear Regression - MW/CI
# Examining the data
sts.mw.ci <- subset(mussel_df, select = c("mussel_width", "CI"))
summary(sts.mw.ci) # Summary of results
cor(sts.mw.ci) # Correlation btwn mussel width & CI

# Plotting the data
plot(sts.mw.ci)

# Fitting linear model
mw.ci.mod <- lm(CI ~ mussel_width, data = mussel_df) #Regression formula
summary(mw.ci.mod) # Regression coefficients table
coef(summary(mw.ci.mod)) # Model Coefficients
confint(mw.ci.mod, level = 0.95) # CIs for model parameters
# fitted(mw.ci.mod) # Predicted values
# residuals(mw.ci.mod) # Residuals
anova(mw.ci.mod) # Anova table
vcov(mw.ci.mod) # Covariance matrix for model parameters
# influence(mw.ci.mod) # Regression diagnostics
# class(mw.ci.mod)
# names(mw.ci.mod)
# methods(class = class(mw.ci.mod))[1:9]

# Check assumptions through plotting (normal distribution, homoscedastic)
plot(mw.ci.mod)

# ******************************************* Linear Regression - MH/CI
# Examining the data
sts.mh.ci <- subset(mussel_df, select = c("mussel_height", "CI"))
summary(sts.mh.ci) # Summary of results
cor(sts.mh.ci) # Correlation btwn mussel height & XI

# Plotting the data
plot(sts.mh.ci)

# Fitting linear model
mh.ci.mod <- lm(CI ~ mussel_height, data = mussel_df) #Regression formula
summary(mh.ci.mod) # Regression coefficients table
coef(summary(mh.ci.mod)) # Model Coefficients
confint(mh.ci.mod, level = 0.95) # CIs for model parameters
# fitted(mh.ci.mod) # Predicted values
# residuals(mh.ci.mod) # Residuals
anova(mh.ci.mod) # Anova table
vcov(mh.ci.mod) # Covariance matrix for model parameters
# influence(mh.ci.mod) # Regression diagnostics
# class(mh.ci.mod)
# names(mh.ci.mod)
# methods(class = class(mh.ci.mod))[1:9]

# Check assumptions through plotting (normal distribution, homoscedastic)
plot(mh.ci.mod)

# ******************************************* Linear Regression - Substrate and Wavebreaker/CI
# Examining the data
sts.swb.ci <- subset(mussel_df, select = c("substrate", "wavebreaker", "CI"))
summary(sts.swb.ci) # Summary of results

# Fitting linear model
####### CHECK THIS PART: substrate*wavebreaker OR substrate + wavebreaker
swb.ci.mod <- lm(CI ~ substrate * wavebreaker, data = mussel_df) #Regression formula
summary(swb.ci.mod) # Regression coefficients table
coef(summary(swb.ci.mod)) # Model Coefficients
confint(swb.ci.mod, level = 0.95) # CIs for model parameters
# fitted(swb.ci.mod) # Predicted values
# residuals(swb.ci.mod) # Residuals
anova(swb.ci.mod) # Anova table
vcov(swb.ci.mod) # Covariance matrix for model parameters
# influence(swb.ci.mod) # Regression diagnostics
# class(swb.ci.mod)
# names(swb.ci.mod)
# methods(class = class(swb.ci.mod))[1:9]

# Check assumptions through plotting (normal distribution, homoscedastic)
plot(swb.ci.mod)

# **************************************************************************************
# **************************************************************************************

# ******************************************* Generalized Linear Models
# http://www.statmethods.net/advstats/glm.html 
# http://www.stat.columbia.edu/~martin/W2024/R11.pdf 
 
install.packages("glmm")
library(glmm)

# Logistic Regression
m.wb.glm <- glm(wavebreaker ~ mussel_length + mussel_width + mussel_height, 
                   family = binomial(link = "logit"), data = mussel_df)

# Poisson Regression

# Survival Analysis

survival <- as.numeric(mussel_survival$Amount.of.mussels)

# Check to see: wavebreaker*substrate OR wavebreaker + substrate
#               What family of GLMM to use? Bernoulli? Poisson? Binomial?
# Continuous data - Bernoulli (CI, AFDW)
# Counted data - Poisson (Survival)
# Proportion survived (beginning vs. end) - Binomial

# 
# survival.model <- glmm(survival ~ mussel_survival$Wavebreaker + mussel_survival$Substrate, 
#                        random = list(mussel_survival$Plot), data = mussel_df, 
#                        family.glmm = bernoulli.glmm, m = 10^4, debug = TRUE)


# GLMM - CI/AFDW/Length/Width/Height/Survival against Substrate/Wavebreaker/Plot

install.packages("nlme")
library(nlme)

CI.m1 <- lme(CI ~ substrate*wavebreaker, random = ~1|plotnr/squarecode)
CI.m2 <- update(CI.m1, random = ~1|squarecode) # Reducing complexity
anova(CI.m1, CI.m2) # Checking significance 
CI.m3 <- update(CI.m1, random = ~1|plotnr)
anova(CI.m1, CI.m3)

AFDW.m1 <- lme(AFDW ~ substrate*wavebreaker, random = ~1|plotnr/squarecode)
AFDW.m2 <- update(AFDW.m1, random = ~1|squarecode) # Reducing complexity
anova(AFDW.m1, AFDW.m2) # Checking significance 
AFDW.m3 <- update(AFDW.m1, random = ~1|plotnr)
anova(AFDW.m1, AFDW.m3)

# Ignore NA's (na.omit or na.rm)
survival.m1 <- lme(survival ~ substrate*wavebreaker, random = ~1|plotnr/squarecode)
survival.m2 <- update(survival.m1, random = ~1|squarecode) # Reducing complexity
anova(survival.m1, survival.m2) # Checking significance 
survival.m3 <- update(survival.m1, random = ~1|plotnr)
anova(survival.m1, survival.m3)

length.m1 <- lme(mussel_length ~ substrate*wavebreaker, random = ~1|plotnr/squarecode)
length.m2 <- update(length.m1, random = ~1|squarecode) # Reducing complexity
anova(length.m1, length.m2) # Checking significance 
length.m3 <- update(length.m1, random = ~1|plotnr)
anova(length.m1, length.m3)

width.m1 <- lme(mussel_width ~ substrate*wavebreaker, random = ~1|plotnr/squarecode)
width.m2 <- update(width.m1, random = ~1|squarecode) # Reducing complexity
anova(width.m1, width.m2) # Checking significance 
width.m3 <- update(width.m1, random = ~1|plotnr)
anova(width.m1, width.m3)

height.m1 <- lme(mussel_height ~ substrate*wavebreaker, random = ~1|plotnr/squarecode)
height.m2 <- update(height.m1, random = ~1|squarecode) # Reducing complexity
anova(height.m1, height.m2) # Checking significance 
height.m3 <- update(height.m1, random = ~1|plotnr)
anova(height.m1, height.m3)


# Survival against crabs??

# Survival graphs against substrate/wavebreaker

# Growth difference btwn intertidal vs subtidal
