#...

# Package descriptions
# "urca" = Unit Root and Cointegration Tests for Time Series Data
# "rugarch" = Univariate GARCH Models
# "xts" = eXtensible Time Series
# "egcm" = Engle-Granger Cointegration Models
# "EnvStats" = Package for Environmental Statistics, Including US EPA Guidance
# "tidyverse" = Easily Install and Load the 'Tidyverse' packages

# Installing packages
# install.packages("urca")
# install.packages("egcm")
# install.packages("EnvStats")
# install.packages("tidyverse")

# Activating packages
library(urca)
library(rugarch)
library(xts)
library(TTR)
library(egcm)
library(EnvStats)
library(tidyverse)

# Setting working directory
getwd()
setwd("C:/Users/juanf/OneDrive/GitRepos/emf_futures/data")

# Importing .csv data
# dat <- as_tibble(read.csv("MainData01_CSV.csv"))                    # as_tibble function is not a must
dat <- read.csv("MainData01_CSV.csv")
names(dat)
head(dat, n=10)
#attach(dat)

# Data cleaning and sorting
dat$Dates <- as.Date(Dates, "%m/%d/%Y")                             # Convert char into dates
#dat$Dates <- NULL                                                  # Remove a column in df
dat <- dat[order(dat$Dates, decreasing = FALSE),]                   # Sort df by one or more columns

# TO DO ---> Replace all non-numeric values for NA

# Calculating lagged values
dat$FMEMAdjPriceLag1 <- lag(dat$FMEMAdjPrice)
dat$EMFAdjPriceLag1 <- lag(dat$EMFAdjPrice)
dat$VWOAdjPriceLag1 <- lag(dat$VWOAdjPrice)
dat$FTSEPriceLag1 <- lag(dat$FTSEPrice)                             # Not sure why it was multiplied by 100

# Calculating returns - daily
dat$FMEMAdjDreturn <- (dat$FMEMAdjPrice / dat$FMEMAdjPriceLag1) - 1
dat$EMFAdjDreturn <- (dat$EMFAdjPrice / dat$EMFAdjPriceLag1) - 1
dat$VWOAdjDreturn <- (dat$VWOAdjPrice / dat$VWOAdjPriceLag1) - 1
dat$FTSEDreturn <- (dat$FTSEPrice / dat$FTSEPriceLag1) - 1

# Summary statistics of returns
summary(dat$FMEMAdjDreturn)
summary(dat$EMFAdjDreturn)
summary(dat$VWOAdjDreturn)
summary(dat$FTSEDreturn)

# Estimating the Hedge Ratio (OLS)
lmodel <- lm(VWOAdjDreturn ~ EMFAdjDreturn, data = dat)
summary(lmodel)
slope <- lmodel$coefficients[2]
intercept <- lmodel$coefficients[1]

dat %>%
  ggplot(aes(x = EMFAdjDreturn, y = VWOAdjDreturn)) +
  geom_point(colour = "red") +
  geom_smooth(method = "lm", fill = NA)

hr_EMF_VWO <- cov(dat$VWOAdjDreturn,dat$EMFAdjDreturn, use = "pairwise.complete.obs") / var(dat$EMFAdjDreturn, na.rm=TRUE)
hr_EMF_VWO
