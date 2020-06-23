# EMF FUTURES ##################################################################

## Installing and loading packages =============================================
# install.packages("urca")
# install.packages("egcm")
# install.packages("EnvStats")
# install.packages("tidyverse")
install.packages("pacman")
pacman::p_load(pacman,
               dplyr,
               tidyr,
               stringr,
               lubridate,
               httr,
               ggvis,
               ggplot2,
               shiny,
               rio,
               rmarkdown
               )
library(datasets)

## Setting working directory ===================================================
getwd()
setwd("C:/Users/juanf/OneDrive/GitRepos/emf_futures/data")

## Importing data ==============================================================
# dat <- as_tibble(read.csv("MainData01_CSV.csv"))
dat <- read.csv("MainData01_CSV.csv")
# names(dat)
# head(dat, n = 5)
#attach(dat)

## Data cleaning and sorting ===================================================
dat$Dates <- as.Date(dat$Dates, "%m/%d/%Y")           # Convert char into dates
#dat$Dates <- NULL                                    # Remove a column
dat <- dat[order(dat$Dates, decreasing = FALSE),]     # Sort by column(s)

## Calculating the Hedge Ratio =================================================
# Calculating lagged values
dat$FMEMAdjPriceLag1 <- lag(dat$FMEMAdjPrice)
dat$EMFAdjPriceLag1 <- lag(dat$EMFAdjPrice)
dat$VWOAdjPriceLag1 <- lag(dat$VWOAdjPrice)
dat$FTSEPriceLag1 <- lag(dat$FTSEPrice)

# Calculating returns - daily
dat$FMEMAdjDreturn <- (dat$FMEMAdjPrice / dat$FMEMAdjPriceLag1) - 1
dat$EMFAdjDreturn <- (dat$EMFAdjPrice / dat$EMFAdjPriceLag1) - 1
dat$VWOAdjDreturn <- (dat$VWOAdjPrice / dat$VWOAdjPriceLag1) - 1
dat$FTSEDreturn <- (dat$FTSEPrice / dat$FTSEPriceLag1) - 1

# Subsetting data to keep relevant variables
keepvars <- c("Dates", 
              "FMEMAdjDreturn",
              "EMFAdjDreturn", 
              "VWOAdjDreturn", 
              "FTSEDreturn"
              )
dat2 <- dat[keepvars]
# head(dat2)

# Summary statistics of returns
summary(dat2$FMEMAdjDreturn)
summary(dat2$EMFAdjDreturn)
summary(dat2$VWOAdjDreturn)
summary(dat2$FTSEDreturn)

# Estimating the Hedge Ratio (OLS)
lmodel <- lm(VWOAdjDreturn ~ EMFAdjDreturn, data = dat2)
summary(lmodel)
slope <- lmodel$coefficients[2]
intercept <- lmodel$coefficients[1]

dat2 %>%
  ggplot(aes(x = EMFAdjDreturn, y = VWOAdjDreturn)) +
  geom_point(colour = "red") +
  geom_smooth(method = "lm", fill = NA)

hr_EMF_VWO <- cov(dat$VWOAdjDreturn,dat$EMFAdjDreturn, use = "pairwise.complete.obs") / var(dat$EMFAdjDreturn, na.rm=TRUE)
hr_EMF_VWO
