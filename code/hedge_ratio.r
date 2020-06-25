# EMF FUTURES ##################################################################

## Installing and loading packages =============================================
# install.packages("urca")
# install.packages("egcm")
# install.packages("EnvStats")
# install.packages("tidyverse")

# install.packages("pacman")
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
               rmarkdown,
               tibble
               )

## Setting working directory ===================================================
getwd()
setwd("C:/Users/juanf/OneDrive/GitRepos/emf_futures/data")

## Importing data ==============================================================
# rm(list = ls())
dat <- as_tibble(read.csv("MainData01_CSV.csv"))
# dat <- read.csv("MainData01_CSV.csv")
# names(dat)
# head(dat, n = 5)
# attach(dat)

## Data cleaning and sorting ===================================================
dat$Dates <- as.Date(dat$Dates, "%m/%d/%Y")           # Convert char into dates
dat <- dat[order(dat$Dates, decreasing = FALSE),]     # Sort by column(s)

## Creating key variables ======================================================

# Calculating lagged values
dat$FMEMAdjPriceLag1 <- lag(dat$FMEMAdjPrice)
dat$EMFAdjPriceLag1 <- lag(dat$EMFAdjPrice)
dat$VWOAdjPriceLag1 <- lag(dat$VWOAdjPrice)
dat$FTSEPriceLag1 <- lag(dat$FTSEPrice)

# Calculating returns - daily
dat$emf_dret <- (dat$EMFAdjPrice / dat$EMFAdjPriceLag1) - 1
dat$fmem_dret <- (dat$FMEMAdjPrice / dat$FMEMAdjPriceLag1) - 1
dat$vwo_dret <- (dat$VWOAdjPrice / dat$VWOAdjPriceLag1) - 1
dat$ftse_dret <- (dat$FTSEPrice / dat$FTSEPriceLag1) - 1

# Subsetting data to keep relevant variables
keepvars <- c("emf_dret",
              "fmem_dret",
              "vwo_dret",
              "ftse_dret"
              )
dat2 <- dat[keepvars]

# Summary statistics of key variables ==========================================
head(dat2)
summary(dat2)
plot(dat2)

## Calculating the Hedge Ratio =================================================

# Calculating the hedge ratio for all pairs (VAR-COV)
hedge.ratio <- function(xvar, yvar) {
  cov(yvar,
      xvar,
      use = "pairwise.complete.obs"
  ) / var(xvar, na.rm=TRUE)
  }

hr_emf_vwo <- hedge.ratio(dat2$emf_dret, dat2$vwo_dret)
hr_emf_ftse <- hedge.ratio(dat2$emf_dret, dat2$ftse_dret)
hr_fmem_vwo <- hedge.ratio(dat2$fmem_dret, dat2$vwo_dret)
hr_fmem_ftse <- hedge.ratio(dat2$fmem_dret, dat2$ftse_dret)
hr_vwo_ftse <- hedge.ratio(dat2$vwo_dret, dat2$ftse_dret)

# Code to loop through columns of dataframe (IGNORE)
# for (col1 in 1:ncol(dat2)) {
#   for (col2 in 1:ncol(dat2)) {
#     # print(dat2[,col1])
#     name <- paste("hr", col1, col2, sep="_")
#     assign(name, hedge.ratio(dat2[,col1], dat2[,col2]))
#   }
# }

# Code to calculate the VAR-COV matrix (IGNORE)
# cov_matrix <- cov(dat2[,1:ncol(dat2)], use = "pairwise.complete.obs")

# Estimating the Hedge Ratio (OLS) for VWO ~ EMF
lm_emf_vwo <- lm(VWOAdjDReturn ~ EMFAdjDReturn, data = dat2)
summary(lm_emf_vwo)
# slope <- lm_emf_vwo$coefficients[2]
# intercept <- lm_emf_vwo$coefficients[1]

lm_fmem_vwo <- lm(VWOAdjDReturn ~ FMEMAdjDReturn, data = dat2)
summary(lm_fmem_vwo)

# Plotting the regression line (ggplot)
dat2 %>%
  ggplot(aes(x = EMFAdjDReturn, y = VWOAdjDReturn)) +
  geom_point(colour = "red") +
  geom_smooth(method = "lm", fill = NA)


# Plotting the regression line (plot)
plot(dat2$EMFAdjDReturn, dat2$VWOAdjDReturn,
     col = "red3",
     pch = 19,
     main = "EMF vs. VWO Daily Returns",
     xlab = "EMF Return",
     ylab = "VWO Return"
)
abline(lm(dat2$VWOAdjDReturn ~ dat2$EMFAdjDReturn))
