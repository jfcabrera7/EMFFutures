#Hedge Estimation

#EMF  FMEZ =FTSE EMERGING MARKET
#FMEM   MESZ =MSCI EMERGING MARKETS INDEX FUTURES

#install.packages("urca")
#library(urca)
library(rugarch)
library(xts)
library(TTR)
install.packages("egcm")
library(egcm)
library(urca)

install.packages("EnvStats")
library(EnvStats)

dat <- read.csv("~/Desktop/Desktop/Eleni/DATA/Price_ADJUSTED.csv", stringsAsFactors = F)
attach(dat)

#Removing NAs
# na_vwo <- is.na(vwo) #vectorized operation
# dat <- dat[!na_vwo,]
# na_FEMZ4_trade_price <- is.na(FEMZ4_trade_price) #vectorized operation
# dat <- dat[!na_FEMZ4_trade_price,]


#Calculating Lagged Values (For Retuen Calculation)
#Calculating the second period values 
MESZ4_Trade_price_1 <- MESZ4_Trade_price[-1]
FEMZ4_trade_price_1 <- FEMZ4_trade_price[-1]
vwo_1 <- vwo[-1]
FTAG01_Price <- FTAG01_Price*100
FTAG01_Price_1 <- FTAG01_Price[-1]

# Calculating the returns (Daily)
delta_VWO <- (vwo_1[1:265] - vwo[1:265])/vwo[1:265]
delta_FTSE <- (FTAG01_Price_1[1:265] - FTAG01_Price[1:265])/FTAG01_Price[1:265]
delta_EMF <- (FEMZ4_trade_price_1[1:265] - FEMZ4_trade_price[1:265])/FEMZ4_trade_price[1:265]
delta_FMEM <- (MESZ4_Trade_price_1[1:265] - MESZ4_Trade_price[1:265])/MESZ4_Trade_price[1:265]


#BID-ASK RETURN

#Long Spot # Short Futures

vwo_bid_price_1 <- vwo_bid_price[-1] 	
vwo_ask_price_1 <- vwo_ask_price[-1]

FEMZ4_bid_price_1 <- FEMZ4_bid_price[-1]
FEMZ4_ask_price_1 <- FEMZ4_ask_price[-1]

MESZ4_bid_price_1 <- MESZ4_bid_price[-1]
MESZ4_ask_price_1 <- MESZ4_ask_price[-1]


# Calculating the returns (Daily) Long _short
delta_VWO <- (vwo_bid_price_1[1:265] - vwo_ask_price[1:265])/vwo_ask_price[1:265]
#delta_FTSE <- (FTAG01_Price_1[1:265] - FTAG01_Price[1:265])/FTAG01_Price[1:265]
delta_EMF <- (FEMZ4_ask_price_1[1:265] - FEMZ4_bid_price[1:265])/FEMZ4_bid_price[1:265]
delta_FMEM <- (MESZ4_ask_price_1[1:265] - MESZ4_bid_price[1:265])/MESZ4_bid_price[1:265]

#Calculating the return (daily Short_Long)
delta_VWO <- (vwo_ask_price_1[1:265] - vwo_bid_price[1:265])/vwo_bid_price[1:265]
#delta_FTSE <- (FTAG01_Price_1[1:265] - FTAG01_Price[1:265])/FTAG01_Price[1:265]
delta_EMF <- (FEMZ4_bid_price_1[1:265] - FEMZ4_ask_price[1:265])/FEMZ4_ask_price[1:265]
delta_FMEM <- (MESZ4_bid_price_1[1:265] - MESZ4_ask_price[1:265])/MESZ4_ask_price[1:265]


#Estimating Hedge OLS Ratio
h_EMF_VWO <- cov(delta_VWO,delta_EMF)/var(delta_EMF)
h_EMF_VWO
h_FMEM_VWO <-cov(delta_VWO,delta_FMEM)/var(delta_FMEM)
h_FMEM_VWO
h_EMF_FTSE <- cov(delta_FTSE,delta_EMF)/var(delta_EMF)
h_EMF_FTSE
h_FMEM_FTSE <- cov(delta_FTSE,delta_FMEM)/var(delta_FMEM)
h_FMEM_FTSE
h_VWO_FTSE <- cov(delta_FTSE,delta_VWO)/var(delta_VWO)
h_VWO_FTSE



#Calculating Hedge Effectiveness using the previously calculated hedge OLS ratio

var_delta_VWO <- var(delta_VWO)
var_h_EMF_VWO <- var(delta_VWO) + h_EMF_VWO^2 * var(delta_EMF) - 2*h_EMF_VWO*cov(delta_VWO,delta_EMF)
he_EMF_VWO <- 1- (var_h_EMF_VWO/var_delta_VWO)
he_EMF_VWO 

var_delta_VWO <- var(delta_VWO)
var_h_FMEM_VWO <- var(delta_VWO) + h_FMEM_VWO^2 * var(delta_FMEM) - 2*h_FMEM_VWO*cov(delta_VWO,delta_FMEM)
he_FMEM_VWO <- 1- (var_h_FMEM_VWO/var_delta_VWO)
he_FMEM_VWO

var_delta_FTSE <- var(delta_FTSE)
var_h_EMF_FTSE <- var(delta_FTSE) + h_EMF_FTSE^2 * var(delta_EMF) - 2*h_EMF_FTSE*cov(delta_FTSE,delta_EMF)
he_EMF_FTSE <- 1- (var_h_EMF_FTSE/var_delta_FTSE)
he_EMF_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_FMEM_FTSE <- var(delta_FTSE) + h_FMEM_FTSE^2 * var(delta_FMEM) - 2*h_FMEM_FTSE*cov(delta_FTSE,delta_FMEM)
he_FMEM_FTSE <- 1- (var_h_FMEM_FTSE/var_delta_FTSE)
he_FMEM_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_VWO_FTSE <- var(delta_FTSE) + h_VWO_FTSE^2 * var(delta_VWO) - 2*h_VWO_FTSE*cov(delta_FTSE,delta_VWO)
he_VWO_FTSE <- 1- (var_h_VWO_FTSE/var_delta_FTSE)
he_VWO_FTSE

#bid_ask_EMF <- FMEZ_ask_Price - FMEZ_bid_Price

bid_ask_EMF <- FEMZ4_ask_price - FEMZ4_bid_price
bid_ask_FMEM <- MESZ4_ask_price - MESZ4_bid_price


fit_EMF_VWO <- lm((delta_VWO + h_EMF_VWO*delta_EMF)[1:255] ~ bid_ask_EMF[2:256] + OPEN_INT_EMF[2:256])
summary(fit_EMF_VWO)

fit_FMEM_VWO <- lm((delta_VWO + h_FMEM_VWO*delta_FMEM)[1:255] ~ bid_ask_FMEM[2:256] + OPEN_INT_FMEM[2:256] 
                   + PX_VOLUME_FMEM[2:256])
summary(fit_FMEM_VWO)

fit_EMF_FTSE <- lm(delta_FTSE + h_EMF_FTSE*delta_EMF ~ bid_ask_EMF[2:266]+ OPEN_INT[2:256])
summary(fit_EMF_FTSE)

fit_FMEM_FTSE <- lm(delta_FTSE + h_FMEM_FTSE*delta_FMEM ~ bid_ask_FMEM[2:266])
summary(fit_FMEM_FTSE)


#Mean of the hedged position

mean_EMF_VWO <- mean(delta_VWO) + h_EMF_VWO*mean(delta_EMF)
mean_EMF_VWO


mean_FMEM_VWO <- mean(delta_VWO) + h_FMEM_VWO*mean(delta_FMEM)
mean_FMEM_VWO


mean_EMF_FTSE <- mean(delta_FTSE) + h_EMF_FTSE*mean(delta_EMF)
mean_EMF_FTSE


mean_FMEM_FTSE <- mean(delta_FTSE) + h_FMEM_FTSE*mean(delta_FMEM)
mean_FMEM_FTSE



mean_VWO_FTSE <- mean(delta_FTSE) + h_VWO_FTSE*mean(delta_VWO)
mean_VWO_FTSE



t.test(x=delta_VWO + h_EMF_VWO*delta_EMF)
t.test(x=delta_VWO + h_EMF_VWO*delta_FMEM)
t.test(x=delta_FTSE + h_EMF_FTSE*delta_EMF)
t.test(x=delta_FTSE + h_FMEM_FTSE*delta_FMEM)
t.test(x=delta_FTSE + h_VWO_FTSE*delta_VWO)




#two sample t-test
t.test((x=delta_VWO + h_EMF_VWO*delta_EMF), (y=delta_VWO + h_EMF_VWO*delta_FMEM))
t.test((x=delta_FTSE + h_EMF_FTSE*delta_EMF), (y=delta_FTSE + h_FMEM_FTSE*delta_FMEM))

var.test((x=delta_VWO + h_EMF_VWO*delta_EMF), (y=delta_VWO + h_EMF_VWO*delta_FMEM))
var.test((x=delta_FTSE + h_EMF_FTSE*delta_EMF), (y=delta_FTSE + h_FMEM_FTSE*delta_FMEM))

varTest(x=delta_VWO + h_EMF_VWO*delta_EMF)
chisq.test(delta_VWO + h_EMF_VWO*delta_EMF)
varTest(x=delta_VWO + h_EMF_VWO*delta_FMEM)
varTest(x=delta_FTSE + h_EMF_FTSE*delta_EMF)
varTest(x=delta_FTSE + h_FMEM_FTSE*delta_FMEM)
varTest(x=delta_FTSE + h_VWO_FTSE*delta_VWO)

varTest(delta_VWO + h_EMF_VWO*delta_EMF, alternative = "two.sided", conf.level = 0.95, 
        sigma.squared = 0, data.name = NULL)

#Variance of the hedged position

var_h_EMF_VWO
var_h_FMEM_VWO
var_h_EMF_FTSE
var_h_FMEM_FTSE
var_h_VWO_FTSE

#NAIVE 1-1 hedging Hedge Effectiveness

var_delta_VWO <- var(delta_VWO)
var_h_EMF_VWO <- var(delta_VWO) + 1 * var(delta_EMF) - 2*1*cov(delta_VWO,delta_EMF)
he_EMF_VWO <- 1- (var_h_EMF_VWO/var_delta_VWO)
he_EMF_VWO 

var_delta_VWO <- var(delta_VWO)
var_h_FMEM_VWO <- var(delta_VWO) + 1 * var(delta_FMEM) - 2*1*cov(delta_VWO,delta_FMEM)
he_FMEM_VWO <- 1- (var_h_FMEM_VWO/var_delta_VWO)
he_FMEM_VWO

var_delta_FTSE <- var(delta_FTSE)
var_h_EMF_FTSE <- var(delta_FTSE) + 1 * var(delta_EMF) - 2*1*cov(delta_FTSE,delta_EMF)
he_EMF_FTSE <- 1- (var_h_EMF_FTSE/var_delta_FTSE)
he_EMF_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_FMEM_FTSE <- var(delta_FTSE) + 1 * var(delta_FMEM) - 2*1*cov(delta_FTSE,delta_FMEM)
he_FMEM_FTSE <- 1- (var_h_FMEM_FTSE/var_delta_FTSE)
he_FMEM_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_VWO_FTSE <- var(delta_FTSE) + 1 * var(delta_VWO) - 2*1*cov(delta_FTSE,delta_VWO)
he_VWO_FTSE <- 1- (var_h_VWO_FTSE/var_delta_FTSE)
he_VWO_FTSE

#Accounting for Cointegration

#Cointegration hedge ration=exo Cointegration hedge effectiveness=ze
e1_VWO_EMF <- egcm(delta_EMF,delta_VWO)
e2_VWO_EMF <- egcm(delta_VWO,delta_EMF)
i1_VWO_EMF <- e1_VWO_EMF$innovations
i2_VWO_EMF <- e2_VWO_EMF$innovations
exo_VWO_EMF <- abs(cov(i1_VWO_EMF,i2_VWO_EMF)/var(i2_VWO_EMF))
exo_VWO_EMF
var_z_VWO_EMF <- var(delta_VWO) + exo_VWO_EMF^2 * var(delta_EMF) - 2*exo_VWO_EMF*cov(delta_VWO,delta_EMF)
ze_VWO_EMF <- 1- (var_z_VWO_EMF/var(delta_VWO))
ze_VWO_EMF

e1_VWO_FMEM <- egcm(delta_FMEM,delta_VWO)
e2_VWO_FMEM <- egcm(delta_VWO,delta_FMEM)
i1_VWO_FMEM <- e1_VWO_FMEM$innovations
i2_VWO_FMEM <- e2_VWO_FMEM$innovations
exo_VWO_FMEM <- abs(cov(i1_VWO_FMEM,i2_VWO_FMEM)/var(i2_VWO_FMEM))
exo_VWO_FMEM
var_z_VWO_FMEM <- var(delta_VWO) + exo_VWO_FMEM^2 * var(delta_FMEM) - 2*exo_VWO_FMEM*cov(delta_VWO,delta_FMEM)
ze_VWO_FMEM <- 1- (var_z_VWO_FMEM/var(delta_VWO))
ze_VWO_FMEM

e1_FTSE_EMF <- egcm(delta_EMF,delta_FTSE)
e2_FTSE_EMF <- egcm(delta_FTSE,delta_EMF)
i1_FTSE_EMF <- e1_FTSE_EMF$innovations
i2_FTSE_EMF <- e2_FTSE_EMF$innovations
exo_FTSE_EMF <- abs(cov(i1_FTSE_EMF,i2_FTSE_EMF)/var(i2_FTSE_EMF))
exo_FTSE_EMF
var_z_FTSE_EMF <- var(delta_FTSE) + exo_FTSE_EMF^2 * var(delta_EMF) - 2*exo_FTSE_EMF*cov(delta_FTSE,delta_EMF)
ze_FTSE_EMF <- 1- (var_z_FTSE_EMF/var(delta_FTSE))
ze_FTSE_EMF

e1_FTSE_FMEM <- egcm(delta_FMEM,delta_FTSE)
e2_FTSE_FMEM <- egcm(delta_FTSE,delta_FMEM)
i1_FTSE_FMEM <- e1_FTSE_FMEM$innovations
i2_FTSE_FMEM <- e2_FTSE_FMEM$innovations
exo_FTSE_FMEM <- abs(cov(i1_FTSE_FMEM,i2_FTSE_FMEM)/var(i2_FTSE_FMEM))
exo_FTSE_FMEM
var_z_FTSE_FMEM <- var(delta_FTSE) + exo_FTSE_FMEM^2 * var(delta_FMEM) - 2*exo_FTSE_FMEM*cov(delta_FTSE,delta_FMEM)
ze_FTSE_FMEM <- 1- (var_z_FTSE_FMEM/var(delta_FTSE))
ze_FTSE_FMEM


e1_FTSE_VWO <- egcm(delta_VWO,delta_FTSE)
e2_FTSE_VWO <- egcm(delta_FTSE,delta_VWO)
i1_FTSE_VWO <- e1_FTSE_VWO$innovations
i2_FTSE_VWO <- e2_FTSE_VWO$innovations
exo_FTSE_VWO <- abs(cov(i1_FTSE_VWO,i2_FTSE_VWO)/var(i2_FTSE_VWO))
exo_FTSE_VWO
var_z_FTSE_VWO <- var(delta_FTSE) + exo_FTSE_VWO^2 * var(delta_VWO) - 2*exo_FTSE_VWO*cov(delta_FTSE,delta_VWO)
ze_FTSE_VWO <- 1- (var_z_FTSE_VWO/var(delta_FTSE))
ze_FTSE_VWO


#Co-integration formula
#Defining X & Y
#x <- delta2_S 
#y <- delta1_F1

#Cointegration
#e1 <- egcm(y,x)
#e2 <- egcm(x,y)
#i1 <- e1$innovations
#i2 <- e2$innovations
#exo <- abs(cov(i1,i2)/var(i2))
#exo
#var_z <- var(x) + exo^2 * var(y) - 2*exo*cov(x,y)
#ze <- 1- (var_z/var(x))
#ze




#Residual Risk

fit <- lm(delta_VWO[1:265]~delta_EMF[1:265])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_VWO[1:265]~delta_FMEM[1:265])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:265]~delta_EMF[1:265])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:265]~delta_FMEM[1:265])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:265]~delta_VWO[1:265])
RR <- 1- summary(fit)$r.squared
RR

#var(delta_VWO[1:265])/mean(vwo[1:266])
#var(delta_FTSE[1:265])/mean(FTAG01_Price[1:266])

#var(delta_EMF[1:265])/mean(FEMZ4_trade_price[1:266])
#var(delta_FMEM[1:265])/mean(MESZ4_Trade_price[1:266])


#Non-rolling-5 days #Go to Price_1 CSV

dat <- read.csv("~/Desktop/Desktop/Eleni/DATA/Price_1.csv", stringsAsFactors = F)
attach(dat)

#Calculating Lagged Values (For Retuen Calculation)
#Calculating the second period values 
MESZ4_Trade_price_1 <- MESZ4_Trade_price[-1]
FEMZ4_trade_price_1 <- FEMZ4_trade_price[-1]
vwo_1 <- vwo[-1]
FTAG01_Price <- FTAG01_Price*100
FTAG01_Price_1 <- FTAG01_Price[-1]

# Calculating the returns (Weekly)
delta_VWO <- (vwo_1[1:52] - vwo[1:52])/vwo[1:52]
delta_FTSE <- (FTAG01_Price_1[1:52] - FTAG01_Price[1:52])/FTAG01_Price[1:52]
delta_EMF <- (FEMZ4_trade_price_1[1:52] - FEMZ4_trade_price[1:52])/FEMZ4_trade_price[1:52]
delta_FMEM <- (MESZ4_Trade_price_1[1:52] - MESZ4_Trade_price[1:52])/MESZ4_Trade_price[1:52]


#BID-ASK RETURN

#Long Spot # Short Futures

vwo_bid_price_1 <- vwo_bid_price[-1] 	
vwo_ask_price_1 <- vwo_ask_price[-1]

FEMZ4_bid_price_1 <- FEMZ4_bid_price[-1]
FEMZ4_ask_price_1 <- FEMZ4_ask_price[-1]

MESZ4_bid_price_1 <- MESZ4_bid_price[-1]
MESZ4_ask_price_1 <- MESZ4_ask_price[-1]


# Calculating the returns (Daily) Long _short
delta_VWO <- (vwo_bid_price_1[1:52] - vwo_ask_price[1:52])/vwo_ask_price[1:52]
#delta_FTSE <- (FTAG01_Price_1[1:265] - FTAG01_Price[1:265])/FTAG01_Price[1:265]
delta_EMF <- (FEMZ4_ask_price_1[1:52] - FEMZ4_bid_price[1:52])/FEMZ4_bid_price[1:52]
delta_FMEM <- (MESZ4_ask_price_1[1:52] - MESZ4_bid_price[1:52])/MESZ4_bid_price[1:52]

#Calculating the return (daily Short_Long)
delta_VWO <- (vwo_ask_price_1[1:52] - vwo_bid_price[1:52])/vwo_bid_price[1:52]
#delta_FTSE <- (FTAG01_Price_1[1:265] - FTAG01_Price[1:265])/FTAG01_Price[1:265]
delta_EMF <- (FEMZ4_bid_price_1[1:52] - FEMZ4_ask_price[1:52])/FEMZ4_ask_price[1:52]
delta_FMEM <- (MESZ4_bid_price_1[1:52] - MESZ4_ask_price[1:52])/MESZ4_ask_price[1:52]




#Estimating Hedge Ratio
h_EMF_VWO <- cov(delta_VWO,delta_EMF)/var(delta_EMF)
h_EMF_VWO
h_FMEM_VWO <-cov(delta_VWO,delta_FMEM)/var(delta_FMEM)
h_FMEM_VWO
h_EMF_FTSE <- cov(delta_FTSE,delta_EMF)/var(delta_EMF)
h_EMF_FTSE
h_FMEM_FTSE <- cov(delta_FTSE,delta_FMEM)/var(delta_FMEM)
h_FMEM_FTSE
h_VWO_FTSE <- cov(delta_FTSE,delta_VWO)/var(delta_VWO)
h_VWO_FTSE


#Calculating Hedge Effectiveness using the previously calculated hedge ratio

var_delta_VWO <- var(delta_VWO)
var_h_EMF_VWO <- var(delta_VWO) + h_EMF_VWO^2 * var(delta_EMF) - 2*h_EMF_VWO*cov(delta_VWO,delta_EMF)
he_EMF_VWO <- 1- (var_h_EMF_VWO/var_delta_VWO)
he_EMF_VWO 

var_delta_VWO <- var(delta_VWO)
var_h_FMEM_VWO <- var(delta_VWO) + h_FMEM_VWO^2 * var(delta_FMEM) - 2*h_FMEM_VWO*cov(delta_VWO,delta_FMEM)
he_FMEM_VWO <- 1- (var_h_FMEM_VWO/var_delta_VWO)
he_FMEM_VWO

var_delta_FTSE <- var(delta_FTSE)
var_h_EMF_FTSE <- var(delta_FTSE) + h_EMF_FTSE^2 * var(delta_EMF) - 2*h_EMF_FTSE*cov(delta_FTSE,delta_EMF)
he_EMF_FTSE <- 1- (var_h_EMF_FTSE/var_delta_FTSE)
he_EMF_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_FMEM_FTSE <- var(delta_FTSE) + h_FMEM_FTSE^2 * var(delta_FMEM) - 2*h_FMEM_FTSE*cov(delta_FTSE,delta_FMEM)
he_FMEM_FTSE <- 1- (var_h_FMEM_FTSE/var_delta_FTSE)
he_FMEM_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_VWO_FTSE <- var(delta_FTSE) + h_VWO_FTSE^2 * var(delta_VWO) - 2*h_VWO_FTSE*cov(delta_FTSE,delta_VWO)
he_VWO_FTSE <- 1- (var_h_VWO_FTSE/var_delta_FTSE)
he_VWO_FTSE

#Mean of the hedged position

mean_EMF_VWO <- mean(delta_VWO) + h_EMF_VWO*mean(delta_EMF)
mean_EMF_VWO
mean_FMEM_VWO <- mean(delta_VWO) + h_FMEM_VWO*mean(delta_FMEM)
mean_FMEM_VWO
mean_EMF_FTSE <- mean(delta_FTSE) + h_EMF_FTSE*mean(delta_EMF)
mean_EMF_FTSE
mean_FMEM_FTSE <- mean(delta_FTSE) + h_FMEM_FTSE*mean(delta_FMEM)
mean_FMEM_FTSE
mean_VWO_FTSE <- mean(delta_FTSE) + h_VWO_FTSE*mean(delta_VWO)
mean_VWO_FTSE


t.test(x=delta_VWO + h_EMF_VWO*delta_EMF)
t.test(x=delta_VWO + h_EMF_VWO*delta_FMEM)
t.test(x=delta_FTSE + h_EMF_FTSE*delta_EMF)
t.test(x=delta_FTSE + h_FMEM_FTSE*delta_FMEM)
t.test(x=delta_FTSE + h_VWO_FTSE*delta_VWO)


#two sample t-test
t.test((x=delta_VWO + h_EMF_VWO*delta_EMF), (y=delta_VWO + h_EMF_VWO*delta_FMEM))
t.test((x=delta_FTSE + h_EMF_FTSE*delta_EMF), (y=delta_FTSE + h_FMEM_FTSE*delta_FMEM))


var.test((x=delta_VWO + h_EMF_VWO*delta_EMF), (y=delta_VWO + h_EMF_VWO*delta_FMEM))
var.test((x=delta_FTSE + h_EMF_FTSE*delta_EMF), (y=delta_FTSE + h_FMEM_FTSE*delta_FMEM))



#Variance of the hedged position

var_h_EMF_VWO
var_h_FMEM_VWO
var_h_EMF_FTSE
var_h_FMEM_FTSE
var_h_VWO_FTSE


#Regression of Hedged Position on Bid-ask Spread

bid_ask_EMF <- FEMZ4_ask_price - FEMZ4_bid_price
bid_ask_FMEM <- MESZ4_ask_price - MESZ4_bid_price


fit_EMF_VWO <- lm(delta_VWO + h_EMF_VWO*delta_EMF ~ bid_ask_EMF[2:53])
summary(fit_EMF_VWO)

fit_FMEM_VWO <- lm(delta_VWO + h_FMEM_VWO*delta_FMEM ~ bid_ask_FMEM[2:53])
summary(fit_FMEM_VWO)

fit_EMF_FTSE <- lm(delta_FTSE + h_EMF_FTSE*delta_EMF ~ bid_ask_EMF[2:53])
summary(fit_EMF_FTSE)

fit_FMEM_FTSE <- lm(delta_FTSE + h_FMEM_FTSE*delta_FMEM ~ bid_ask_FMEM[2:53])
summary(fit_FMEM_FTSE)




#NAIVE 1-1 hedging

var_delta_VWO <- var(delta_VWO)
var_h_EMF_VWO <- var(delta_VWO) + 1 * var(delta_EMF) - 2*1*cov(delta_VWO,delta_EMF)
he_EMF_VWO <- 1- (var_h_EMF_VWO/var_delta_VWO)
he_EMF_VWO 

var_delta_VWO <- var(delta_VWO)
var_h_FMEM_VWO <- var(delta_VWO) + 1 * var(delta_FMEM) - 2*1*cov(delta_VWO,delta_FMEM)
he_FMEM_VWO <- 1- (var_h_FMEM_VWO/var_delta_VWO)
he_FMEM_VWO

var_delta_FTSE <- var(delta_FTSE)
var_h_EMF_FTSE <- var(delta_FTSE) + 1 * var(delta_EMF) - 2*1*cov(delta_FTSE,delta_EMF)
he_EMF_FTSE <- 1- (var_h_EMF_FTSE/var_delta_FTSE)
he_EMF_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_FMEM_FTSE <- var(delta_FTSE) + 1 * var(delta_FMEM) - 2*1*cov(delta_FTSE,delta_FMEM)
he_FMEM_FTSE <- 1- (var_h_FMEM_FTSE/var_delta_FTSE)
he_FMEM_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_VWO_FTSE <- var(delta_FTSE) + 1 * var(delta_VWO) - 2*1*cov(delta_FTSE,delta_VWO)
he_VWO_FTSE <- 1- (var_h_VWO_FTSE/var_delta_FTSE)
he_VWO_FTSE



#Cointegration hedge ration=z Cointegration hedge effectiveness=ze
e1_VWO_EMF <- egcm(delta_EMF,delta_VWO)
e2_VWO_EMF <- egcm(delta_VWO,delta_EMF)
i1_VWO_EMF <- e1_VWO_EMF$innovations
i2_VWO_EMF <- e2_VWO_EMF$innovations
exo_VWO_EMF <- abs(cov(i1_VWO_EMF,i2_VWO_EMF)/var(i2_VWO_EMF))
exo_VWO_EMF
var_z_VWO_EMF <- var(delta_VWO) + exo_VWO_EMF^2 * var(delta_EMF) - 2*exo_VWO_EMF*cov(delta_VWO,delta_EMF)
ze_VWO_EMF <- 1- (var_z_VWO_EMF/var(delta_VWO))
ze_VWO_EMF

e1_VWO_FMEM <- egcm(delta_FMEM,delta_VWO)
e2_VWO_FMEM <- egcm(delta_VWO,delta_FMEM)
i1_VWO_FMEM <- e1_VWO_FMEM$innovations
i2_VWO_FMEM <- e2_VWO_FMEM$innovations
exo_VWO_FMEM <- abs(cov(i1_VWO_FMEM,i2_VWO_FMEM)/var(i2_VWO_FMEM))
exo_VWO_FMEM
var_z_VWO_FMEM <- var(delta_VWO) + exo_VWO_FMEM^2 * var(delta_FMEM) - 2*exo_VWO_FMEM*cov(delta_VWO,delta_FMEM)
ze_VWO_FMEM <- 1- (var_z_VWO_FMEM/var(delta_VWO))
ze_VWO_FMEM

e1_FTSE_EMF <- egcm(delta_EMF,delta_FTSE)
e2_FTSE_EMF <- egcm(delta_FTSE,delta_EMF)
i1_FTSE_EMF <- e1_FTSE_EMF$innovations
i2_FTSE_EMF <- e2_FTSE_EMF$innovations
exo_FTSE_EMF <- abs(cov(i1_FTSE_EMF,i2_FTSE_EMF)/var(i2_FTSE_EMF))
exo_FTSE_EMF
var_z_FTSE_EMF <- var(delta_FTSE) + exo_FTSE_EMF^2 * var(delta_EMF) - 2*exo_FTSE_EMF*cov(delta_FTSE,delta_EMF)
ze_FTSE_EMF <- 1- (var_z_FTSE_EMF/var(delta_FTSE))
ze_FTSE_EMF

e1_FTSE_FMEM <- egcm(delta_FMEM,delta_FTSE)
e2_FTSE_FMEM <- egcm(delta_FTSE,delta_FMEM)
i1_FTSE_FMEM <- e1_FTSE_FMEM$innovations
i2_FTSE_FMEM <- e2_FTSE_FMEM$innovations
exo_FTSE_FMEM <- abs(cov(i1_FTSE_FMEM,i2_FTSE_FMEM)/var(i2_FTSE_FMEM))
exo_FTSE_FMEM
var_z_FTSE_FMEM <- var(delta_FTSE) + exo_FTSE_FMEM^2 * var(delta_FMEM) - 2*exo_FTSE_FMEM*cov(delta_FTSE,delta_FMEM)
ze_FTSE_FMEM <- 1- (var_z_FTSE_FMEM/var(delta_FTSE))
ze_FTSE_FMEM


e1_FTSE_VWO <- egcm(delta_VWO,delta_FTSE)
e2_FTSE_VWO <- egcm(delta_FTSE,delta_VWO)
i1_FTSE_VWO <- e1_FTSE_VWO$innovations
i2_FTSE_VWO <- e2_FTSE_VWO$innovations
exo_FTSE_VWO <- abs(cov(i1_FTSE_VWO,i2_FTSE_VWO)/var(i2_FTSE_VWO))
exo_FTSE_VWO
var_z_FTSE_VWO <- var(delta_FTSE) + exo_FTSE_VWO^2 * var(delta_VWO) - 2*exo_FTSE_VWO*cov(delta_FTSE,delta_VWO)
ze_FTSE_VWO <- 1- (var_z_FTSE_VWO/var(delta_FTSE))
ze_FTSE_VWO




#Residual Risk
fit <- lm(delta_VWO[1:52]~delta_EMF[1:52])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_VWO[1:52]~delta_FMEM[1:52])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:52]~delta_EMF[1:52])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:52]~delta_FMEM[1:52])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:52]~delta_VWO[1:52])
RR <- 1- summary(fit)$r.squared
RR

#Non-rolling-10 days


#Non-rolling-10 days #Go to Price_10

dat <- read.csv("~/Desktop/Desktop/Eleni/DATA/Price_10.csv", stringsAsFactors = F)
attach(dat)

#Calculating Lagged Values (For Retuen Calculation)
#Calculating the second period values 
MESZ4_Trade_price_1 <- MESZ4_Trade_price[-1]
FEMZ4_trade_price_1 <- FEMZ4_trade_price[-1]
vwo_1 <- vwo[-1]
FTAG01_Price <- FTAG01_Price*100
FTAG01_Price_1 <- FTAG01_Price[-1]

# Calculating the returns (Bi-Weekly)
delta_VWO <- (vwo_1[1:25] - vwo[1:25])/vwo[1:25]
delta_FTSE <- (FTAG01_Price_1[1:25] - FTAG01_Price[1:25])/FTAG01_Price[1:25]
delta_EMF <- (FEMZ4_trade_price_1[1:25] - FEMZ4_trade_price[1:25])/FEMZ4_trade_price[1:25]
delta_FMEM <- (MESZ4_Trade_price_1[1:25] - MESZ4_Trade_price[1:25])/MESZ4_Trade_price[1:25]


#BID-ASK RETURN

#Long Spot # Short Futures

vwo_bid_price_1 <- vwo_bid_price[-1] 	
vwo_ask_price_1 <- vwo_ask_price[-1]

FEMZ4_bid_price_1 <- FEMZ4_bid_price[-1]
FEMZ4_ask_price_1 <- FEMZ4_ask_price[-1]

MESZ4_bid_price_1 <- MESZ4_bid_price[-1]
MESZ4_ask_price_1 <- MESZ4_ask_price[-1]


# Calculating the returns (Biweekly) Long _short
delta_VWO <- (vwo_bid_price_1[1:25] - vwo_ask_price[1:25])/vwo_ask_price[1:25]
#delta_FTSE <- (FTAG01_Price_1[1:265] - FTAG01_Price[1:265])/FTAG01_Price[1:265]
delta_EMF <- (FEMZ4_ask_price_1[1:25] - FEMZ4_bid_price[1:25])/FEMZ4_bid_price[1:25]
delta_FMEM <- (MESZ4_ask_price_1[1:25] - MESZ4_bid_price[1:25])/MESZ4_bid_price[1:25]

#Calculating the return (Biweekly Short_Long)
delta_VWO <- (vwo_ask_price_1[1:25] - vwo_bid_price[1:25])/vwo_bid_price[1:25]
#delta_FTSE <- (FTAG01_Price_1[1:265] - FTAG01_Price[1:265])/FTAG01_Price[1:265]
delta_EMF <- (FEMZ4_bid_price_1[1:25] - FEMZ4_ask_price[1:25])/FEMZ4_ask_price[1:25]
delta_FMEM <- (MESZ4_bid_price_1[1:25] - MESZ4_ask_price[1:25])/MESZ4_ask_price[1:25]




#Estimating Hedge Ratio
h_EMF_VWO <- cov(delta_VWO,delta_EMF)/var(delta_EMF)
h_EMF_VWO
h_FMEM_VWO <-cov(delta_VWO,delta_FMEM)/var(delta_FMEM)
h_FMEM_VWO

h_EMF_FTSE <- cov(delta_FTSE,delta_EMF)/var(delta_EMF)
h_EMF_FTSE

h_FMEM_FTSE <- cov(delta_FTSE,delta_FMEM)/var(delta_FMEM)
h_FMEM_FTSE
h_VWO_FTSE <- cov(delta_FTSE,delta_VWO)/var(delta_VWO)
h_VWO_FTSE


#Calculating Hedge Effectiveness using the previously calculated hedge ratio

var_delta_VWO <- var(delta_VWO)
var_h_EMF_VWO <- var(delta_VWO) + h_EMF_VWO^2 * var(delta_EMF) - 2*h_EMF_VWO*cov(delta_VWO,delta_EMF)
he_EMF_VWO <- 1- (var_h_EMF_VWO/var_delta_VWO)
he_EMF_VWO 

var_delta_VWO <- var(delta_VWO)
var_h_FMEM_VWO <- var(delta_VWO) + h_FMEM_VWO^2 * var(delta_FMEM) - 2*h_FMEM_VWO*cov(delta_VWO,delta_FMEM)
he_FMEM_VWO <- 1- (var_h_FMEM_VWO/var_delta_VWO)
he_FMEM_VWO

var_delta_FTSE <- var(delta_FTSE)
var_h_EMF_FTSE <- var(delta_FTSE) + h_EMF_FTSE^2 * var(delta_EMF) - 2*h_EMF_FTSE*cov(delta_FTSE,delta_EMF)
he_EMF_FTSE <- 1- (var_h_EMF_FTSE/var_delta_FTSE)
he_EMF_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_FMEM_FTSE <- var(delta_FTSE) + h_FMEM_FTSE^2 * var(delta_FMEM) - 2*h_FMEM_FTSE*cov(delta_FTSE,delta_FMEM)
he_FMEM_FTSE <- 1- (var_h_FMEM_FTSE/var_delta_FTSE)
he_FMEM_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_VWO_FTSE <- var(delta_FTSE) + h_VWO_FTSE^2 * var(delta_VWO) - 2*h_VWO_FTSE*cov(delta_FTSE,delta_VWO)
he_VWO_FTSE <- 1- (var_h_VWO_FTSE/var_delta_FTSE)
he_VWO_FTSE


#Mean of the hedged position

mean_EMF_VWO <- mean(delta_VWO) + h_EMF_VWO*mean(delta_EMF)
mean_EMF_VWO
mean_FMEM_VWO <- mean(delta_VWO) + h_FMEM_VWO*mean(delta_FMEM)
mean_FMEM_VWO
mean_EMF_FTSE <- mean(delta_FTSE) + h_EMF_FTSE*mean(delta_EMF)
mean_EMF_FTSE
mean_FMEM_FTSE <- mean(delta_FTSE) + h_FMEM_FTSE*mean(delta_FMEM)
mean_FMEM_FTSE
mean_VWO_FTSE <- mean(delta_FTSE) + h_VWO_FTSE*mean(delta_VWO)
mean_VWO_FTSE

#ttest of mean
t.test(x=delta_VWO + h_EMF_VWO*delta_EMF)
t.test(x=delta_VWO + h_EMF_VWO*delta_FMEM)
t.test(x=delta_FTSE + h_EMF_FTSE*delta_EMF)
t.test(x=delta_FTSE + h_FMEM_FTSE*delta_FMEM)
t.test(x=delta_FTSE + h_VWO_FTSE*delta_VWO)


#two sample t-test
t.test((x=delta_VWO + h_EMF_VWO*delta_EMF), (y=delta_VWO + h_EMF_VWO*delta_FMEM))
t.test((x=delta_FTSE + h_EMF_FTSE*delta_EMF), (y=delta_FTSE + h_FMEM_FTSE*delta_FMEM))


var.test((x=delta_VWO + h_EMF_VWO*delta_EMF), (y=delta_VWO + h_EMF_VWO*delta_FMEM))
var.test((x=delta_FTSE + h_EMF_FTSE*delta_EMF), (y=delta_FTSE + h_FMEM_FTSE*delta_FMEM))


#Variance of the hedged position

var_h_EMF_VWO
var_h_FMEM_VWO
var_h_EMF_FTSE
var_h_FMEM_FTSE
var_h_VWO_FTSE

#Regression of Hedged Position on Bid-ask Spread

bid_ask_EMF <- FEMZ4_ask_price - FEMZ4_bid_price
bid_ask_FMEM <- MESZ4_ask_price - MESZ4_bid_price


fit_EMF_VWO <- lm(delta_VWO + h_EMF_VWO*delta_EMF ~ bid_ask_EMF[2:26])
summary(fit_EMF_VWO)

fit_FMEM_VWO <- lm(delta_VWO + h_FMEM_VWO*delta_FMEM ~ bid_ask_FMEM[2:26])
summary(fit_FMEM_VWO)

fit_EMF_FTSE <- lm(delta_FTSE + h_EMF_FTSE*delta_EMF ~ bid_ask_EMF[2:26])
summary(fit_EMF_FTSE)

fit_FMEM_FTSE <- lm(delta_FTSE + h_FMEM_FTSE*delta_FMEM ~ bid_ask_FMEM[2:26])
summary(fit_FMEM_FTSE)

#NAIVE 1-1 hedging

var_delta_VWO <- var(delta_VWO)
var_h_EMF_VWO <- var(delta_VWO) + 1 * var(delta_EMF) - 2*1*cov(delta_VWO,delta_EMF)
he_EMF_VWO <- 1- (var_h_EMF_VWO/var_delta_VWO)
he_EMF_VWO 

var_delta_VWO <- var(delta_VWO)
var_h_FMEM_VWO <- var(delta_VWO) + 1 * var(delta_FMEM) - 2*1*cov(delta_VWO,delta_FMEM)
he_FMEM_VWO <- 1- (var_h_FMEM_VWO/var_delta_VWO)
he_FMEM_VWO

var_delta_FTSE <- var(delta_FTSE)
var_h_EMF_FTSE <- var(delta_FTSE) + 1 * var(delta_EMF) - 2*1*cov(delta_FTSE,delta_EMF)
he_EMF_FTSE <- 1- (var_h_EMF_FTSE/var_delta_FTSE)
he_EMF_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_FMEM_FTSE <- var(delta_FTSE) + 1 * var(delta_FMEM) - 2*1*cov(delta_FTSE,delta_FMEM)
he_FMEM_FTSE <- 1- (var_h_FMEM_FTSE/var_delta_FTSE)
he_FMEM_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_VWO_FTSE <- var(delta_FTSE) + 1 * var(delta_VWO) - 2*1*cov(delta_FTSE,delta_VWO)
he_VWO_FTSE <- 1- (var_h_VWO_FTSE/var_delta_FTSE)
he_VWO_FTSE

#NAIVE 1-1 hedging

var_delta_VWO <- var(delta_VWO)
var_h_EMF_VWO <- var(delta_VWO) + 1 * var(delta_EMF) - 2*1*cov(delta_VWO,delta_EMF)
he_EMF_VWO <- 1- (var_h_EMF_VWO/var_delta_VWO)
he_EMF_VWO 

var_delta_VWO <- var(delta_VWO)
var_h_FMEM_VWO <- var(delta_VWO) + 1 * var(delta_FMEM) - 2*1*cov(delta_VWO,delta_FMEM)
he_FMEM_VWO <- 1- (var_h_FMEM_VWO/var_delta_VWO)
he_FMEM_VWO

var_delta_FTSE <- var(delta_FTSE)
var_h_EMF_FTSE <- var(delta_FTSE) + 1 * var(delta_EMF) - 2*1*cov(delta_FTSE,delta_EMF)
he_EMF_FTSE <- 1- (var_h_EMF_FTSE/var_delta_FTSE)
he_EMF_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_FMEM_FTSE <- var(delta_FTSE) + 1 * var(delta_FMEM) - 2*1*cov(delta_FTSE,delta_FMEM)
he_FMEM_FTSE <- 1- (var_h_FMEM_FTSE/var_delta_FTSE)
he_FMEM_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_VWO_FTSE <- var(delta_FTSE) + 1 * var(delta_VWO) - 2*1*cov(delta_FTSE,delta_VWO)
he_VWO_FTSE <- 1- (var_h_VWO_FTSE/var_delta_FTSE)
he_VWO_FTSE


#Cointegration hedge ration=z Cointegration hedge effectiveness=ze
e1_VWO_EMF <- egcm(delta_EMF,delta_VWO)
e2_VWO_EMF <- egcm(delta_VWO,delta_EMF)
i1_VWO_EMF <- e1_VWO_EMF$innovations
i2_VWO_EMF <- e2_VWO_EMF$innovations
exo_VWO_EMF <- abs(cov(i1_VWO_EMF,i2_VWO_EMF)/var(i2_VWO_EMF))
exo_VWO_EMF
var_z_VWO_EMF <- var(delta_VWO) + exo_VWO_EMF^2 * var(delta_EMF) - 2*exo_VWO_EMF*cov(delta_VWO,delta_EMF)
ze_VWO_EMF <- 1- (var_z_VWO_EMF/var(delta_VWO))
ze_VWO_EMF

e1_VWO_FMEM <- egcm(delta_FMEM,delta_VWO)
e2_VWO_FMEM <- egcm(delta_VWO,delta_FMEM)
i1_VWO_FMEM <- e1_VWO_FMEM$innovations
i2_VWO_FMEM <- e2_VWO_FMEM$innovations
exo_VWO_FMEM <- abs(cov(i1_VWO_FMEM,i2_VWO_FMEM)/var(i2_VWO_FMEM))
exo_VWO_FMEM
var_z_VWO_FMEM <- var(delta_VWO) + exo_VWO_FMEM^2 * var(delta_FMEM) - 2*exo_VWO_FMEM*cov(delta_VWO,delta_FMEM)
ze_VWO_FMEM <- 1- (var_z_VWO_FMEM/var(delta_VWO))
ze_VWO_FMEM

e1_FTSE_EMF <- egcm(delta_EMF,delta_FTSE)
e2_FTSE_EMF <- egcm(delta_FTSE,delta_EMF)
i1_FTSE_EMF <- e1_FTSE_EMF$innovations
i2_FTSE_EMF <- e2_FTSE_EMF$innovations
exo_FTSE_EMF <- abs(cov(i1_FTSE_EMF,i2_FTSE_EMF)/var(i2_FTSE_EMF))
exo_FTSE_EMF
var_z_FTSE_EMF <- var(delta_FTSE) + exo_FTSE_EMF^2 * var(delta_EMF) - 2*exo_FTSE_EMF*cov(delta_FTSE,delta_EMF)
ze_FTSE_EMF <- 1- (var_z_FTSE_EMF/var(delta_FTSE))
ze_FTSE_EMF

e1_FTSE_FMEM <- egcm(delta_FMEM,delta_FTSE)
e2_FTSE_FMEM <- egcm(delta_FTSE,delta_FMEM)
i1_FTSE_FMEM <- e1_FTSE_FMEM$innovations
i2_FTSE_FMEM <- e2_FTSE_FMEM$innovations
exo_FTSE_FMEM <- abs(cov(i1_FTSE_FMEM,i2_FTSE_FMEM)/var(i2_FTSE_FMEM))
exo_FTSE_FMEM
var_z_FTSE_FMEM <- var(delta_FTSE) + exo_FTSE_FMEM^2 * var(delta_FMEM) - 2*exo_FTSE_FMEM*cov(delta_FTSE,delta_FMEM)
ze_FTSE_FMEM <- 1- (var_z_FTSE_FMEM/var(delta_FTSE))
ze_FTSE_FMEM


e1_FTSE_VWO <- egcm(delta_VWO,delta_FTSE)
e2_FTSE_VWO <- egcm(delta_FTSE,delta_VWO)
i1_FTSE_VWO <- e1_FTSE_VWO$innovations
i2_FTSE_VWO <- e2_FTSE_VWO$innovations
exo_FTSE_VWO <- abs(cov(i1_FTSE_VWO,i2_FTSE_VWO)/var(i2_FTSE_VWO))
exo_FTSE_VWO
var_z_FTSE_VWO <- var(delta_FTSE) + exo_FTSE_VWO^2 * var(delta_VWO) - 2*exo_FTSE_VWO*cov(delta_FTSE,delta_VWO)
ze_FTSE_VWO <- 1- (var_z_FTSE_VWO/var(delta_FTSE))
ze_FTSE_VWO



#Residual Risk
fit <- lm(delta_VWO[1:25]~delta_EMF[1:25])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_VWO[1:25]~delta_FMEM[1:25])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:25]~delta_EMF[1:25])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:25]~delta_FMEM[1:25])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:25]~delta_VWO[1:25])
RR <- 1- summary(fit)$r.squared
RR



#Rolling_5_days use "Price CSV"

dat <- read.csv("~/Desktop/Desktop/Eleni/DATA/Price_ADJUSTED.csv", stringsAsFactors = F)
attach(dat)

#Weekly
MESZ4_Trade_price_5 <- MESZ4_Trade_price[6:266]
FEMZ4_trade_price_5 <- FEMZ4_trade_price[6:266]
vwo_5 <- vwo[6:266]
FTAG01_Price_5 <- FTAG01_Price[6:266]



# Calculating the returns (Weekly)
delta_VWO <- (vwo_5[1:261] - vwo[1:261])/vwo[1:261]
delta_FTSE <- (FTAG01_Price_5[1:261] - FTAG01_Price[1:261])/FTAG01_Price[1:261]
delta_EMF <- (FEMZ4_trade_price_5[1:261] - FEMZ4_trade_price[1:261])/FEMZ4_trade_price[1:261]
delta_FMEM <- (MESZ4_Trade_price_5[1:261] - MESZ4_Trade_price[1:261])/MESZ4_Trade_price[1:261]


vwo_bid_price_5 <- vwo_bid_price[6:266] 	
vwo_ask_price_5 <- vwo_ask_price[6:266]

FEMZ4_bid_price_5 <- FEMZ4_bid_price[6:266]
FEMZ4_ask_price_5 <- FEMZ4_ask_price[6:266]

MESZ4_bid_price_5 <- MESZ4_bid_price[6:266]
MESZ4_ask_price_5 <- MESZ4_ask_price[6:266]


# Calculating the returns (Biweekly) Long _short
delta_VWO <- (vwo_bid_price_5[1:261] - vwo_ask_price[1:261])/vwo_ask_price[1:261]
#delta_FTSE <- (FTAG01_Price_1[1:265] - FTAG01_Price[1:265])/FTAG01_Price[1:265]
delta_EMF <- (FEMZ4_ask_price_5[1:261] - FEMZ4_bid_price[1:261])/FEMZ4_bid_price[1:261]
delta_FMEM <- (MESZ4_ask_price_5[1:261] - MESZ4_bid_price[1:261])/MESZ4_bid_price[1:261]

#Calculating the return (Biweekly Short_Long)
delta_VWO <- (vwo_ask_price_5[1:261] - vwo_bid_price[1:261])/vwo_bid_price[1:261]
#delta_FTSE <- (FTAG01_Price_1[1:265] - FTAG01_Price[1:265])/FTAG01_Price[1:265]
delta_EMF <- (FEMZ4_bid_price_5[1:261] - FEMZ4_ask_price[1:261])/FEMZ4_ask_price[1:261]
delta_FMEM <- (MESZ4_bid_price_5[1:261] - MESZ4_ask_price[1:261])/MESZ4_ask_price[1:261]




#Estimating Hedge Ratio
h_EMF_VWO <- cov(delta_VWO,delta_EMF)/var(delta_EMF)
h_EMF_VWO
h_FMEM_VWO <-cov(delta_VWO,delta_FMEM)/var(delta_FMEM)
h_FMEM_VWO

h_EMF_FTSE <- cov(delta_FTSE,delta_EMF)/var(delta_EMF)
h_EMF_FTSE

h_FMEM_FTSE <- cov(delta_FTSE,delta_FMEM)/var(delta_FMEM)
h_FMEM_FTSE
h_VWO_FTSE <- cov(delta_FTSE,delta_VWO)/var(delta_VWO)
h_VWO_FTSE


#Calculating Hedge Effectiveness using the previously calculated hedge ratio

var_delta_VWO <- var(delta_VWO)
var_h_EMF_VWO <- var(delta_VWO) + h_EMF_VWO^2 * var(delta_EMF) - 2*h_EMF_VWO*cov(delta_VWO,delta_EMF)
he_EMF_VWO <- 1- (var_h_EMF_VWO/var_delta_VWO)
he_EMF_VWO 

var_delta_VWO <- var(delta_VWO)
var_h_FMEM_VWO <- var(delta_VWO) + h_FMEM_VWO^2 * var(delta_FMEM) - 2*h_FMEM_VWO*cov(delta_VWO,delta_FMEM)
he_FMEM_VWO <- 1- (var_h_FMEM_VWO/var_delta_VWO)
he_FMEM_VWO

var_delta_FTSE <- var(delta_FTSE)
var_h_EMF_FTSE <- var(delta_FTSE) + h_EMF_FTSE^2 * var(delta_EMF) - 2*h_EMF_FTSE*cov(delta_FTSE,delta_EMF)
he_EMF_FTSE <- 1- (var_h_EMF_FTSE/var_delta_FTSE)
he_EMF_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_FMEM_FTSE <- var(delta_FTSE) + h_FMEM_FTSE^2 * var(delta_FMEM) - 2*h_FMEM_FTSE*cov(delta_FTSE,delta_FMEM)
he_FMEM_FTSE <- 1- (var_h_FMEM_FTSE/var_delta_FTSE)
he_FMEM_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_VWO_FTSE <- var(delta_FTSE) + h_VWO_FTSE^2 * var(delta_VWO) - 2*h_VWO_FTSE*cov(delta_FTSE,delta_VWO)
he_VWO_FTSE <- 1- (var_h_VWO_FTSE/var_delta_FTSE)
he_VWO_FTSE


#Mean of the hedged position

mean_EMF_VWO <- mean(delta_VWO) + h_EMF_VWO*mean(delta_EMF)
mean_EMF_VWO
mean_FMEM_VWO <- mean(delta_VWO) + h_FMEM_VWO*mean(delta_FMEM)
mean_FMEM_VWO
mean_EMF_FTSE <- mean(delta_FTSE) + h_EMF_FTSE*mean(delta_EMF)
mean_EMF_FTSE
mean_FMEM_FTSE <- mean(delta_FTSE) + h_FMEM_FTSE*mean(delta_FMEM)
mean_FMEM_FTSE
mean_VWO_FTSE <- mean(delta_FTSE) + h_VWO_FTSE*mean(delta_VWO)
mean_VWO_FTSE


#ttest of mean
t.test(x=delta_VWO + h_EMF_VWO*delta_EMF)
t.test(x=delta_VWO + h_EMF_VWO*delta_FMEM)
t.test(x=delta_FTSE + h_EMF_FTSE*delta_EMF)
t.test(x=delta_FTSE + h_FMEM_FTSE*delta_FMEM)
t.test(x=delta_FTSE + h_VWO_FTSE*delta_VWO)

#two sample t-test
t.test((x=delta_VWO + h_EMF_VWO*delta_EMF), (y=delta_VWO + h_EMF_VWO*delta_FMEM))
t.test((x=delta_FTSE + h_EMF_FTSE*delta_EMF), (y=delta_FTSE + h_FMEM_FTSE*delta_FMEM))

var.test((x=delta_VWO + h_EMF_VWO*delta_EMF), (y=delta_VWO + h_EMF_VWO*delta_FMEM))
var.test((x=delta_FTSE + h_EMF_FTSE*delta_EMF), (y=delta_FTSE + h_FMEM_FTSE*delta_FMEM))

#Variance of the hedged position

var_h_EMF_VWO
var_h_FMEM_VWO
var_h_EMF_FTSE
var_h_FMEM_FTSE
var_h_VWO_FTSE

#Regression of Hedged Position on Bid-ask Spread

bid_ask_EMF <- FEMZ4_ask_price - FEMZ4_bid_price
bid_ask_FMEM <- MESZ4_ask_price - MESZ4_bid_price


fit_EMF_VWO <- lm(delta_VWO + h_EMF_VWO*delta_EMF ~ bid_ask_EMF[6:266])
summary(fit_EMF_VWO)

fit_FMEM_VWO <- lm(delta_VWO + h_FMEM_VWO*delta_FMEM ~ bid_ask_FMEM[6:266])
summary(fit_FMEM_VWO)

fit_EMF_FTSE <- lm(delta_FTSE + h_EMF_FTSE*delta_EMF ~ bid_ask_EMF[6:266])
summary(fit_EMF_FTSE)

fit_FMEM_FTSE <- lm(delta_FTSE + h_FMEM_FTSE*delta_FMEM ~ bid_ask_FMEM[6:266])
summary(fit_FMEM_FTSE)



#NAIVE 1-1 hedging

var_delta_VWO <- var(delta_VWO)
var_h_EMF_VWO <- var(delta_VWO) + 1 * var(delta_EMF) - 2*1*cov(delta_VWO,delta_EMF)
he_EMF_VWO <- 1- (var_h_EMF_VWO/var_delta_VWO)
he_EMF_VWO 

var_delta_VWO <- var(delta_VWO)
var_h_FMEM_VWO <- var(delta_VWO) + 1 * var(delta_FMEM) - 2*1*cov(delta_VWO,delta_FMEM)
he_FMEM_VWO <- 1- (var_h_FMEM_VWO/var_delta_VWO)
he_FMEM_VWO

var_delta_FTSE <- var(delta_FTSE)
var_h_EMF_FTSE <- var(delta_FTSE) + 1 * var(delta_EMF) - 2*1*cov(delta_FTSE,delta_EMF)
he_EMF_FTSE <- 1- (var_h_EMF_FTSE/var_delta_FTSE)
he_EMF_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_FMEM_FTSE <- var(delta_FTSE) + 1 * var(delta_FMEM) - 2*1*cov(delta_FTSE,delta_FMEM)
he_FMEM_FTSE <- 1- (var_h_FMEM_FTSE/var_delta_FTSE)
he_FMEM_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_VWO_FTSE <- var(delta_FTSE) + 1 * var(delta_VWO) - 2*1*cov(delta_FTSE,delta_VWO)
he_VWO_FTSE <- 1- (var_h_VWO_FTSE/var_delta_FTSE)
he_VWO_FTSE


#Cointegration hedge ration=z Cointegration hedge effectiveness=ze
e1_VWO_EMF <- egcm(delta_EMF,delta_VWO)
e2_VWO_EMF <- egcm(delta_VWO,delta_EMF)
i1_VWO_EMF <- e1_VWO_EMF$innovations
i2_VWO_EMF <- e2_VWO_EMF$innovations
exo_VWO_EMF <- abs(cov(i1_VWO_EMF,i2_VWO_EMF)/var(i2_VWO_EMF))
exo_VWO_EMF
var_z_VWO_EMF <- var(delta_VWO) + exo_VWO_EMF^2 * var(delta_EMF) - 2*exo_VWO_EMF*cov(delta_VWO,delta_EMF)
ze_VWO_EMF <- 1- (var_z_VWO_EMF/var(delta_VWO))
ze_VWO_EMF

e1_VWO_FMEM <- egcm(delta_FMEM,delta_VWO)
e2_VWO_FMEM <- egcm(delta_VWO,delta_FMEM)
i1_VWO_FMEM <- e1_VWO_FMEM$innovations
i2_VWO_FMEM <- e2_VWO_FMEM$innovations
exo_VWO_FMEM <- abs(cov(i1_VWO_FMEM,i2_VWO_FMEM)/var(i2_VWO_FMEM))
exo_VWO_FMEM
var_z_VWO_FMEM <- var(delta_VWO) + exo_VWO_FMEM^2 * var(delta_FMEM) - 2*exo_VWO_FMEM*cov(delta_VWO,delta_FMEM)
ze_VWO_FMEM <- 1- (var_z_VWO_FMEM/var(delta_VWO))
ze_VWO_FMEM

e1_FTSE_EMF <- egcm(delta_EMF,delta_FTSE)
e2_FTSE_EMF <- egcm(delta_FTSE,delta_EMF)
i1_FTSE_EMF <- e1_FTSE_EMF$innovations
i2_FTSE_EMF <- e2_FTSE_EMF$innovations
exo_FTSE_EMF <- abs(cov(i1_FTSE_EMF,i2_FTSE_EMF)/var(i2_FTSE_EMF))
exo_FTSE_EMF
var_z_FTSE_EMF <- var(delta_FTSE) + exo_FTSE_EMF^2 * var(delta_EMF) - 2*exo_FTSE_EMF*cov(delta_FTSE,delta_EMF)
ze_FTSE_EMF <- 1- (var_z_FTSE_EMF/var(delta_FTSE))
ze_FTSE_EMF

e1_FTSE_FMEM <- egcm(delta_FMEM,delta_FTSE)
e2_FTSE_FMEM <- egcm(delta_FTSE,delta_FMEM)
i1_FTSE_FMEM <- e1_FTSE_FMEM$innovations
i2_FTSE_FMEM <- e2_FTSE_FMEM$innovations
exo_FTSE_FMEM <- abs(cov(i1_FTSE_FMEM,i2_FTSE_FMEM)/var(i2_FTSE_FMEM))
exo_FTSE_FMEM
var_z_FTSE_FMEM <- var(delta_FTSE) + exo_FTSE_FMEM^2 * var(delta_FMEM) - 2*exo_FTSE_FMEM*cov(delta_FTSE,delta_FMEM)
ze_FTSE_FMEM <- 1- (var_z_FTSE_FMEM/var(delta_FTSE))
ze_FTSE_FMEM


e1_FTSE_VWO <- egcm(delta_VWO,delta_FTSE)
e2_FTSE_VWO <- egcm(delta_FTSE,delta_VWO)
i1_FTSE_VWO <- e1_FTSE_VWO$innovations
i2_FTSE_VWO <- e2_FTSE_VWO$innovations
exo_FTSE_VWO <- abs(cov(i1_FTSE_VWO,i2_FTSE_VWO)/var(i2_FTSE_VWO))
exo_FTSE_VWO
var_z_FTSE_VWO <- var(delta_FTSE) + exo_FTSE_VWO^2 * var(delta_VWO) - 2*exo_FTSE_VWO*cov(delta_FTSE,delta_VWO)
ze_FTSE_VWO <- 1- (var_z_FTSE_VWO/var(delta_FTSE))
ze_FTSE_VWO

#Residual Risk
fit <- lm(delta_VWO[1:261]~delta_EMF[1:261])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_VWO[1:261]~delta_FMEM[1:261])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:261]~delta_EMF[1:261])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:261]~delta_FMEM[1:261])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:261]~delta_VWO[1:261])
RR <- 1- summary(fit)$r.squared
RR



#Bi-Weekly_Rolling Use Price.CSV

dat <- read.csv("~/Desktop/Desktop/Eleni/DATA/Price_ADJUSTED.csv", stringsAsFactors = F)
attach(dat)

MESZ4_Trade_price_10 <- MESZ4_Trade_price[11:266]
FEMZ4_trade_price_10 <- FEMZ4_trade_price[11:266]
vwo_10 <- vwo[11:266]
FTAG01_Price_10 <- FTAG01_Price[11:266]


# Calculating the returns (Bi_Weekly)
delta_VWO <- (vwo_10[1:256] - vwo[1:256])/vwo[1:256]
delta_FTSE <- (FTAG01_Price_10[1:256] - FTAG01_Price[1:256])/FTAG01_Price[1:256]
delta_EMF <- (FEMZ4_trade_price_10[1:256] - FEMZ4_trade_price[1:256])/FEMZ4_trade_price[1:256]
delta_FMEM <- (MESZ4_Trade_price_10[1:256] - MESZ4_Trade_price[1:256])/MESZ4_Trade_price[1:256]


vwo_bid_price_10 <- vwo_bid_price[11:266] 	
vwo_ask_price_10 <- vwo_ask_price[11:266]

FEMZ4_bid_price_10 <- FEMZ4_bid_price[11:266]
FEMZ4_ask_price_10 <- FEMZ4_ask_price[11:266]

MESZ4_bid_price_10 <- MESZ4_bid_price[11:266]
MESZ4_ask_price_10 <- MESZ4_ask_price[11:266]


# Calculating the returns (Biweekly) Long _short
delta_VWO <- (vwo_bid_price_10[1:256] - vwo_ask_price[1:256])/vwo_ask_price[1:256]
#delta_FTSE <- (FTAG01_Price_1[1:265] - FTAG01_Price[1:265])/FTAG01_Price[1:265]
delta_EMF <- (FEMZ4_ask_price_10[1:256] - FEMZ4_bid_price[1:256])/FEMZ4_bid_price[1:256]
delta_FMEM <- (MESZ4_ask_price_10[1:256] - MESZ4_bid_price[1:256])/MESZ4_bid_price[1:256]

#Calculating the return (Biweekly Short_Long)
delta_VWO <- (vwo_ask_price_10[1:256] - vwo_bid_price[1:256])/vwo_bid_price[1:256]
#delta_FTSE <- (FTAG01_Price_1[1:265] - FTAG01_Price[1:265])/FTAG01_Price[1:265]
delta_EMF <- (FEMZ4_bid_price_10[1:256] - FEMZ4_ask_price[1:256])/FEMZ4_ask_price[1:256]
delta_FMEM <- (MESZ4_bid_price_10[1:256] - MESZ4_ask_price[1:256])/MESZ4_ask_price[1:256]






#Estimating Hedge Ratio
h_EMF_VWO <- cov(delta_VWO,delta_EMF)/var(delta_EMF)
h_EMF_VWO
h_FMEM_VWO <-cov(delta_VWO,delta_FMEM)/var(delta_FMEM)
h_FMEM_VWO

h_EMF_FTSE <- cov(delta_FTSE,delta_EMF)/var(delta_EMF)
h_EMF_FTSE

h_FMEM_FTSE <- cov(delta_FTSE,delta_FMEM)/var(delta_FMEM)
h_FMEM_FTSE
h_VWO_FTSE <- cov(delta_FTSE,delta_VWO)/var(delta_VWO)
h_VWO_FTSE


#Calculating Hedge Effectiveness using the previously calculated hedge ratio

var_delta_VWO <- var(delta_VWO)
var_h_EMF_VWO <- var(delta_VWO) + h_EMF_VWO^2 * var(delta_EMF) - 2*h_EMF_VWO*cov(delta_VWO,delta_EMF)
he_EMF_VWO <- 1- (var_h_EMF_VWO/var_delta_VWO)
he_EMF_VWO 

var_delta_VWO <- var(delta_VWO)
var_h_FMEM_VWO <- var(delta_VWO) + h_FMEM_VWO^2 * var(delta_FMEM) - 2*h_FMEM_VWO*cov(delta_VWO,delta_FMEM)
he_FMEM_VWO <- 1- (var_h_FMEM_VWO/var_delta_VWO)
he_FMEM_VWO

var_delta_FTSE <- var(delta_FTSE)
var_h_EMF_FTSE <- var(delta_FTSE) + h_EMF_FTSE^2 * var(delta_EMF) - 2*h_EMF_FTSE*cov(delta_FTSE,delta_EMF)
he_EMF_FTSE <- 1- (var_h_EMF_FTSE/var_delta_FTSE)
he_EMF_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_FMEM_FTSE <- var(delta_FTSE) + h_FMEM_FTSE^2 * var(delta_FMEM) - 2*h_FMEM_FTSE*cov(delta_FTSE,delta_FMEM)
he_FMEM_FTSE <- 1- (var_h_FMEM_FTSE/var_delta_FTSE)
he_FMEM_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_VWO_FTSE <- var(delta_FTSE) + h_VWO_FTSE^2 * var(delta_VWO) - 2*h_VWO_FTSE*cov(delta_FTSE,delta_VWO)
he_VWO_FTSE <- 1- (var_h_VWO_FTSE/var_delta_FTSE)
he_VWO_FTSE

#Mean of the hedged position

mean_EMF_VWO <- mean(delta_VWO) + h_EMF_VWO*mean(delta_EMF)
mean_EMF_VWO
mean_FMEM_VWO <- mean(delta_VWO) + h_FMEM_VWO*mean(delta_FMEM)
mean_FMEM_VWO
mean_EMF_FTSE <- mean(delta_FTSE) + h_EMF_FTSE*mean(delta_EMF)
mean_EMF_FTSE
mean_FMEM_FTSE <- mean(delta_FTSE) + h_FMEM_FTSE*mean(delta_FMEM)
mean_FMEM_FTSE
mean_VWO_FTSE <- mean(delta_FTSE) + h_VWO_FTSE*mean(delta_VWO)
mean_VWO_FTSE



#mean ttest
t.test(x=delta_VWO + h_EMF_VWO*delta_EMF)
t.test(x=delta_VWO + h_EMF_VWO*delta_FMEM)
t.test(x=delta_FTSE + h_EMF_FTSE*delta_EMF)
t.test(x=delta_FTSE + h_FMEM_FTSE*delta_FMEM)
t.test(x=delta_FTSE + h_VWO_FTSE*delta_VWO)

#two sample t-test
t.test((x=delta_VWO + h_EMF_VWO*delta_EMF), (y=delta_VWO + h_EMF_VWO*delta_FMEM))
t.test((x=delta_FTSE + h_EMF_FTSE*delta_EMF), (y=delta_FTSE + h_FMEM_FTSE*delta_FMEM))

var.test((x=delta_VWO + h_EMF_VWO*delta_EMF), (y=delta_VWO + h_EMF_VWO*delta_FMEM))
var.test((x=delta_FTSE + h_EMF_FTSE*delta_EMF), (y=delta_FTSE + h_FMEM_FTSE*delta_FMEM))
#Variance of the hedged position

var_h_EMF_VWO
var_h_FMEM_VWO
var_h_EMF_FTSE
var_h_FMEM_FTSE
var_h_VWO_FTSE



#Regression of Hedged Position on Bid-ask Spread

bid_ask_EMF <- FEMZ4_ask_price - FEMZ4_bid_price
bid_ask_FMEM <- MESZ4_ask_price - MESZ4_bid_price


fit_EMF_VWO <- lm(delta_VWO + h_EMF_VWO*delta_EMF ~ bid_ask_EMF[11:266])
summary(fit_EMF_VWO)

fit_FMEM_VWO <- lm(delta_VWO + h_FMEM_VWO*delta_FMEM ~ bid_ask_FMEM[11:266])
summary(fit_FMEM_VWO)

fit_EMF_FTSE <- lm(delta_FTSE + h_EMF_FTSE*delta_EMF ~ bid_ask_EMF[11:266])
summary(fit_EMF_FTSE)

fit_FMEM_FTSE <- lm(delta_FTSE + h_FMEM_FTSE*delta_FMEM ~ bid_ask_FMEM[11:266])
summary(fit_FMEM_FTSE)


#NAIVE 1-1 hedging

var_delta_VWO <- var(delta_VWO)
var_h_EMF_VWO <- var(delta_VWO) + 1 * var(delta_EMF) - 2*1*cov(delta_VWO,delta_EMF)
he_EMF_VWO <- 1- (var_h_EMF_VWO/var_delta_VWO)
he_EMF_VWO 

var_delta_VWO <- var(delta_VWO)
var_h_FMEM_VWO <- var(delta_VWO) + 1 * var(delta_FMEM) - 2*1*cov(delta_VWO,delta_FMEM)
he_FMEM_VWO <- 1- (var_h_FMEM_VWO/var_delta_VWO)
he_FMEM_VWO

var_delta_FTSE <- var(delta_FTSE)
var_h_EMF_FTSE <- var(delta_FTSE) + 1 * var(delta_EMF) - 2*1*cov(delta_FTSE,delta_EMF)
he_EMF_FTSE <- 1- (var_h_EMF_FTSE/var_delta_FTSE)
he_EMF_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_FMEM_FTSE <- var(delta_FTSE) + 1 * var(delta_FMEM) - 2*1*cov(delta_FTSE,delta_FMEM)
he_FMEM_FTSE <- 1- (var_h_FMEM_FTSE/var_delta_FTSE)
he_FMEM_FTSE

var_delta_FTSE <- var(delta_FTSE)
var_h_VWO_FTSE <- var(delta_FTSE) + 1 * var(delta_VWO) - 2*1*cov(delta_FTSE,delta_VWO)
he_VWO_FTSE <- 1- (var_h_VWO_FTSE/var_delta_FTSE)
he_VWO_FTSE



#Cointegration hedge ration=z Cointegration hedge effectiveness=ze
e1_VWO_EMF <- egcm(delta_EMF,delta_VWO)
e2_VWO_EMF <- egcm(delta_VWO,delta_EMF)
i1_VWO_EMF <- e1_VWO_EMF$innovations
i2_VWO_EMF <- e2_VWO_EMF$innovations
exo_VWO_EMF <- abs(cov(i1_VWO_EMF,i2_VWO_EMF)/var(i2_VWO_EMF))
exo_VWO_EMF
var_z_VWO_EMF <- var(delta_VWO) + exo_VWO_EMF^2 * var(delta_EMF) - 2*exo_VWO_EMF*cov(delta_VWO,delta_EMF)
ze_VWO_EMF <- 1- (var_z_VWO_EMF/var(delta_VWO))
ze_VWO_EMF

e1_VWO_FMEM <- egcm(delta_FMEM,delta_VWO)
e2_VWO_FMEM <- egcm(delta_VWO,delta_FMEM)
i1_VWO_FMEM <- e1_VWO_FMEM$innovations
i2_VWO_FMEM <- e2_VWO_FMEM$innovations
exo_VWO_FMEM <- abs(cov(i1_VWO_FMEM,i2_VWO_FMEM)/var(i2_VWO_FMEM))
exo_VWO_FMEM
var_z_VWO_FMEM <- var(delta_VWO) + exo_VWO_FMEM^2 * var(delta_FMEM) - 2*exo_VWO_FMEM*cov(delta_VWO,delta_FMEM)
ze_VWO_FMEM <- 1- (var_z_VWO_FMEM/var(delta_VWO))
ze_VWO_FMEM

e1_FTSE_EMF <- egcm(delta_EMF,delta_FTSE)
e2_FTSE_EMF <- egcm(delta_FTSE,delta_EMF)
i1_FTSE_EMF <- e1_FTSE_EMF$innovations
i2_FTSE_EMF <- e2_FTSE_EMF$innovations
exo_FTSE_EMF <- abs(cov(i1_FTSE_EMF,i2_FTSE_EMF)/var(i2_FTSE_EMF))
exo_FTSE_EMF
var_z_FTSE_EMF <- var(delta_FTSE) + exo_FTSE_EMF^2 * var(delta_EMF) - 2*exo_FTSE_EMF*cov(delta_FTSE,delta_EMF)
ze_FTSE_EMF <- 1- (var_z_FTSE_EMF/var(delta_FTSE))
ze_FTSE_EMF

e1_FTSE_FMEM <- egcm(delta_FMEM,delta_FTSE)
e2_FTSE_FMEM <- egcm(delta_FTSE,delta_FMEM)
i1_FTSE_FMEM <- e1_FTSE_FMEM$innovations
i2_FTSE_FMEM <- e2_FTSE_FMEM$innovations
exo_FTSE_FMEM <- abs(cov(i1_FTSE_FMEM,i2_FTSE_FMEM)/var(i2_FTSE_FMEM))
exo_FTSE_FMEM
var_z_FTSE_FMEM <- var(delta_FTSE) + exo_FTSE_FMEM^2 * var(delta_FMEM) - 2*exo_FTSE_FMEM*cov(delta_FTSE,delta_FMEM)
ze_FTSE_FMEM <- 1- (var_z_FTSE_FMEM/var(delta_FTSE))
ze_FTSE_FMEM


e1_FTSE_VWO <- egcm(delta_VWO,delta_FTSE)
e2_FTSE_VWO <- egcm(delta_FTSE,delta_VWO)
i1_FTSE_VWO <- e1_FTSE_VWO$innovations
i2_FTSE_VWO <- e2_FTSE_VWO$innovations
exo_FTSE_VWO <- abs(cov(i1_FTSE_VWO,i2_FTSE_VWO)/var(i2_FTSE_VWO))
exo_FTSE_VWO
var_z_FTSE_VWO <- var(delta_FTSE) + exo_FTSE_VWO^2 * var(delta_VWO) - 2*exo_FTSE_VWO*cov(delta_FTSE,delta_VWO)
ze_FTSE_VWO <- 1- (var_z_FTSE_VWO/var(delta_FTSE))
ze_FTSE_VWO

#Residual Risk
fit <- lm(delta_VWO[1:256]~delta_EMF[1:256])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_VWO[1:256]~delta_FMEM[1:256])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:256]~delta_EMF[1:256])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:256]~delta_FMEM[1:256])
RR <- 1- summary(fit)$r.squared
RR

fit <- lm(delta_FTSE[1:256]~delta_VWO[1:256])
RR <- 1- summary(fit)$r.squared
RR






#Plotting


index <- 1:255
index_label <- rep("",each=255)
index_label[1] <- "6/14"
index_label[12] <- "7/14"
index_label[34] <- "8/14"
index_label[55] <- "9/14"
index_label[76] <- "10/14"
index_label[99] <- "11/14"
index_label[118] <- "12/14"
index_label[140] <- "1/15"
index_label[160] <- "2/15"
index_label[179] <- "3/15"
index_label[201] <- "4/15"
index_label[222] <- "5/15"
index_label[242] <- "6/15"

dat <- cbind(index,dat)





q3 <- ggplot(data=dat) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
q3 <- q3 + geom_line(aes(x=index ,y= (vwo_bid_ask), color= "#CC0000"), size=0.6)
q3 <- q3 + geom_line(aes(x=index,y= (FEMZ4_bid_ask), color= "mediumblue"), size=0.6)
q3 <- q3 + geom_line(aes(x=index,y= (MESZ4_bid_ask), color= "magenta4"), size=0.6)

q3 <- q3 + scale_x_discrete("Time",limits=index_label)
q3 <- q3 + scale_y_continuous("Bid Ask Spread")
q3 <- q3+ scale_color_discrete("Contracts",labels=c("VWO","EMF","FMEM"))
#q3 <- q3 + scale_colour_discrete(breaks=c("NG1","NG3", "NG6","NG9"))
#q3 <- q3+ scale_color_discrete("Nearby Volatility",labels="Volatility")
plot(q3)


q3 <- ggplot(data=dat) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
q3 <- q3 + geom_line(aes(x=index ,y= (FEMZ4_OPEN), color= "#CC0000"), size=0.6)
q3 <- q3 + geom_line(aes(x=index,y= (MESZ4_OPEN), color= "mediumblue"), size=0.6)
#q3 <- q3 + geom_line(aes(x=index,y= (MESZ4_bid_ask), color= "magenta4"), size=0.6)

q3 <- q3 + scale_x_discrete("Time",limits=index_label)
q3 <- q3 + scale_y_continuous("Open Interest")
q3 <- q3+ scale_color_discrete("Contracts",labels=c("FMEM"))
#q3 <- q3 + scale_colour_discrete(breaks=c("NG1","NG3", "NG6","NG9"))
#q3 <- q3+ scale_color_discrete("Nearby Volatility",labels="Volatility")
plot(q3)
