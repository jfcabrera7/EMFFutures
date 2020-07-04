# %% Imports packages
import pandas as pd
import numpy as np
import statsmodels as sm
import matplotlib.pyplot as plt

# %% Imports .csv dataset as a dataframe
df = pd.read_csv("../data/MainData01_CSV.csv")
df.head()

# %% Formats dates and converts non-numeric to NaN
df['Dates'] = pd.to_datetime(df['Dates'], format="%m/%d/%Y")
# print(df["Dates"].dtypes)

df = pd.concat([df["Dates"], df.iloc[:, 1:].apply(
    pd.to_numeric, errors='coerce')], axis=1)

# %% Calculates returns and subsets data
period = 1 # 1 for daily

df["emf_ret"] = (df["EMFAdjPrice"] / df["EMFAdjPrice"].shift(period)) - 1
df["fmem_ret"] = (df["FMEMAdjPrice"] / df["FMEMAdjPrice"].shift(period)) - 1
df["vwo_ret"] = (df["VWOAdjPrice"] / df["VWOAdjPrice"].shift(period)) - 1
df["ftse_ret"] = (df["FTSEPrice"] / df["FTSEPrice"].shift(period)) - 1
# print(df.iloc[:10, -4:])

keepvars = ["emf_ret", "fmem_ret", "vwo_ret", "ftse_ret"]
dfret = df[keepvars].dropna()
dfret.reset_index(inplace = True, drop = True)
dfret.head()

# %% Calculates the hedge ratio (Table 3)
def hedge_ratio(xvar, yvar):
    return xvar.cov(yvar) / xvar.var()

hr = {}
hr["emf_vwo"] = hedge_ratio(dfret["emf_ret"], dfret["vwo_ret"])
hr["emf_ftse"] = hedge_ratio(dfret["emf_ret"], dfret["ftse_ret"])
hr["fmem_vwo"] = hedge_ratio(dfret["fmem_ret"], dfret["vwo_ret"])
hr["fmem_ftse"] = hedge_ratio(dfret["fmem_ret"], dfret["ftse_ret"])
hr["vwo_ftse"] = hedge_ratio(dfret["vwo_ret"], dfret["ftse_ret"])
# %% Calculates the hedging effectiveness (Table 12 - OLS)
def hedge_effec(xvar, yvar, hr):
    return 1 - (yvar.var() + hr**2 * xvar.var() - 2*hr * xvar.cov(yvar)) / yvar.var()

he = {}
he["emf_vwo"] = hedge_effec(dfret["emf_ret"], dfret["vwo_ret"], hr["emf_vwo"])
he["emf_ftse"] = hedge_effec(dfret["emf_ret"], dfret["ftse_ret"], hr["emf_ftse"])
he["fmem_vwo"] = hedge_effec(dfret["fmem_ret"], dfret["vwo_ret"], hr["fmem_vwo"])
he["fmem_ftse"] = hedge_effec(dfret["fmem_ret"], dfret["ftse_ret"], hr["fmem_ftse"])
he["vwo_ftse"] = hedge_effec(dfret["vwo_ret"], dfret["ftse_ret"], hr["vwo_ftse"])

# %% Calculates the mean of hedhed position (Table 4)
def mean_hedged(fut, spot, hr):
    return (spot + hr * fut).mean()

mh = {}
mh["emf_vwo"] = mean_hedged(dfret["emf_ret"], dfret["vwo_ret"], hr["emf_vwo"])
mh["emf_ftse"] = mean_hedged(dfret["emf_ret"], dfret["ftse_ret"], hr["emf_ftse"])
mh["fmem_vwo"] = mean_hedged(dfret["fmem_ret"], dfret["vwo_ret"], hr["fmem_vwo"])
mh["fmem_ftse"] = mean_hedged(dfret["fmem_ret"], dfret["ftse_ret"], hr["fmem_ftse"])
mh["vwo_ftse"] = mean_hedged(dfret["vwo_ret"], dfret["ftse_ret"], hr["vwo_ftse"])

# %% Testing code
# dfret.describe()
# dfret.iloc[:,0]
# list(dfret.columns)
# len(dfret.columns)
# dfret.cov()
# dfret["vwo_ret"].mean()
