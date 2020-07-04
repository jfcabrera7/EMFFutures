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
print(df["Dates"].dtypes)

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

# %% Calculates the hedge ratio
def hedge_ratio(xvar, yvar):
    return xvar.cov(yvar) / xvar.var()

hr_emf_vwo = hedge_ratio(dfret["emf_ret"], dfret["vwo_ret"])
hr_emf_ftse = hedge_ratio(dfret["emf_ret"], dfret["ftse_ret"])
hr_fmem_vwo = hedge_ratio(dfret["fmem_ret"], dfret["vwo_ret"])
hr_fmem_ftse = hedge_ratio(dfret["fmem_ret"], dfret["ftse_ret"])
hr_vwo_ftse = hedge_ratio(dfret["vwo_ret"], dfret["ftse_ret"])

# %% Calculates the hedging effectiveness
