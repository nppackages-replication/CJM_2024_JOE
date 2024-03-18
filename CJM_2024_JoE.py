######################################################################
# Local Regression Distribution Estimators -- Replication
# M.D. Cattaneo, M. Jansson, X. Ma, R. Chandak
# Oct 01, 2021
######################################################################

from lpdensity import lpdensity
from lpdensity import lpbwdensity
import math
import pandas as pd
import numpy as np
import random
import statistics as stat
import plotnine as plt

random.seed(42)

jtpa = pd.read_csv("jtpa.csv")

#################################################################################
# Summary Statistics
#################################################################################

X = jtpa[['income', 'hsorged', 'male', 'nonwhite', 'married', 'wkless13', 'afdc', 'age2225', 'age2629', 'age3035', 'age3644', 'age4554']]

n = len(jtpa['income'])
ones_df = pd.DataFrame(np.repeat(1,n))
X = pd.concat([X, ones_df], axis=1)

sumTable = np.zeros((len(X.columns), 5))
for j in range(len(X.columns)):
	sumTable[j, 0] = stat.mean(X.iloc[:, j])
#	idx = jtpa.loc[jtpa['instrument'] == 0].index
	sumTable[j, 1] = stat.mean(X.iloc[jtpa.loc[jtpa['instrument'] == 0].index, j])
	sumTable[j, 2] = stat.mean(X.iloc[jtpa.loc[jtpa['instrument'] == 1].index, j])
	sumTable[j, 3] = stat.mean(X.iloc[jtpa.loc[jtpa['treatment'] == 0].index, j])
	sumTable[j, 4] = stat.mean(X.iloc[jtpa.loc[jtpa['treatment'] == 1].index, j])
	if j==max(range(len(X.columns))):
		sumTable[j, 0] = sum(X.iloc[:, j])
		sumTable[j, 1] = sum(X.iloc[jtpa.loc[jtpa['instrument'] == 0].index, j])
		sumTable[j, 2] = sum(X.iloc[jtpa.loc[jtpa['instrument'] == 1].index, j])
		sumTable[j, 3] = sum(X.iloc[jtpa.loc[jtpa['treatment'] == 0].index, j])
		sumTable[j, 4] = sum(X.iloc[jtpa.loc[jtpa['treatment'] == 1].index, j])

np.around(sumTable, decimals=2, out=sumTable)


jtpa_notreat = jtpa.loc[jtpa['treatment'] == 0]
grid = np.linspace(2, 5, 10)

# Full sample
est_edu_all = lpdensity(data=jtpa_notreat['logincome'], bwselect='imse-dpi', grid=grid)
#est_edu_all.plot(CIuniform=True)
print(est_edu_all)
confint_all = est_edu_all.confint(CIuniform=True)

# High school or GED
est_edu_1 = lpdensity(data=jtpa_notreat['logincome'], bwselect="imse-dpi", grid=grid, Cweights=jtpa_notreat['hsorged'])
print(est_edu_1)
confint_1 = est_edu_1.confint(CIuniform=True)

# No high school or GED
est_edu_0 = lpdensity(data=jtpa_notreat['logincome'], bwselect="imse-dpi", grid=grid, Cweights=(jtpa_notreat['hsorged'] == 0)*1)
print(est_edu_0)
confint_0 = est_edu_0.confint(CIuniform=True)

# collecting all the data
data_all = est_edu_all.Estimate[['grid', 'f_p', 'f_q', 'se_p', 'se_q']]
data_all['CI_l'] = confint_all['CI_l_q']
data_all['CI_r'] = confint_all['CI_r_q']

data_1 = est_edu_1.Estimate[['grid', 'f_p', 'f_q', 'se_p', 'se_q']]
data_1['CI_l'] = confint_1['CI_l_q']
data_1['CI_r'] = confint_1['CI_r_q']

data_0 = est_edu_0.Estimate[['grid', 'f_p', 'f_q', 'se_p', 'se_q']]
data_0['CI_l'] = confint_0['CI_l_q']
data_0['CI_r'] = confint_0['CI_r_q']

#plotting all three estimates on a single graph
plot = plt.ggplot() + plt.theme_bw()
#CI region
plot = plot + plt.geom_ribbon(data=data_all, mapping=plt.aes(x='grid', ymin='CI_l', ymax='CI_r'), alpha=.2) + plt.geom_ribbon(data=data_1, mapping=plt.aes(x='grid', ymin='CI_l', ymax='CI_r'), alpha=.4) + plt.geom_ribbon(data=data_0, mapping=plt.aes(x='grid', ymin='CI_l', ymax='CI_r'), alpha=.6)

#Lines
plot = plot + plt.geom_line(data=data_all, mapping=plt.aes(x='grid', y='f_p')) + plt.geom_line(data=data_1, mapping=plt.aes(x='grid', y='f_p')) + plt.geom_line(data=data_0, mapping=plt.aes(x='grid', y='f_p'))

#show plot
plot
