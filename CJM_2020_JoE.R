######################################################################
# Local Regression Distribution Estimators -- Replication
# M.D. Cattaneo, M. Jansson, X. Ma
# 24-SEP-2020
######################################################################

rm(list=ls())

# install.packages("ggplot2")
# install.packages("lpdensity")

library(ggplot2)
library(lpdensity) 

jtpa <- read.csv("jtpa.csv", header=TRUE) # load data

set.seed(42) # seed for simulating critical values to construct confidence bands

# NOTE: if you are using RDDENSITY version 2020 or newer, the option 
# masspoints=FALSE may be needed to replicate the results in the monograph.
# For example:
#    out = rddensity(X)
# should be replaced by:
#    out = rddensity(X, masspoints=FALSE)

#################################################################################
# Summary Statistics
#################################################################################

X <- jtpa[, c("income", "hsorged", "male", "nonwhite", "married", "wkless13", "afdc", 
              "age2225", "age2629", "age3035", "age3644", "age4554")]
X <- cbind(X, 1)
SumTable <- matrix(NA, ncol=5, nrow=ncol(X))
for (j in 1:ncol(X)) {
  SumTable[j, 1] <- mean(X[, j])
  SumTable[j, 2] <- mean(X[jtpa$instrument==0, j])
  SumTable[j, 3] <- mean(X[jtpa$instrument==1, j])
  SumTable[j, 4] <- mean(X[jtpa$treatment==0, j])
  SumTable[j, 5] <- mean(X[jtpa$treatment==1, j])
  if (j == ncol(X)) {
    SumTable[j, 1] <- sum(X[, j])
    SumTable[j, 2] <- sum(X[jtpa$instrument==0, j])
    SumTable[j, 3] <- sum(X[jtpa$instrument==1, j])
    SumTable[j, 4] <- sum(X[jtpa$treatment==0, j])
    SumTable[j, 5] <- sum(X[jtpa$treatment==1, j])
  }
}

round(SumTable, 2)

#################################################################################
# Figure 2
#################################################################################

#--------------------------------------------------------------------------------
# Panel (a)
#--------------------------------------------------------------------------------

jtpa_notreat <- jtpa[jtpa$treatment==0, ]
grid <- seq(2, 5, length.out=100)

# Full sample
est_edu_all <- lpdensity(data=jtpa_notreat$logincome, bwselect="imse-dpi", grid=grid)

# High school or GED
est_edu_1   <- lpdensity(data=jtpa_notreat$logincome, bwselect="imse-dpi", grid=grid, Cweights=jtpa_notreat$hsorged)

# No high school or GED
est_edu_0   <- lpdensity(data=jtpa_notreat$logincome, bwselect="imse-dpi", grid=grid, Cweights=(jtpa_notreat$hsorged == 0)*1)

fig_2a <- lpdensity.plot(est_edu_1, est_edu_0, est_edu_all, 
                         legendTitle="", legendGroups=c("1 HS or GED", "2 No HS or GED", "3 Full"), 
                         lty=c(1, 2, 4), lcol=c(2, 4, 6), 
                         CIuniform=TRUE) + 
  theme(legend.position=c(0.13, 0.95), legend.background = element_blank()) + 
  scale_y_continuous("", breaks=seq(0, 1.25, 0.25), labels=seq(0, 1.25, 0.25), limits=c(-0.05, 1.25)) +
  scale_x_continuous("", breaks=c(2, 3, 4, 5), labels=c(expression(10^2), expression(10^3), expression(10^4), expression(10^5)), limits=c(2, 5))

fig_2a

#--------------------------------------------------------------------------------
# Panel (b)
#--------------------------------------------------------------------------------

pscore <- lm(hsorged ~ male + nonwhite + wkless13 + married +
               I(male * nonwhite) + I(male * wkless13) + I(nonwhite * wkless13) + 
               I(afdc * male) + age2225 + age2629 + age3035 + age3644 + age4554,
             data=jtpa_notreat)$fitted.values
Cweights <- (1 - pscore) / pscore * sum(jtpa_notreat$hsorged==1) / sum(jtpa_notreat$hsorged==0)
Cweights <- Cweights * jtpa_notreat$hsorged

# Counterfactual
est_edu_0c   <- lpdensity(data=jtpa_notreat$logincome, bwselect="imse-dpi", grid=grid, Cweights=Cweights)

fig_2b <- lpdensity.plot(est_edu_1, est_edu_0, est_edu_0c,
                         legendTitle="", legendGroups=c("1 HS or GED", "2 No HS or GED", "3 No HS or GED, Counterfactual"), 
                         lty=c(1, 2, 3), lcol=c(2, 4, 1),
                         CIuniform=TRUE) + 
  theme(legend.position=c(0.2, 0.95), legend.background = element_blank()) + 
  scale_y_continuous("", breaks=seq(0, 1.25, 0.25), labels=seq(0, 1.25, 0.25), limits=c(-0.05, 1.25)) +
  scale_x_continuous("", breaks=c(2, 3, 4, 5), labels=c(expression(10^2), expression(10^3), expression(10^4), expression(10^5)), limits=c(2, 5))

fig_2b

#################################################################################
# Figure 4
#################################################################################

#--------------------------------------------------------------------------------
# Panel (a)
#--------------------------------------------------------------------------------

grid <- seq(2, 5, length.out=100)

# Full sample 
est_all   <- lpdensity(data=jtpa$logincome, bwselect="imse-dpi", grid=grid)

# Compliers
pscore <- lm(instrument ~ male + hsorged + nonwhite + wkless13 + married +
               I(hsorged * male) + I(hsorged * nonwhite) + I(hsorged * wkless13) + 
               I(male * nonwhite) + I(male * wkless13) + I(nonwhite * wkless13), data=jtpa)$fitted.values

est_compliers   <- lpdensity(data=jtpa$logincome, bwselect="imse-dpi", grid=grid, Cweights=
                               1 - jtpa$instrument * (1 - jtpa$treatment) / pscore - (1 - jtpa$instrument) * jtpa$treatment / (1 - pscore))

fig_4a <- lpdensity.plot(est_all, est_compliers,
                         legendTitle="", legendGroups=c("1 JTPA: Full sample", "2 JTPA: Compliers"), 
                         lty=c(1, 2, 3), lcol=c(1, 6),
                         CIuniform=TRUE) + 
  theme(legend.position=c(0.15, 0.95), legend.background = element_blank()) + 
  scale_y_continuous("", breaks=seq(0, 1.25, 0.25), labels=seq(0, 1.25, 0.25), limits=c(-0.05, 1.25)) +
  scale_x_continuous("", breaks=c(2, 3, 4, 5), labels=c(expression(10^2), expression(10^3), expression(10^4), expression(10^5)), limits=c(2, 5))

fig_4a

#--------------------------------------------------------------------------------
# Panel (b)
#--------------------------------------------------------------------------------

# Offered
est_z_1   <- lpdensity(data=jtpa$logincome, bwselect="imse-dpi", grid=grid, Cweights=(jtpa$instrument == 1)*1)

# Not offered
est_z_0   <- lpdensity(data=jtpa$logincome, bwselect="imse-dpi", grid=grid, Cweights=(jtpa$instrument == 0)*1)

fig_4b <- lpdensity.plot(est_z_0, est_z_1,
                         legendTitle="", legendGroups=c("1 JTPA: Not Offered", "2 JTPA: Offered"), 
                         lty=c(1, 2, 3), lcol=c(2, 4, 1),
                         CIuniform=TRUE) + 
  theme(legend.position=c(0.15, 0.95), legend.background = element_blank()) + 
  scale_y_continuous("", breaks=seq(0, 1.25, 0.25), labels=seq(0, 1.25, 0.25), limits=c(-0.05, 1.25)) +
  scale_x_continuous("", breaks=c(2, 3, 4, 5), labels=c(expression(10^2), expression(10^3), expression(10^4), expression(10^5)), limits=c(2, 5))

fig_4b

#--------------------------------------------------------------------------------
# Panel (c)
#--------------------------------------------------------------------------------

# Enrolled
est_d_1   <- lpdensity(data=jtpa$logincome, bwselect="imse-dpi", grid=grid, Cweights=(jtpa$treatment == 1)*1)

# Not enrolled
est_d_0   <- lpdensity(data=jtpa$logincome, bwselect="imse-dpi", grid=grid, Cweights=(jtpa$treatment == 0)*1)

fig_4c <- lpdensity.plot(est_d_0, est_d_1,
                         legendTitle="", legendGroups=c("1 JTPA: Not Enrolled", "2 JTPA: Enrolled"), 
                         lty=c(1, 2, 3), lcol=c(2, 4, 1),
                         CIuniform=TRUE) + 
  theme(legend.position=c(0.15, 0.95), legend.background = element_blank()) + 
  scale_y_continuous("", breaks=seq(0, 1.25, 0.25), labels=seq(0, 1.25, 0.25), limits=c(-0.05, 1.25)) +
  scale_x_continuous("", breaks=c(2, 3, 4, 5), labels=c(expression(10^2), expression(10^3), expression(10^4), expression(10^5)), limits=c(2, 5))

fig_4c

#--------------------------------------------------------------------------------
# Panel (d)
#--------------------------------------------------------------------------------

# Compliers, potential outcome x(1)
est_compliers1   <- lpdensity(data=jtpa$logincome, bwselect="imse-dpi", grid=grid, Cweights=
                                (jtpa$treatment) * (jtpa$instrument - (pscore)) / (pscore * (1 - pscore)))

# Compliers, potential outcome x(0)
est_compliers0   <- lpdensity(data=jtpa$logincome, bwselect="imse-dpi", grid=grid, Cweights=
                                (1 - jtpa$treatment) * (1 - jtpa$instrument - (1 - pscore)) / (pscore * (1 - pscore)))

fig_4d <- lpdensity.plot(est_compliers0, est_compliers1,
                         legendTitle="", legendGroups=c("1 Potential Outcome: x(0)", "2 Potential Outcome: x(1)"), 
                         lty=c(1, 2, 3), lcol=c(2, 4, 1),
                         CIuniform=TRUE) + 
  theme(legend.position=c(0.175, 0.95), legend.background = element_blank()) + 
  scale_y_continuous("", breaks=seq(0, 1.25, 0.25), labels=seq(0, 1.25, 0.25), limits=c(-0.05, 1.25)) +
  scale_x_continuous("", breaks=c(2, 3, 4, 5), labels=c(expression(10^2), expression(10^3), expression(10^4), expression(10^5)), limits=c(2, 5))

fig_4d

######################################################################
# Figure 3
######################################################################

# Estimate for the subsample z=0, d=0, with scaling
est_z_0_d_0   <- lpdensity(data=jtpa$logincome, bwselect="imse-dpi", grid=grid, 
                       Cweights=(jtpa$treatment == 0 & jtpa$instrument == 0)*1, 
                       scale= sum(jtpa$treatment == 0 & jtpa$instrument == 0) / sum(jtpa$instrument == 0))

# Estimate for the subsample z=1, d=0, with scaling
est_z_1_d_0   <- lpdensity(data=jtpa$logincome, bwselect="imse-dpi", grid=grid, 
                           Cweights=(jtpa$treatment == 0 & jtpa$instrument == 1)*1, 
                           scale= sum(jtpa$treatment == 0 & jtpa$instrument == 1) / sum(jtpa$instrument == 1))

fig_3 <- lpdensity.plot(est_z_0_d_0, est_z_1_d_0, 
                        legendTitle="", legendGroups=c("1 JTPA: Not Offered & Not Enrolled", "2 JTPA: Offered & Not Enrolled"), 
                        lty=c(1, 2), lcol=c(2, 4),
                        CIuniform=TRUE) + 
  theme(legend.position=c(0.24, 0.95), legend.background = element_blank()) + 
  scale_y_continuous("", breaks=seq(0, 1.25, 0.25), labels=seq(0, 1.25, 0.25), limits=c(-0.05, 1.25)) +
  scale_x_continuous("", breaks=c(2, 3, 4, 5), labels=c(expression(10^2), expression(10^3), expression(10^4), expression(10^5)), limits=c(2, 5))

fig_3
