# Fitovanje REM modela 1

library(dplyr)
library(relevent)
library(abind)


wie_edgelist_200 <- readRDS("results/edgelist_200_by_timeframe.RDS")
is_organizer_200 <- readRDS("results/is_organizer_cov.RData")

fit_structural <- readRDS("results/fit_structural.RDS")


# fit_structural <- list()
# fit_structural <- lapply(wie_edgelist_200,
#                             function(x)(rem.dyad(x[,1:3], 200,
#                                                  effects = c("NIDRec",
#                                                              "RRecSnd", "RSndSnd",
#                                                              "OTPSnd", "ITPSnd", "OSPSnd", "ISPSnd"),
#                                                  covar = list(CovInt = is_organizer_200$covar),
#                                                  hessian = TRUE)))
# 
# saveRDS(fit_structural, "fit_structural.RDS")



summary(fit_structural$time_1)
summary(fit_structural$time_2)
summary(fit_structural$time_3)
summary(fit_structural$time_4)
summary(fit_structural$time_5)