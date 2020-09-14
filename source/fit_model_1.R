# Fitovanje REM modela 1

library(dplyr)
library(relevent)
library(abind)


wie_edgelist_200 <- readRDS("results/edgelist_200_by_timeframe.RDS")

# U prvom modelu se ispituju samo strukturalni efekti, a binarna varijabla jeste/nije organizator
# se kontroliše, jer je u analizi mreže otkriveno da ima najveći stepen 
is_organizer_200 <- readRDS("results/is_organizer_cov.RData")

# fitovani model
fit_structural <- readRDS("results/fit_structural.RDS")

# pozivanje funkcije za modelovanje. Uključeni efekti su 
# * popularnost (normalizovani dolazni stepen učesnika v utiče na njegovu stopu primanja događaja)
# * skorašnjost (vremenska blizina primanja događaja od strane v utiče na stopu slanja poruka ka v)
# * inercija (vremenska blizina slanja poruke učesniku v utiče na stopu ponovnog slanja poruke ka v)
# * efekti trijada (zajednički sagovornik utiče na stopu slanja događaja između dva  sagovornika )
# * OTPSnd: A->B, B->C utiče na A->C
# * ITPSnd: C->B, B->A učiče na A->C
# * OSPSnd: A->B, C->B utiče na A->C
# * ISPSnd: B->A, B->C utiče na A->C
# 
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