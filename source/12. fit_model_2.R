# Fitovanje REM modela 2

library(dplyr)
library(relevent)
library(abind)


wie_edgelist_200 <- readRDS("results/edgelist_200_by_timeframe.RDS")

# Drugi model pored svih efekata uključenih u prvi model, uključuje dve dodatne binarne varijable:
# da li događaj govori o posmatranom skupu i da li događaj pozitivno govori o posmatranom skupu
# funkcije koje prave array su ispod.

is_organizer_200 <- readRDS("results/is_organizer_cov.RData")
is_conf <- readRDS("results/is_conf.RData")
wie_sentiment_positive <- readRDS("results/wie_sentiment_positive_updated.RDS")


########## binarna varijabla koja govori o tome da li dogadjaj govori o skupu
make_singleconf_covar <- function(edgelist, n_participants, is_conf){
        ar <- array(0, dim = c(nrow(edgelist), 1, n_participants, n_participants))
        event_offset <- edgelist[[1,1]]-1
        indices <- edgelist %>% left_join(is_conf) %>% filter(conf == TRUE)
        for(i in 1:nrow(indices)){
                ar[indices[[i, "event_no"]]-event_offset, 1, indices[[i, "sender"]], indices[[i, "receiver"]]]<-1
        }
        ar
}

########## binarna varijabla koja govori o tome da li dogadjaj pozitivno govori o skupu
make_single_confpositive_covar <- function(edgelist, n_participants, is_conf, conf_sentiment){
        ar <- array(0, dim = c(nrow(edgelist), 1, n_participants, n_participants))
        event_offset <- edgelist[[1,1]]-1
        indices <- edgelist %>% left_join(is_conf) %>% left_join(conf_sentiment[,c("id", "positive")]) %>% 
                filter(conf == TRUE & positive == 1)
        for(i in 1:nrow(indices)){
                ar[indices[[i, "event_no"]]-event_offset, 1, indices[[i, "sender"]], indices[[i, "receiver"]]]<-1
        }
        ar
}

# fitovani model
fit_struct_conf <- readRDS("results/fit_struct_conf.RDS")

# funkcija koja fituje model. Izvršavanje funkcije je trajalo 7.26 časova, sa do 10Gb zauzete RAM memorije
# fit_struct_conf <- list()
# fit_struct_conf <- lapply(wie_edgelist_200,
#                             function(x)(rem.dyad(x[,1:3], 200,
#                                                  effects = c("NIDRec",
#                                                              "RRecSnd", "RSndSnd",
#                                                              "OTPSnd", "ITPSnd", "OSPSnd", "ISPSnd",
#                                                              "CovInt", "CovEvent"),
#                                                  covar = list(CovInt = is_organizer_200$covar,
#                                                               CovEvent = abind(make_singleconf_covar(x, 200, is_conf),
#                                                                                make_single_confpositive_covar(x, 200, is_conf, wie_sentiment_positive),
#                                                                                along = 2)),
#                                                  hessian = TRUE)))
# 
# # saveRDS(fit_struct_conf, "fit_struct_conf.RDS")


summary(fit_struct_conf$time_1)
summary(fit_struct_conf$time_2)
summary(fit_struct_conf$time_3)
summary(fit_struct_conf$time_4)
summary(fit_struct_conf$time_5)
