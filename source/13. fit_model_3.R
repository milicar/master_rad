# Fitovanje REM modela 3

library(dplyr)
library(relevent)
library(abind)


wie_edgelist_200 <- readRDS("results/edgelist_200_by_timeframe.RDS")

# Ovaj model uključuje sve efekte kao i prvi model, uz dve binarne varijable:
# da li događaj/poruka govori o skupu i da li je događaj pozitivan i govori o skupu
# funkcije koje prave varijable su date ispod

is_organizer_200 <- readRDS("results/is_organizer_cov.RData")
has_topics <- readRDS("results/has_topics.RData")
wie_sentiment_positive <- readRDS("results/wie_sentiment_positive_updated.RDS")


########## binarna varijabla koja govori o tome da li dogadjaj govori o temama skupa
make_single_topics_covar <- function(edgelist, n_participants, has_topics){
        ar <- array(0, dim = c(nrow(edgelist), 1, n_participants, n_participants))
        event_offset <- edgelist[[1,1]]-1
        indices <- edgelist %>% left_join(has_topics) %>% filter(has_bigram == TRUE)
        for(i in 1:nrow(indices)){
                ar[indices[[i, "event_no"]]-event_offset, 1, indices[[i, "sender"]], indices[[i, "receiver"]]]<-1
        }
        ar
}


########## binarna varijabla koja govori o tome da li dogadjaj pozitivno govori o temama skupa
make_single_topics_positive_covar <- function(edgelist, n_participants, has_topics, conf_sentiment){
        ar <- array(0, dim = c(nrow(edgelist), 1, n_participants, n_participants))
        event_offset <- edgelist[[1,1]]-1
        indices <- edgelist %>% left_join(has_topics) %>% 
                left_join(conf_sentiment[,c("id", "positive")]) %>% 
                filter(has_bigram == TRUE & positive == 1)
        for(i in 1:nrow(indices)){
                ar[indices[[i, "event_no"]]-event_offset, 1, indices[[i, "sender"]], indices[[i, "receiver"]]]<-1
        }
        ar
}

# fitovani model
fit_struct_topics <- readRDS("results/fit_struct_topics.RDS")

# funkcija koja fituje model. Izvršavanje funkcije je trajalo oko 6.24 časova, sa do 10Gb RAM memorije
# fit_struct_topics <- list()
# fit_struct_topics <- lapply(wie_edgelist_200,
#                   function(x)(rem.dyad(x[,1:3], 200,
#                                        effects = c("NIDRec",
#                                                    "RRecSnd", "RSndSnd",
#                                                    "OTPSnd", "ITPSnd", "OSPSnd", "ISPSnd",
#                                                    "CovInt", "CovEvent"),
#                                        covar = list(CovInt = is_organizer_200$covar,
#                                                     CovEvent = abind(make_single_topics_covar(x, 200, has_topics),
#                                                                      make_single_topics_positive_covar(x, 200, has_topics, wie_sentiment_positive),
#                                                                      along = 2)),
#                                        hessian = TRUE)))
# 
# saveRDS(fit_struct_topics, "fit_struct_topics.RDS")

summary(fit_struct_topics$time_1)
summary(fit_struct_topics$time_2)
summary(fit_struct_topics$time_3)
summary(fit_struct_topics$time_4)
summary(fit_struct_topics$time_5)
