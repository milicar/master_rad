library(dplyr)
library(tidyr)
library(stringi)



wie_tweets <- readRDS("results/selected_tweets.RData")

########### prvo treba uzeti samo jedinstvene tvitove, ali zbog varijanti istog
#tvita sa razlicitim metapodacima o tome kliko je puta retvitovan ili
#favoritovan, nije bas jednostavno kao distinct(df). Prvo treba da se
#grupise, uzme veci kaunt za ta dva podatka, izbaci originalni broj rt/fav (sa
#razlicitim vrednostima) i onda radi distinct..

wie_really_unique <- unique(wie_tweets) %>% group_by(id) %>% 
        mutate(retweetCount = max(retweetCount), favoriteCount = max(favoriteCount)) %>% 
        distinct(id, .keep_all = TRUE) %>% ungroup()

# slicni koraci su odradjeni i u pripremi za analizu teksta, ali su ovde podaci koji nisu 
# obradjeni, a osim toga, isto sredjivanje podataka ovde treba da se odradi na jos dve varijable.
wie_really_unique$text <- stri_trans_tolower(stri_enc_toascii(wie_really_unique$text))
wie_really_unique$screenName <- stri_trans_tolower(stri_enc_toascii(wie_really_unique$screenName))
wie_really_unique$replyToSN <- stri_trans_tolower(stri_enc_toascii(wie_really_unique$replyToSN))

#### FUNKCIJE ZA PRAVLJENJE EDGELISTE

# parsiranje teksta tvitova ---------------------------------------------------
# ova verzija bi trebalo da zanemari primaoce/mentione koji nisu posmatrani ucesnici
# ovo treba da se uradi pre kodiranja "none" primaoca
parse_text_for_receivers <- function(tweet_txt, senders, tweet_id) {
        tw_handle_pattern <- "@[_A-Za-z1-9]+"
        tweets_regmatches <- regmatches(tweet_txt, gregexpr(pattern = tw_handle_pattern, tweet_txt))
        senders <- unique(senders)
        mentions <- lapply(tweets_regmatches, function(x, s){
                substr(x[substr(x, 2, 16) %in% s], 2, 16)
        }, senders)
        names(mentions) <- tweet_id
        mentions
}

# pronalazenje ucesnika na cije je statuse odgovoreno  repliyToSID --------------------------
# ako je na status odgovoreno, nadje autora tog tvita i stavlja u rezultat..
# poziva se iz sledece funkcije, kao pomocna
# u obzir dolaze samo posmatrani ucesnici! 
replied_to_status <- function(tweets_df){
        sapply(tweets_df$replyToSID, function(x, y){
                ifelse(!is.na(x), y[y$id == x,]$screenName, NA)
        }, tweets_df, USE.NAMES = FALSE)
}

# pronalazenje ucesnika kojima je odgovoreno  ----------------------------------
# na screenName ili status replyToSN i replyToSID <- za ovo drugo se poziva  prethodna funkcija
# spaja sve učesnike koji su primaoci poruka iz metapodataka
replied_to_receivers <- function(tweets_df){
        replied_to_valid_names <- ifelse(is.na(tweets_df$replyToSN), 
                                         NA, 
                                         ifelse(tweets_df$replyToSN %in% tweets_df$screenName, 
                                                tweets_df$replyToSN, NA))
        mapply(function(x, y){ 
                if(is.na(x) & is.na(y)) list(NA) #ako nema ni jedan ni drugi reply, onda NA
                else(if(!is.na(x) & !is.na(y)) list(unique(c(x,y)))
                     else(ifelse (is.na(x), list(y), list(x)))) # ako su oba !na, onda je u prethodnom koraku odradio
        }, replied_to_valid_names, replied_to_status(tweets_df), 
        SIMPLIFY = TRUE, USE.NAMES = FALSE)
}

# spaja sve primaoce, sto iz teksta, sto iz drugih varijabli -----------------------
# ovde se tvitovi bez primaoca kodiraju kao da imaju "none" primaoca
# onda tvit nije upucen nikome, rec="none"
# ostavljaju se samo unique primaoci (na primer, ako je primalac pronadjen i 
# u tekstu i kao replyTo ucesnik
add_all_receivers <- function(mentions, replied_to){
        all_receivers <- mapply(function(x,y){
                if ((length(x) == 0) & is.na(y)) "none"
                else if(is.na(y)) x 
                else if(length(x) == 0) y
                else append(y, x)}, mentions, replied_to,
                SIMPLIFY = TRUE, USE.NAMES = TRUE)
        lapply(all_receivers, unique)
}
# dodavanje kolone o svim primaocima jednog tvita
tws2edgelist <- function(tweets_df, list_receivers){
        
        edgelist <- tweets_df %>% arrange(id)
        list_receivers <- list_receivers[order(names(list_receivers))] 
        # ovo je po IDju, jer su elementi liste nazvani po idju.. 
        edgelist$receivers <- list_receivers
        
        #razdvajanje svih kombinacija posiljalac-primalac za svaki tvit
        edgelist <- edgelist %>% unnest(receivers) %>% ungroup() %>% 
                arrange(created, id) %>% 
                select(sender = screenName, receiver = receivers, id)
}


remove_nondyads_loops <- function(edgelist){
        edgelist <- edgelist %>% filter(receiver != "none" & sender != receiver)
}

# pre izbacivanja self-loops i non-dyads ima 457 posiljalaca, 
# ali kad se izbaci, ima ih 441 i 182 primalaca, ukupno 447 razlicitih

# od svih dogadjaja, na osnovu svih tvitova, prvo se izbace oni koji nisu o skupu,
# pa na taj nacin ostanu samo ucesnici koji su komunicirali o skupu
# zatim se izabere n ucesnika koji su najcesce bili ucesnici komunikacije
# na kraju se za njih iz pocetne istorije dogadjaja izaberu samo oni u kojima
# su oba ucesnika među izabranim ucesnicima
take_n_conf_participants <- function(edgelist, n){
        conf_regex <- "(?i)wielead|wieilc|[#|@]wie|women.+engineer|wie.+ieee|ieee.+wie"
        el_conf <- edgelist %>% left_join(wie_tweets[,c("id", "text")]) %>% 
                filter(str_detect(text, pattern = conf_regex)) %>% select(sender, receiver)
        participants <- tibble(participant = c(el_conf$sender, el_conf$receiver)) %>%
                group_by(participant) %>% count() %>% ungroup() %>% arrange(desc(n)) %>% 
                mutate(total = sum(n)) %>% .[1:n,] %>% 
                mutate(frac = sum(n) / total, key = row_number())
        
        edgelist <- edgelist %>% filter(sender %in% participants$participant &
                                                receiver %in% participants$participant) %>% 
                left_join(participants[,c("participant", "key")], 
                          by = c("sender" = "participant")) %>% rename(sender_key = key) %>% 
                left_join(participants[,c("participant", "key")], 
                          by = c("receiver" = "participant")) %>% rename(receiver_key = key) %>%
                mutate(event_no = row_number()) %>%
                select(event_no ,sender = sender_key, receiver = receiver_key, id)
        res <- list(edgelist = edgelist, key = participants)
}


################# PRAVLJENJE EDGELISTE

text_receivers <- parse_text_for_receivers(wie_really_unique$text, 
                                           unique(wie_really_unique$screenName),
                                           wie_really_unique$id)

replied_to_recs <- replied_to_receivers(wie_really_unique)

list_all_receivers <- add_all_receivers(text_receivers, replied_to_recs)

edgelist <- tws2edgelist(wie_really_unique, list_all_receivers)

edgelist <- remove_nondyads_loops(edgelist)

edgelist_200 <- take_n_conf_participants(edgelist, 200)

# edgelist ima listu dogadjaja i kljuc za dekodiranje broja ucesnika u korisnicko ime
edgelist_200$edgelist 
edgelist_200$key

######################################## DODAVANJE PODATAKA; PRAVLJENJE EDGELISTE PO PERIODIMA

# podatak o periodu po id-ju tvita
wie_timeframe <- tibble(id = wie_tweets$id, timeframe = wie_tweets$timeframe)
saveRDS(wie_timeframe, "results/wie_tmeframe_key.RDS")

edgelist_200_df <- left_join(edgelist_200$edgelist, wie_timeframe)
edgelist_200_by_timeframe <- split(edgelist_200_df, edgelist_200_df$timeframe)

lapply(edgelist_200_by_timeframe, nrow)

# broj dogadjaja po periodima:
# $time_1
# [1] 631
# 
# $time_2
# [1] 644
# 
# $time_3
# [1] 2121
# 
# $time_4
# [1] 477
# 
# $time_5
# [1] 391

saveRDS(edgelist_200, "results/edgelist_200_list.RDS")
saveRDS(edgelist_200_by_timeframe, "results/edgelist_200_by_timeframe.RDS")

##################################################### PRAVLJENJE VARIJABLI

edgelist_200$key <- edgelist_200$key %>% mutate(organizer = FALSE)
edgelist_200$key[edgelist_200$key$participant == "wieilc", "organizer"] <- TRUE

is_organizer_cov <- tibble(covar = edgelist_200$key$organizer)
saveRDS(is_organizer_cov, "results/is_organizer_cov.RData")

library(stringr)
conf_regex <- "(?i)wielead|wieilc|[#|@]wie|women.+engineer|wie.+ieee|ieee.+wie"
wie_tweets <- wie_tweets %>% mutate(conf = ifelse(str_detect(text, pattern = conf_regex), TRUE, FALSE))

is_conf <- tibble(id = wie_tweets$id, conf = wie_tweets$conf)
saveRDS(is_conf, "results/is_conf.RData")


# sentiment je vec izracunat, samo da ga spojim.. tvitovi koji nisu sa konferencije imaju
# NA sentiment
conf_tweets_sentiment <- readRDS("results/conf_tweets_sentiments_updated.RData")

wie_sentiment <- wie_tweets[,c("id", "conf")]
wie_sentiment <- wie_sentiment %>% left_join(conf_tweets_sentiment[,c("id", "sentiment_mean")])
head(wie_sentiment)
saveRDS(wie_sentiment, "results/wie_sentiment_double")

# tvit ce se smatrati pozitivnim ako prelazi 0.1 vrednost za sentiment
conf_tweets_sentiment <- conf_tweets_sentiment %>% mutate(positive = ifelse(sentiment_mean >= 0.1, 1, 0))
wie_sentiment <- wie_sentiment %>% left_join(conf_tweets_sentiment[,c("id", "positive")])
head(wie_sentiment)
saveRDS(wie_sentiment, "results/wie_sentiment_positive_updated.RDS")

# covar conf
# covar ce da bude oblika m x p x n x n == nrow() x 1 x nrow(participants) x nrow(partic)

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


has_topics <- readRDS("results/has_topics.RData")

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
