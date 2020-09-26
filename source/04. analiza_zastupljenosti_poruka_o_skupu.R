# Analiza zastupljenosti tvitova i retvitova o skupu

library(dplyr)
library(ggplot2)

tweets <- readRDS("results/tweets.RData")
tweets %>% nrow()

# procenat tvitova o skupu
tweets %>% filter(conf == TRUE) %>% 
        summarise(conf_tweets = nrow(.), 
                  conf_tweets_percent = nrow(.)*100/nrow(tweets))

# procenat retvitova o skupu
tweets %>% filter(conf == TRUE & retweet == TRUE) %>% 
        summarise(conf_retweets = nrow(.),
                  conf_retweets_percent = nrow(.)*100 /nrow(tweets %>% filter(conf == TRUE)))

### Ukupno gledano, procenat tvitova o skupu nije veliki, ali i posmatrani
### period je veliki.

# dodavanje atributa date
tweets <- tweets %>% left_join(readRDS("results/selected_tweets.RData") %>% select(id, date))

# izdvojeno u danima skupa, udeo tvitova o skupu u ukupnim tvitovima
conf_tweets_percent <- tweets %>%
        mutate(timeframe = ifelse((date == "2017-05-22" | date == "2017-05-23"), "confdays", timeframe)) %>%
        group_by(timeframe) %>% mutate(total_tweets = n()) %>% ungroup() %>%
        filter(conf == TRUE) %>%
        group_by(timeframe) %>% mutate(conf_tweets = n()) %>%
        summarise(conf_tweets = first(conf_tweets), 
                  total_tweets = first(total_tweets), 
                  conf_tweets_percent = conf_tweets * 100 / total_tweets)

### U danima skupa, procenat tvitova o skupu je 11.2%, sto ne govori o velikoj
### posvecenosti skupu. 

# procenat retvitova samo za tvitove sa skupa
conf_retweets_percent <- tweets %>%
        filter(conf == TRUE) %>%
        mutate(timeframe = ifelse((date == "2017-05-22" | date == "2017-05-23"), "confdays", timeframe)) %>%
        group_by(timeframe) %>% mutate(total_conf_tweets = n()) %>% ungroup() %>%
        filter(retweet == TRUE) %>%
        group_by(timeframe) %>% mutate(conf_retweets = n()) %>%
        summarise(conf_retweets = first(conf_retweets), 
                  total_conf_tweets = first(total_conf_tweets), 
                  conf_retweets_percent = conf_retweets * 100 / total_conf_tweets)

### Nesto ispod polovine tvitova o skupu su retvitovi. Retvitovi su znacajni
### zato sto imaju moc da prosire poruku do sire publike. Sa druge strane, moglo
### bi se reci da zahtevaju manju angazovanost ucesnika skupa u smislu kreiranja
### sadrzaja. Ipak, i jedno i drugo je vazno, a buduci da je vise od polovine
### tvitova originalnih, bilo je dovoljno raznovrsnih poruka koje su mogle da
### budu retvitovane, odnosno prosirene do sire publike.

# Da li su ova dva dana bila jednako interesantna?

conf_tweets_by_days_percent <-  tweets %>%
        filter(date == "2017-05-22" | date == "2017-05-23") %>%
        mutate(timeframe = ifelse(date == "2017-05-22", "day1", "day2")) %>%
        group_by(timeframe) %>% mutate(total_tweets = n()) %>% ungroup() %>%
        filter(conf == TRUE) %>%
        group_by(timeframe) %>% mutate(conf_tweets = n()) %>%
        summarise(conf_tweets = first(conf_tweets), 
                  total_tweets = first(total_tweets), 
                  tweets_percent = conf_tweets * 100 / total_tweets)


conf_retweets_by_day_percent <- tweets %>%
        filter(date == "2017-05-22" | date == "2017-05-23") %>%
        filter(conf == TRUE) %>%
        mutate(timeframe = ifelse(date == "2017-05-22", "day1", "day2")) %>%
        group_by(timeframe) %>% mutate(total_conf_tweets = n()) %>% ungroup() %>%
        filter(retweet == TRUE) %>%
        group_by(timeframe) %>% mutate(conf_retweets = n()) %>%
        summarise(conf_retweets = first(conf_retweets), 
                  total_conf_tweets = first(total_conf_tweets), 
                  conf_retweets_percent = conf_retweets * 100 / total_conf_tweets)

### Drugog dana skupa, procenat tvitova o skupu izgleda nesto veci, ali se radi 
### o tome da je ukupan broj tvitova nesto manji, a konacno, razlika nije
### velika. Procenat retvitova je gotovo identican za ova dva dana.

# broj ucesnika sa tvitovima o skupu, po periodima
participants_by_timeframe <- tweets %>% 
        filter(conf == TRUE) %>%
        group_by(timeframe) %>% 
        distinct(screenName) %>%
        summarise(total_participants = n())

# isto to, kada se izdvoje dani skupa
participants_by_timeframe2 <- tweets %>% 
        filter(conf == TRUE) %>%
        mutate(timeframe = ifelse((date == "2017-05-22" | date == "2017-05-23"), "confdays", timeframe)) %>%
        group_by(timeframe) %>% 
        distinct(screenName) %>%
        summarise(total_participants = n())       

# broj ucesnika po danima
participants_by_days <- tweets %>% 
        filter(conf == TRUE & (date == "2017-05-22" | date == "2017-05-23")) %>%
        mutate(timeframe = ifelse(date == "2017-05-22", "day1", "day2")) %>%
        group_by(timeframe) %>% 
        distinct(screenName) %>%
        summarise(total_participants = n()) 

### Samo u danima skupa, o skupu je tvitovalo 343 ucesnika. Po danima ima
### podjednak broj ucesnika, ali se oni ne preklapaju u potpunosti, tj. nisu
### isti ucesnici tvitovali u oba dana skupa.

# ucesnici koji imaju tvitove o skupu
tweets %>% filter((date == "2017-05-22" | date == "2017-05-23") & conf == TRUE) %>% 
        distinct(screenName) %>% nrow()



# angazovanost po ucesniku, tvitovi 
tweets_by_participant <- tweets %>%
        filter(date == "2017-05-22" | date == "2017-05-23") %>%
        group_by(screenName) %>% mutate(total_tweets = n()) %>% ungroup() %>%
        filter(conf == TRUE) %>%
        group_by(screenName) %>% mutate(conf_tweets = n()) %>%
        summarise(conf_tweets = first(conf_tweets), 
                  total_tweets = first(total_tweets), 
                  conf_tweets_percent = conf_tweets * 100 / total_tweets) %>%
        arrange(desc(conf_tweets_percent))



png("visuals/2_01_aktivnost_ucesnika_tvitovi_o_skupu.png")
ggplot(tweets_by_participant, aes(conf_tweets_percent)) +
        geom_histogram(binwidth = 10, col =  "gray40", fill = "#A3A500")+
        labs(title = "Aktivnost učesnika:\nprocenat tvitova o skupu", 
             x = "tvitovi o skupu od ukupnog broja tvitova",
             y = "broj učesnika") +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
              axis.text=element_text(size=12, face="bold"),
              axis.title=element_text(size=16,face="bold")) 
dev.off()


# broj ucesnika sa odredjenim procentom tvitova o skupu (u deseticama), 
tweets_by_participant %>% group_by(rounded_percent = conf_tweets_percent %/% 10) %>% 
        summarise(freq = n())


### 148 ucesnika ima manje od 10% tvitova posvecenih skupu, sto je vise od trecine (43%)
### onih ucesnika koji su tvitovali u danima skupa. Sa druge strane, 48 ucesnika ima 100%
### tvitova o skupu! 

# ucesnici sa najmanjim procentom
tail(tweets_by_participant)


# ucesnici sa najvecim procentom tvitova o skupu
tweets_by_participant %>% filter(conf_tweets_percent == 100) %>%
        arrange(desc(total_tweets)) %>% print(n = Inf)

# od toga, ucesnici sa samo jednim tvitom o skupu
tweets_by_participant %>% filter(conf_tweets_percent == 100 & total_tweets == 1) %>%
        arrange(desc(total_tweets)) %>% print(n = Inf)

# procenat tvitova o skupu za organizatora
tweets_by_participant %>% filter(screenName == "wieilc")


retweets_by_participant <- tweets %>%
        filter(date == "2017-05-22" | date == "2017-05-23") %>%
        filter(conf == TRUE & retweet == TRUE) %>%
        group_by(screenName) %>% mutate(conf_retweets = n()) %>% 
        right_join(tweets_by_participant) %>%
        summarise(conf_tweets = first(conf_tweets), 
                  total_tweets = first(total_tweets), 
                  conf_tweets_percent = conf_tweets * 100 / total_tweets,
                  conf_retweets = ifelse(is.na(first(conf_retweets)), 0, first(conf_retweets)),
                  conf_retweets_percent = conf_retweets * 100 / conf_tweets) %>%
        arrange(desc(conf_tweets))





# ucesnici koji imaju samo tvitove sa skupa
retweets_by_participant %>% filter(conf_tweets_percent == 100) %>%
        arrange(desc(total_tweets), desc(conf_retweets_percent)) %>% print(n = Inf)

# ucesnici koji imaju samo retvitove sa skupa
retweets_by_participant %>% filter(conf_tweets_percent == 100 & conf_retweets_percent == 100) %>%
        arrange(desc(total_tweets), desc(conf_retweets_percent)) %>% print(n = Inf)

# oni koji imaju 100% poruka o skupu, ali je to samo jedna poruka
retweets_by_participant %>% filter(conf_tweets_percent == 100 & total_tweets == 1) %>%
        arrange(desc(conf_retweets)) %>% print(n = Inf)


#ucesnici koji imaju samo retvitove, koji procenat tvitova o skupu imaju?
retweets_by_participant %>% filter(conf_retweets_percent == 100) %>% 
        group_by(binned_conf_tweets_percent = conf_tweets_percent %/% 10) %>% 
        summarise(n = n())

### 134 ucesnika, ili 39% ucesnika koji su tvitovali o skupu u danima skupa, ima 
### manje od 10% tvitova o skupu, i to su retvitovi. Ovi ucesnici su vrlo malo
### posveceni skupu, i moze se postaviti pitanje o tome da li su stvarno ucestvovali
### na skupu ili su samo preneli neki tvit sa skupa.

# ucesnici koji nemaju retvitove!
retweets_by_participant %>% filter(conf_retweets_percent == 0) %>% 
        group_by(binned_conf_tweets_percent = conf_tweets_percent %/% 10) %>% 
        summarise(n = n())

### Ovo su ucesnici koji su doprineli raznovrsnosti poruka sa skupa koje su
### drugi retvitovali.

# ucesnici koji nemaju retvitove, a imaju samo tvitove o skupu u vreme trajanja skupa
retweets_by_participant %>% filter(conf_retweets_percent == 0 & conf_tweets_percent == 100) %>% 
        print(n=Inf)
# 21

# ucesnici koji nemaju retvitove, a imaju do 10% tvitova o skupu u vreme trajanja skupa
retweets_by_participant %>% filter(conf_retweets_percent == 0 & conf_tweets_percent <= 10) %>% 
        print(n=Inf)
# 11

retweets_by_participant %>% filter(conf_tweets >= 10 & conf_retweets_percent < 50) %>%
        arrange(desc(conf_tweets), conf_retweets_percent)

# ucesnici sa najvecim brojem originalnih tvitova
retweets_by_participant %>% filter(conf_retweets_percent == 0) %>% 
        print(n=20)

# ucesnici sa samo jednim originalnim tvitom
retweets_by_participant %>% filter(conf_retweets_percent == 0 & conf_tweets == 1) %>% 
        print(n=Inf)

theme_set(theme_bw())

png("visuals/2_02_aktivnost_ucesnika_procenat_retvitova_o_skupu.png")
ggplot(retweets_by_participant, aes(conf_retweets_percent)) +
        geom_histogram(binwidth = 10, col =  "gray40", fill = "#A3A500")+
        labs(title = "Aktivnost učesnika:\nprocenat retvitova o skupu", 
             x = "procenat retvitova u tvitovima o skupu",
             y = "broj učesnika") +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
              axis.text=element_text(size=12,face="bold"),
              axis.title=element_text(size=16,face="bold"))
dev.off()


png("visuals/2_03_aktivnost_ucesnika_koji_su_samo_retvitovali.png")
ggplot(retweets_by_participant %>% filter(conf_retweets_percent == 100), 
       aes(conf_tweets_percent)) +
        geom_histogram(binwidth = 10, col =  "gray40", fill = "#A3A500")+
        labs(title = "Aktivnost učesnika koji su samo retvitovali:\nprocenat tvitova o skupu", 
             x = "procenat tvitova o skupu",
             y = "broj učesnika") +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = 0.5,size = 20),
              axis.text=element_text(size=12,face="bold"),
              axis.title=element_text(size=14,face="bold"))
dev.off()

png("visuals/2_04_aktivnost_ucesnika_sa_originalnim_tvitovima.png")
ggplot(retweets_by_participant %>% filter(conf_retweets_percent == 0), 
       aes(conf_tweets_percent)) +
        geom_histogram(binwidth = 10, col =  "gray40", fill = "#A3A500")+
        labs(title = "Aktivnost učesnika koji imaju samo \noriginalne tvitove: procenat tvitova o skupu", 
             x = "procenat tvitova o skupu",
             y = "broj učesnika") +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
              axis.text=element_text(size=12,face="bold"),
              axis.title=element_text(size=14,face="bold"))
dev.off()

### Ucesnici su relativno malo tvitovali o skupu u danima skupa. Medju porukama 
### o skupu, nesto vise od polovine su originalni tvitovi, a ostalo su retvitovi
### koji sire poruke sa skupa na siru publiku, koja inace ne bi videla ove 
### poruke. Najveci broj poruka o skupu ima organizator, a veliki broj ucesnika.
### njih 148, ima manje od 10% tvitova o skupu medju svojim tvitovima u vreme 
### trajanja skupa. Kada je rec o retvitovanju, 209 ucesnika (skoro 61% onih 
### koji su tvitovali u danima skupa) je samo retvitovalo tudje poruke o skupu, 
### a od njih, 134 ima samo po jedan (re)tvit o skupu. Za ove ucesnike je upitno
### da li su stvarno prisustvovali skupu. Sa druge strane, 70 ucesnika ima samo 
### originalne tvitove o skupu, a od njih, najveci broj originalnih tvitova koje
### neki ucesnik ima je 65. Ovi ucesnici su najvise doprineli raznovrsnosti
### poruka o skupu.