### Analiza sentimenta

library(dplyr)
library(sentimentr)
library(stringr)
library(stringi)
library(tidytext)
library(ggplot2)


### Kakav je opsti utisak o skupu?
### Za odgovor na ovo pitanje analiziran je sentiment tvitova koji govore o skupu.

conf_tweets <- readRDS("results/tweets.RData") %>% filter(conf == TRUE)

conf_tweets %>% count()
# n
# 2366


### Najjednostavniji nacin za analizu sentimenta je preko recnika sentimenta.
### Odredivanje sentimenta tvita kao sume sentimenta pojedinacnih reci je dosta
### jednostavno, ali ne uzima u obzir kontekst, odnosno negacije ili priloge
### ("not good", "quite good", "somewhat good"). Paket sentimentr odreduje
### sentiment tako sto za svaku rec koja ima odredeni sentiment odredi kontekst 
### (broj reci pre i posle konkretne reci moze da se prosledi kao argument,
### podrazumevano je 4 i 2) u okviru koga negacije i modifikatori sentimenta
### uticu na ukupnu tezinu reci za koju se racuna sentiment. Kod negacija se
### vodi racuna o broju, sa pretpostavkom da se paran broj negacija ponistava;
### modifikatori pojacavaju ili smanjuju intenzitet sentimenta; suprotni veznici
### ("but", "however"..) takode uticu na intenzitet sentimenta, dajuci vecu
### tezinu delu konteksta posle veznika; zarez, tacka-zarez i dve tacke sluze
### kao dodatne granice u kontekstu, jer oznacavaju promenu u toku misli. Na
### kraju se suma tezina za sve reci podeli kvadratnim korenom broja reci,
### dajuci neograniceni skor za celu recenicu.

### Dve konstrukcije karakteristicne za Tviter poruke su pominjanja i hestegovi.
### Buduci da predstavljaju izraz licnih stavova i misljenja, hestegovi ce biti
### ukljuceni u analizu. Oni mogu da budu nosioci sentimenta:

sentiment("#happy") # => sentiment: 0.75

### Oznaka ucesnika komunikacije moze da bude rec koja nosi neki sentiment, ali 
### ne treba tako da se racuna. Zbog toga ce sva pominjanja ucesnika biti
### zamenjena oznakom @username.

sentiment("@happy:") # => sentiment: 0.75

conf_tweets_sentiment <- conf_tweets %>%
        mutate(text = str_replace_all(text, "@[A-Za-z\\d-_]+:?", "@username"))


### Funkcija sentiment primenjena na ceo tekst (tvit) vraca po jednu vrednost
### sentimenta za svaku recenicu (povratna vrednost je tipa data frame sa cetiri
### kolone, od kojih je "sentiment" cetvrta). Za konacan skor tvita ce se uzeti
### prosecna vrednost svih recenica u tvitu.


get_conf_sentiments_mean <- function(tweetset){
        tweetset <- tweetset %>% mutate(sentiment_mean = 0)
        
        for(i in 1:nrow(tweetset)) {
                sents <- sentiment(tweetset[i, "text"])
                tweetset[i, "sentiment_mean"] <- apply(sents[,4], 2, mean)
        }
        tweetset
}


# izvrsava se oko 2min!
conf_tweets_sentiment <- get_conf_sentiments_mean(conf_tweets_sentiment)



### Prvih 10 najpozitivnijih tvitova:

conf_tweets_sentiment %>% 
        arrange(desc(sentiment_mean)) %>% 
        top_n(10) %>% select(text, sentiment_mean) 
#                         
# 1 #wielead @username \ncleanest safest and most reliable utility!!!\nlooking to hire powerful engineers! 
# 2 growing your professional strengths by investing in your personal passions #wielead #leadership #philanthropy 
# 3 thank you for enabling us to do so much to inspire, engage and advance women in engineering! 
# 4 #wielead #innovation \ngreat to listen to texas instruments history, vision, integrity, innovation, commitment !! 
# 5 rt @username thank you for enabling us to do so much to inspire, engage and advance women in engineering! 
# 6 .@username you can have talent, but to be a master you have to hone that talent #wielead
# 7 rt @username .@username you can have talent, but to be a master you have to hone that talent #wielead
# 8 #wielead #inspire#improve#visibility#enhancement\nwomen for technical leadership @username
# 9 rt @username hope all the girls are really rocking the event, great to meet real engineers @username  great networking  and learning f\032
# 10excited two days with fabulous leaders @username great stories have already excited and inspired the audience.\032 
# sentiment_mean
# 1 1.0478668
# 2 0.9984604
# 3 0.9943961
# 4 0.9707253
# 5 0.9406045
# 6 0.9398255
# 7 0.8889860
# 8 0.8063808
# 9 0.7973707
# 10 0.7500000         

### Tvitovi su stvarno pozitivni. Jedino se moze postaviti pitanje koliko su
### same teme skupa doprinele da neki tvitovi budu tako pozitivno ocenjeni, na 
### primer tvit: 
# "#wielead #innovation \ngreat to listen to texas instruments history, 
# vision, integrity, innovation, commitment !!"

sentiment("innovation") #0.75


### Evo i najnegativnijih tvitova:

conf_tweets_sentiment %>% 
        arrange(sentiment_mean) %>% 
        top_n(-10) %>% select(sentiment_mean, text)
# sentiment_mean
# 1 -0.6798423
# 2 -0.6030227
# 3 -0.6000000
# 4 -0.5427204
# 5 -0.5427204
# 6 -0.4647385
# 7 -0.4483060
# 8 -0.4400000
# 9 -0.3952847
# 10 -0.3500000
# text
# 1 it's the same bs we always got around the mascot issue at fsu. it's depressing and upsetting #wielead
# 2 @username @username @username i wish i'd made it to that booth!
# 3 huge potential for disaster robotics #wielead #rescuerobotics:  via @username
# 4 rt @username huge potential for disaster robotics #wielead #rescuerobotics:  via @username
# 5 rt @username huge potential for disaster robotics #wielead #rescuerobotics:  via @username
# 6 rt @username #wielead what's leadership: get comfortable with being uncomfortable! not the spotlight but motivating  championing other\032
# 7 +tips: stretch ur risk muscle; u don't need to be 100% ready2accept promotion; find a career sponsor  don't fear2waste their time #wielead
# 8 harvey mudd isn't my fight, but i'm disappointed ieee had a keynote speaker in the midst of such a divisive situation w/o context #wielead
# 9 facebook accused of gender bias against women engineers  via @username
# 10 .@username most common struggles for athletes: lack of confidence, loss of enjoyment, self sabotage, etc  #wielead


### Najnegativniji tvit jeste vrlo negativan, iako nije jasno iz teksta koliko 
### se bas odnosi na sam skup: 
# "it's the same BS we always got around the mascot issue at FSU. 
# It's depressing and upsetting #wielead "

### Kao i za pozitivne tvitove, za neke negativne tvitove nije jasno koliko su 
### stvarno negativni, a koliko je taj sentiment rezultat samih tema skupa: 
# "facebook accused of gender bias against women engineers  via @username"
# ".@username most common struggles for athletes: lack of confidence, loss of 
# enjoyment, self sabotage, etc  #wielead"

### Prenesena znacenja, kao i ironiju i sarkazam, pristup analizi sentimenta 
### preko recnika ne moze da otkrije i ispravno oceni, ali ovde
### ne izgleda da je bilo vecih problema sa tim.

### Medu najnegativnijim tvitovima se naslo i nekoliko onih koji se odnose bas na govornike i
### predavanja na skupu, na primer: "harvey mudd isn't my fight, but i'm disappointed 
### ieee had a keynote speaker in the midst of such a divisive situation w/o context #wielead"


### Koje su pojedinacne reci koje najvise doprinose pozitivnom i negativnom
### sentimentu?

conf_words_sentiment <- conf_tweets_sentiment %>% 
        unnest_tokens(word, text, token = "regex", pattern = "[^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@])") %>%
        count(timeframe, word) %>% 
        mutate(word_sent = sentiment(word)[,4][[1]], t_word_sent_by_timefr = n * word_sent) %>%
        group_by(word) %>% mutate(t_word_sent_total = sum(t_word_sent_by_timefr))


### po desetak reci koje najvise doprinose pozitivnom i negativnom sentimentu
conf_words_sentiment %>% group_by(word) %>% summarise(t_word_sent_total = max(t_word_sent_total)) %>%
        group_by(t_word_sent_total < 0) %>% 
        top_n(., 10, abs(t_word_sent_total)) %>% 
        ungroup() %>%
        arrange(desc(t_word_sent_total)) %>%
        print(n=Inf)


png("visuals/1_01_doprinos_reci_sentimentu.png")
conf_words_sentiment %>% group_by(word) %>% 
        summarise(t_word_sent_total = max(t_word_sent_total)) %>%
        group_by(t_word_sent_total < 0) %>% 
        top_n(10, abs(t_word_sent_total)) %>% 
        ungroup() %>%
        mutate(word = reorder(word, t_word_sent_total)) %>%
        ggplot(aes(word, t_word_sent_total, fill = t_word_sent_total < 0)) +
        geom_col() + coord_flip() + 
        labs(title = "doprinos sentimentu pojedinačnih reči", 
             x = NULL, y = "sentiment * broj pojavljivanja") + 
        scale_fill_discrete(name = "", labels = c("pozitivan", "negativan")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))
dev.off()

### Medu deset najpozitivnijih su se nasle reci "learn", "great", "honored",
### "congratulations", "excited", "amazing", a medu najnegativnijima su "stop",
### "disaster", "bias", "competition"... Po periodima:

words_sent_by_timeframe <- split(conf_words_sentiment, conf_words_sentiment$timeframe) # zbog lakse vizualizacije

periods <- c("od 21.04.2017. do 04.05.2017.", "od 05.05.2017. do 18.05.2017.", 
             "od 19.05.2017. do 26.05.2017.", 
             "od 27.05.2017. do 09.06.2017.", "od 10.06.2017. do 23.06.2017.")

top_words_by_timeframe <- lapply(1:length(words_sent_by_timeframe), function(x) {
        w <- words_sent_by_timeframe[[x]] %>% group_by(t_word_sent_by_timefr < 0) %>% 
                top_n(5, abs(t_word_sent_by_timefr)) %>%
                ungroup() %>% 
                arrange(desc(t_word_sent_by_timefr))})


lapply(1:length(words_sent_by_timeframe), function(x) {
        filename <- paste0("visuals/1_02_doprinos_reci_sentimentu_za_period_", x, ".png")
        s <- words_sent_by_timeframe[[x]] %>% group_by(t_word_sent_by_timefr < 0) %>% 
                top_n(10, abs(t_word_sent_by_timefr)) %>%
                ungroup() %>% mutate(word = reorder(word, t_word_sent_by_timefr))
        t <- paste0("doprinos reci sentimentu\nza period", periods[x])
        png(filename)
        print(ggplot(s, aes(word, t_word_sent_by_timefr, fill = t_word_sent_by_timefr < 0)) +
                geom_col() + coord_flip() + 
                labs(title = t, 
                     x = NULL, y = "sentiment * broj pojavljivanja") + 
                scale_fill_discrete(name = "", labels = c("pozitivan", "negativan")) +
                theme(plot.title = element_text(face = "bold", hjust = 0.5)))
        dev.off()
})


### Od pozitivnih reci, u periodima pre skupa dominira "learn", nesto ispod su
### "join" i "sponsor", u periodu trajanja skupa, najpozitivnije su reci
### "great", "honored", "learn", "congratulations", dok su u prvom periodu posle
### skupa to reci "thanks", "great", "learning", "inspiration", i upravo ovo bi
### moglo da se shvati kao pozitivna ocena nakon odrzanog skupa. Medu negativnim
### recima ima nekih koje izgledaju pogresno ocenjene, kao "interviewed", a i
### nekih koje u ovom domenu imaju drugacije znacenje, kao "disruptive". Mozda
### ce bigrami dati malo vise konteksta:

conf_bigrams_sentiment <- conf_tweets_sentiment %>% 
        mutate(text = str_replace_all(text, pattern = "\\.", replacement = " ")) %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% count(timeframe, bigram) %>% 
        mutate(bigram_sent = sentiment(bigram)[,4][[1]], t_big_sent_by_timefr = n * bigram_sent) %>%
        group_by(bigram) %>% mutate(t_big_sent_total = sum(t_big_sent_by_timefr))


bigrams_sent_by_timeframe <- split(conf_bigrams_sentiment, conf_bigrams_sentiment$timeframe)

top_bigrams_by_timeframe <- 
        lapply(1:length(bigrams_sent_by_timeframe), 
               function(x) {
                       w <- bigrams_sent_by_timeframe[[x]] %>% 
                               group_by(t_big_sent_by_timefr < 0) %>% 
                               top_n(10, abs(t_big_sent_by_timefr)) %>%
                               ungroup() %>% 
                               arrange(desc(t_big_sent_by_timefr))})

top_bigrams_by_timeframe[[1]] %>% select(bigram, t_big_sent_by_timefr)

lapply(1:length(top_bigrams_by_timeframe), function(x) {
        s <- bigrams_sent_by_timeframe[[x]] %>% group_by(t_big_sent_by_timefr < 0) %>% 
                top_n(10, abs(t_big_sent_by_timefr)) %>%
                ungroup() %>% mutate(bigram = reorder(bigram, t_big_sent_by_timefr))
        t <- paste0("doprinos bigrama sentimentu\nza period ", periods[x])
        filename <- paste0("visuals/1_03_doprinos_bigrama_sentimentu_za_period_", x, ".png")
        png(filename)
        print(ggplot(s, aes(bigram, t_big_sent_by_timefr, fill = t_big_sent_by_timefr < 0)) +
                geom_col() + coord_flip() + 
                labs(title = t, x = NULL, y = "sentiment * broj pojavljivanja") + 
                scale_fill_discrete(name = "", labels = c("pozitivni", "negativni")) +
                theme(plot.title = element_text(face = "bold", hjust = 0.5)))
        dev.off()
})



### Pojavljivanje reci u kontekstu je donekle promenilo rang reci (na primer, u 
### prvom vremenskom odeljku, "learn" se pojavljuje cesce nego "sponsor", ali u 
### varijantama "learn more", "learn about", "learn from", pa su ovi bigrami 
### nesto redi, dok se "sponsor" uvek pojavljuje kao "our sponsor", odnosno 
### "(our) sponsor Ericsson"). Ipak, ovako se dobije malo jasnija slika 
### ocenjenih reci. Na primer, u drugom periodu, "sneak" se odnosi na "(a) sneak
### peek", sto nije negativno, "blocks" na "blocks 9am" (nejasno) i "building 
### blocks" (pozitivno). U trecem periodu, "stop" se odnosi na "stop by" (ne
### negativno), "disaster" na "(for) disaster robotics", sto nije negativan
### pojam. Pozitivni bigrami su uglavnom ispravno ocenjeni, osim mozda "our
### sponsor", "career fair", machine learning", "learn about i slicne, koje bih
### licno ocenila kao neutralne, ali svakako nemaju tako izrazeno suprotno
### znacenje kao neki od navedenih negativno ocenjenih bigrama.

### Cak i sa ovakvim rezultatima, gde je negativni sentiment mozda preuvelican,
### intenzitet negativnog sentimenta je mnogo manji nego intenzitet pozitivnog.

### Sentiment po vremenskim periodima: sumiran, normalizovan brojem tvitova,
### maksimalan i minimalan rezultat za pojedinacni tvit:

conf_tweets_sentiment %>% group_by(timeframe) %>% 
        summarise(sum_sentiment = sum(sentiment_mean), num = length(timeframe), 
                  norm_sentiment = sum_sentiment / num,
                  max_sentiment = max(sentiment_mean), 
                  min_sentiment = min(sentiment_mean))

# procenat neutralnih tvitova
(conf_tweets_sentiment %>% 
                filter(sentiment_mean == 0) %>% 
                count()) * 100 / nrow(conf_tweets)

# slicno, malo sire, tvitovi sa veoma niskim skorom sentimenta
(conf_tweets_sentiment %>% 
                filter(sentiment_mean < 0.1 & sentiment_mean > -0.1) %>% 
                count()) * 100 / nrow(conf_tweets)



### Buduci da ova analiza dosta govori o samim temama skupa, a u toku
### konferencije je potencijalno bilo vise rasprave nego u periodima posle
### skupa, mozda bi bilo zanimljivo pogledati koji su tvitovi najpozitivniji i 
### najnegativniji za svaki period.


timefr <- sort(unique(conf_tweets_sentiment$timeframe), decreasing = FALSE)

time_sent <- lapply(1:length(timefr), function(x){
        res <- list()
        sent <- conf_tweets_sentiment %>% 
                filter(timeframe == timefr[x]) %>% 
                arrange(sentiment_mean)
        res$pos <- sent %>% top_n(3) %>% arrange(desc(sentiment_mean)) %>% select("sentiment_mean", "text")
        res$neg <- sent %>% top_n(-3) %>% select("sentiment_mean", "text")
        return(res)
})


### Iz ovog sazetog sadrzaja tvitova moze se primetiti da je ovakva analiza
### donekle problematicna, buduci da ne moze da razdvoji tvitove koji govore o
### samom skupu, koliko je dobro ili lose organizovan, koliko je zanimljiv ili 
### kakav je kvalitet predavaca, i one koji govore o temama skupa, a koje u ovom
### slucaju nisu neutralne i pominju liderstvo, inovacije, osnazivanje, ali i
### predrasude i borbu za veca prava.


### Kakav je sentiment kljucnih reci izdvojenih iz programa skupa, i koliko one
### ucestvuju u ukupnom sentimentu? (Okvirno, jer u celom tvitu na njih utice i
### kontekst.) Da bih do ovoga dosla, tabelu sentimenta po recima i tabelu
### kljucnih reci sam spojila po zajednickoj osnovi reci, buduci da se, na
### primer, "disaster" u kljucnim recima javlja samo u mnozini, a u tvitovima
### samo u jednini. Zatim sam za svaku rec sumirala broj pojavljivanja i ukupan
### sentiment (u tabeli conf_word_sentiment su ovi podaci dati po periodima).

program_keywords <- readRDS("results/conf_keywords_df.RData") %>% distinct(word)
program_bigrams <- readRDS("results/conf_bigrams_df.RData")

### koje reci koje najvise doprinose sentimentu pripadaju kljucnim recima, tj temama skupa?
conf_words_sentiment %>% group_by(word) %>% summarise(t_word_sent_total = max(t_word_sent_total)) %>%
        group_by(t_word_sent_total < 0) %>% 
        top_n(10, abs(t_word_sent_total)) %>% 
        ungroup() %>%
        arrange(desc(t_word_sent_total)) %>% 
        transmute(word = tm::stemDocument(word)) %>%
        inner_join(program_keywords %>% transmute(word = tm::stemDocument(word))) %>%
        unique()


### Obrnuto, koliko su kljucne reci tema skupa doprinele sentimentu? 
### Gledano po osnovi reci, za sve periode zajedno
keywords_sentiment <- conf_words_sentiment %>% 
        mutate(word = tm::stemDocument(word)) %>%
        select(word, t_word_sent_by_timefr) %>%
        group_by(word) %>%  summarise(sent_total = sum(t_word_sent_by_timefr)) %>%
        inner_join(program_keywords %>%
                           transmute(word = tm::stemDocument(word)) %>%
                           unique()) %>%
        arrange(sent_total)

keywords_sentiment %>% group_by(sent_total < 0) %>% top_n(5, abs(sent_total)) %>%
        ungroup()

png("visuals/1_04_doprinos_kljucnih_reci_skupa_sentimentu.png")
keywords_sentiment %>% group_by(sent_total < 0) %>% top_n(5, abs(sent_total)) %>%
        ungroup() %>% mutate(word = reorder(word, sent_total)) %>%
        ggplot(aes(word, sent_total, fill = sent_total < 0)) +
        geom_col() + coord_flip() + 
        labs(title = "5 ključnih reči koje najviše\ndoprinose sentimentu", x = NULL, 
             y = "sentiment * broj pojavljivanja") + 
        scale_fill_discrete(name = "", labels = c("pozitivne", "negativne")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))
dev.off()


### Teme skupa su u velikoj meri uticale na to da ukupan sentiment bude izrazito
### pozitivan. Termin "disaster robotics", iako je pogresno ocenjen kao 
### negativan, nije imao veceg udela u ukupnom sentimentu. Buduci da je 
### ocekivano da ce se o temama skupa dosta govoriti u ovim tvitovima, može se 
### probati sa kontekstualizacijom ovih reči i videti kakva je ocena sentimenta
### ako se ne posmatraju teme skupa. Ipak, teme su u ovom slucaju predstavljene
### vrlo sirokim i opstim terminima, kao sto su ucenje, podsticanje, ohrabrenje,
### pa je moguce da ce se na ovaj nacin neutralisati i vise od samih tema skupa.


### Pokusace se kontekstualizacija najuticajnijih kljucne reci izmenom njihove ocene 
### sentimenta u recniku koji se koristiza analizu.

top_words <- keywords_sentiment %>% group_by(sent_total < 0) %>% 
        top_n(5, abs(sent_total)) %>% ungroup() %>% select(word)

### Recnik i reci koje treba izmeniti se spajaju preko osnove, tako ce se
### izmeniti i "bias" i "biased", ali nece "wellinformed", iako "well" hoce. 
### lexicon::hash_sentiment_jockers_rinker je podrazumevani recnik koji paket
### sentimentr koristi, a proverom nekoliko reci u razlicitim recnicima koje
### ovaj paket koristi, izgleda da jeste i najbolja opcija.

words_to_update <- lexicon::hash_sentiment_jockers_rinker %>%    
        mutate(stemmed = tm::stemDocument(lexicon::hash_sentiment_jockers_rinker$x)) %>%
        inner_join(top_words %>% mutate(word = tm::stemDocument(top_words$word)) %>% 
                           distinct(word), by = c("stemmed" = "word")) %>% select(x) %>% mutate(y = 0)


### Funkcija update_polarity_table dodaje i izbacuje reci iz recnika (dve
### odvojene operacije, ne moze da izmeni vrednost sentimenta za rec koja se vec
### nalazi u recniku)
updated_polarity_table <- update_polarity_table(lexicon::hash_sentiment_jockers_rinker, 
                                                drop = words_to_update$x,  # moraju prvo da se izbace postojece vrednosti
                                                x = data.frame(x = words_to_update$x, y = words_to_update$y))


### Nove ocene sentimenta:

conf_tweets_sentiments2 <- conf_tweets_sentiment[,-7]


# izvrsava se oko 2min!
get_conf_sentiments_updated <- function(tweetset){
        tweetset <- tweetset %>% mutate(sentiment_mean = 0)
        
        for(i in 1:nrow(tweetset)) {
                sents <- sentiment(tweetset[i, "text"], updated_polarity_table)
                tweetset[i, "sentiment_mean"] <- apply(sents[,4], 2, mean)
        }
        tweetset
}

conf_tweets_sentiments2 <- get_conf_sentiments_updated(conf_tweets_sentiments2)


### Po pet najpozitivnijih i najnegativnijih tvitova: (opadajuce do najnegativnijeg)
conf_tweets_sentiments2 %>% filter(retweet == FALSE) %>%
        group_by(sentiment_mean < 0) %>% 
        top_n(10, abs(sentiment_mean)) %>% ungroup() %>%
        arrange(desc(sentiment_mean)) %>% select(text, sentiment_mean)


### Sa novim ocenama sentimenta, medu najpozitivnijim i najnegativnijim
### tvitovima nema neke velike promene, vecina ovih tvitova je i u pocetnoj
### analizi bila medu prvih deset, a i najpozitivniji i najnegativniji tvit je
### isti kao i sa prvim ocenama.

conf_tweets_sentiments2 %>% group_by(timeframe) %>% 
        summarise(sum_sentiment = sum(sentiment_mean),
                  num = length(timeframe), 
                  norm_sentiment = sum_sentiment / num,
                  max_sentiment = max(sentiment_mean), 
                  min_sentiment = min(sentiment_mean))

### Maksimalna i minimalna ocena sentimenta su ostale iste, a i ostali zakljucci
### i dalje vaze, jedina je razlika u tome sto su tvitovi treceg perioda
### pozitivniji i od tvitova drugog i cetvrtog perioda. Kod ucestvovanja
### pojedinacnih reci u sentimentu, razlika je samo u rangu reci (zbog izbacenih
### reci).


time_sent2 <- lapply(1:length(timefr), function(x){
        res <- list()
        sent <- conf_tweets_sentiments2 %>% 
                filter(timeframe == timefr[x]) %>% 
                arrange(sentiment_mean)
        res$pos <- sent %>% top_n(3) %>% arrange(desc(sentiment_mean)) %>% select("sentiment_mean", "text")
        res$neg <- sent %>% top_n(-3) %>% select("sentiment_mean", "text")
        return(res)
})

############################# ridge plot distribucije sentimenta 
##############################  pre i posle kontekstualizacije
library(ggridges)
theme_set(theme_ridges())

conf_tweets_sentiment$timeframe <- recode(conf_tweets_sentiment$timeframe, "time_1" = "Period 1", "time_2" = "Period 2", "time_3" = "Period 3", "time_4" = "Period 4", "time_5" = "Period 5")

conf_tweets_sentiments2$timeframe <- recode(conf_tweets_sentiments2$timeframe, "time_1" = "Period 1", "time_2" = "Period 2", "time_3" = "Period 3", "time_4" = "Period 4", "time_5" = "Period 5")

png("visuals/1_05_distribucija_sentimenta_ridge.png")
ggplot(conf_tweets_sentiment, aes(x = sentiment_mean, y = timeframe)) +
        geom_density_ridges(aes(fill = timeframe)) +
        labs(title = "Distribucija sentimenta", x = "sentiment tvita", y = "vremenski period") + 
        theme_bw() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
              axis.text=element_text(size=12,face="bold"),
              axis.title=element_text(size=14,face="bold"),
              strip.text.x = element_text(size = 14, face = "bold"),
              legend.position = "none")
dev.off()



png("visuals/1_05_distribucija_sentimenta_posle_kontekstualizacije_ridge.png")
ggplot(conf_tweets_sentiments2, aes(x = sentiment_mean, y = timeframe)) +
        geom_density_ridges(aes(fill = timeframe)) +
        labs(title = "Distribucija sentimenta\nposle kontekstualizacije", x = "sentiment tvita", y = "vremenski period") + 
        theme_bw() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
              axis.text=element_text(size=12,face="bold"),
              axis.title=element_text(size=14,face="bold"),
              strip.text.x = element_text(size = 14, face = "bold"),
              legend.position = "none")
dev.off()


saveRDS(conf_tweets_sentiments2, "results/conf_tweets_sentiments_updated.RData")
