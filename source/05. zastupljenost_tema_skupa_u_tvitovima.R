# Zastupljenost tema skupa u tvitovima
# Koliko su teme skupa zanimljive za ucesnike?
# Da li teme skupa ostaju predmet interesovanja ucesnika i posle skupa?

library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)

# svi posmatrani tvitovi
tweets <- readRDS("results/tweets.RData")
# bigrami (teme) izdvojeni iz programa skupa
conf_bigrams <- readRDS("results/conf_bigrams_df.RData") 



### Tvitovi su podeljeni na bigrame, a zatim su ti bigrami pretrazeni za
### poklapanje sa bigramima koji predstavljaju teme skupa.

tweets_bigrams <- tweets %>% unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = FALSE) %>%
        separate(bigram, c("word1", "word2"), sep = " ")

conf_matched_bigrams <- inner_join(tweets_bigrams, conf_bigrams) %>% unite(bigram, word1, word2, sep = " ")

### Ako se uparuju bigrami, odnosno identicna konstrukcija, takvih poklapanja je
### relativno malo (665).
#broj pominjanja bigrama po vremenskom periodu
conf_matched_bigrams %>% group_by(timeframe) %>% count()
# 81, 107, 329, 78, 70

#broj razlicitih bigrama koriscenih ukupno
conf_matched_bigrams %>% distinct(bigram) %>% count() 
#82


### Koriscenje razlicitih bigrama (tema) po periodima:
conf_matched_bigrams %>% distinct(timeframe, bigram) %>% count(timeframe)
# time_1 : time_5 =>  20 29 72 24 27
### U periodu skupa je korisceno najvise razlicitih tema.

# broj razlicitih ucesnika koji je koristio bigrame
conf_matched_bigrams %>% distinct(screenName) %>% count() 
# 212
# a po vremenskom periodu
conf_matched_bigrams %>% distinct(timeframe, screenName) %>% count(timeframe)
# time_1 : time_5 =>  53 55 139 44 39


### Nekoliko nacina je isprobano u pokusaju da se dobije veci broj poklapanja:
### prvo prosirenjem pretrage na nesusedne reci u tvitu (opcija "skip_ngrams" za
### tokenizaciju n-grama, sa maksimalno k reci izmedu n reci koje cine n-gram), 
### a zatim i pretrazivanjem samo po osnovama reci. Rezultati su bili nesto 
### bolji, u smislu da je doslo do veceg broja poklapanja, ali se cinilo da je
### upotreba i dalje dosta retka. Na kraju je pretraga prosirena na ceo tvit, u
### kombinaciji sa pretragom samo po osnovama reci.

### Pretrazivanjem celog tvita, odnosno pojedinacnih reci, bez obzira na
#poziciju reci u tvitu, moguce je upariti, na primer, bigram "microsoft
#hololens" u tvitu: 
#@IEEESA: Deanna Hearns demonstrates @Microsoft's vision of the future of
##MixedReality with the @HoloLens at #WIELead #IEEEWIE @WIEILC h… 

### Ovde su reci koje cine bigram/temu skupa suvise udaljene jedna od druge u
### tekstu tvita, pa bi opciji skip_ngrams morao da se prosledi veliko k (broj
### reci koji moze da se nade izmedu reci koje cine n-gram). Drugi primer je
### uparivanje "robots disasters" u tvitu: 
#Thanks for sharing your thoughts on the potential for disaster #robotics,
#@robinrmurphy! 
### gde se ove reci nalaze u obrnutom redosledu, tako da pomocu "skip_ngrams" 
### opcije ne bi uopste bilo moguce upariti ovaj bigram. Osim toga, u
### ovom slucaju je neophodno i pretrazivanje po osnovama reci.

### Za pretrazivanje po pojedinacnim recima, prvo su i tvitovi i bigrami (teme)
### podeljeni na reci.

### bigrami (teme) su podeljeni na dve reci, ali je svaka u posebnoj koloni;
### potrebno ih je staviti u jednu kolonu po kojoj ce se vrsiti spajanje sa
### recima tvitova; bigid ce biti id bigrama, a which ce sadrzati naziv
### originalne kolone reci (word1/word2)
gathered_conf_bigrams <- conf_bigrams %>% select(-track) %>% distinct() %>% #neki bigr. su isti za vise trekova
        mutate(bigid = 1:nrow(.)) %>%                                              #id bigrama 
        gather(which, word, word1, word2) %>%                                      #kombinuje word1 i word2 u 1 kolonu
        mutate(word = tm::stemDocument(word))


### izvrsava se oko 5 minuta! deljenje tvita na reci i svodenje na osnovni oblik
tweets_words <- tweets %>% unnest_tokens(word, text, token = "words") %>%
        mutate(word = tm::stemDocument(word))

### spajanje tvitova i tema skupa, sa inner_join ce rezultat biti samo uparene
### reci; izbacivanje tvitova/bigrama u kojima je uparena samo jedna rec tog
### bigrama
matched_by_word <- inner_join(tweets_words, gathered_conf_bigrams) %>% 
        group_by(id, bigid) %>% filter(n() >= 2)          


### U matched_by_word su za svaki tvit ostale samo reci koje su se poklopile sa 
### recima iz tema skupa. Dalje je potrebno proveriti da li su, na primer, za
### cetiri pojavljivanja iste (id tvita-id bigrama) kombinacije sva cetiri ista 
### rec, ili tri iste i jedna razlicita, sto cini jedan kompletan bigram, ili po
### dve razlicite - dva kompletna bigrama. Ovo je izvedeno tako sto su spojene
### sve reci koje cine jedan tvit, s tim da su spojene i u pravom i u obrnutom 
### redosledu, da bi bilo moguce otkriti i bigrame koji imaju obrnut red reci. 
### Tako dobijeni "tekst" je podeljen na bigrame i pretrazen za poklapanje sa 
### bigramima/temama.

### Za ovo je neophodno ponovo spojiti reci tema u bigrame
united_conf_bigrams <- gathered_conf_bigrams %>% group_by(bigid) %>% spread(which, word) %>% 
        unite(bigram, word1, word2, sep = " ") %>% ungroup() %>% select(-bigid)


matched_by_bigram <- matched_by_word %>% group_by(id, bigid) %>% distinct(word, .keep_all = TRUE) %>%
        summarise(text = str_c(word, rev(word), sep = " ", collapse = " "), # spajanje svih reci u tekst, u oba smera
                  timeframe = first(timeframe), screenName = first(screenName)) %>%  # da se ne izgube kolone sumarizacijom
        unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = FALSE) %>% # izdvajanje bigrama
        inner_join(united_conf_bigrams) %>% ungroup()                                # spajanje sa temama

# ovi tvitovi ce biti oznaceni kao oni koji govore o temama skupa!

has_topics <- tweets %>% left_join(matched_by_bigram %>% group_by(id) %>% summarise(n = n())) %>%
        mutate(has_bigram = ifelse(!is.na(n), TRUE, FALSE)) %>% select(id, has_bigram)

saveRDS(has_topics, "results/has_topics.RData")

#### BROJ TEMA
# ukupno koriscenje tema je 2225, a po periodima:
matched_by_bigram %>% group_by(timeframe) %>% count()
# 326, 461, 698, 322, 418

### Broj pojavljivanja tema skupa u tvitovima u ukupno posmatranom periodu nije 
### veliki, moze se reci da odgovara broju tvitova koji govore o skupu. Kada se 
### posmatra po vremenskim periodima, moze se reci da je u periodu skupa najvise
### tvitovano o temama skupa, ali da su teme skupa i inace zanimljive ovim
### korisnicima Tvitera i da ih koriste i pre i posle skupa.

#### BROJ RAZLICITIH TEMA
#broj razlicitih tema koje su pomenute bilo kad
matched_by_bigram %>% distinct(bigram) %>% count()
# 117

### broj razlicitih bigrama (prepoznatih tema) po periodu:
matched_by_bigram %>% distinct(timeframe, bigram) %>% count(timeframe)
# 58  77 105  67  68 

### U vreme skupa su teme skupa o kojima je tvitovano i najraznovrsnije.

# koje su teme najzanimljivije, a koje su koriscene samo jednom?

matched_by_bigram %>% group_by(bigram) %>% count() %>% arrange(desc(n)) %>% head(n = 10)
# tech women, ieee women, start up
matched_by_bigram %>% group_by(bigram) %>% count() %>% arrange(desc(n)) %>% tail(n = 10)

# o kojim temama je najvise tvitovao organizator?
matched_by_bigram %>% filter(screenName == "wieilc") %>%
        group_by(bigram) %>% count() %>% arrange(desc(n)) 

### Najzanimljivije teme su "tech women", "ieee women" i "start up", a najmanje 
### zanimljive su "vmware win", "transform technic" i "super grid"; ipak, i o
### ovim najmanje zanimljivim temama se tvitovalo

matched_by_bigram %>% group_by(bigram) %>% count() %>% filter(n == 1) %>% nrow()

### Samo 6 tema je pomenuto samo jednom, a ne treba zaboraviti ni 141-117=24
### teme o kojima se nije tvitovalo uopste



### BROJ UCESNIKA 
# broj razlicitih ucesnika koji je koristio bigrame
matched_by_bigram %>% distinct(screenName) %>% count()
# 370
# a po vremenskom periodu
matched_by_bigram %>% distinct(timeframe, screenName) %>% count(timeframe)
# time_1 : time_5 =>  152 179 213 138 171

### Kod 80.9% ucesnika je primeceno pominjanje tema skupa u bilo kom vremenskom 
### periodu, a najvise ucesnika je tvitovalo o temama skupa u periodu skupa,
### 46.6% svih ucesnika.

# samo jednu temu je pomenulo
matched_by_bigram %>% group_by(screenName) %>% count() %>% filter(n==1) %>% nrow()
# 86 ucesnika

# broj pominjanja tema po ucesniku
matched_by_bigram %>% group_by(screenName) %>% count() %>% arrange(desc(n))

# broj pominjanja tema od strane organizatora
matched_by_bigram %>% filter(screenName =="wieilc") %>% nrow()

### Organizator je najvise tvitovao o temama skupa, 122 puta, i to o 53
### razlicite teme, dok je 86 ucesnika samo jednom pomenulo neku temu skupa.

# distribucija pominjanja tema skupa u tvitovima
# bas bazicni plot, ali se vidi power-law distribucija
matched_by_bigram %>% group_by(screenName) %>% count() %>% 
        ungroup() %>% arrange(desc(n)) %>% 
        ggplot(aes(1:370, n)) + geom_line() + 
        geom_abline(intercept = log10(130), slope = -0.8) +
        scale_x_log10() + scale_y_log10()



#POSLE SKUPA -------------------------

# ukupno koriscenje tema je 2255, a po periodima:
matched_by_bigram %>% group_by(timeframe) %>% count()
# 326, 461, 698, 322, 418
matched_by_bigram %>% filter(timeframe == "time_4" | timeframe == "time_5") %>% count()
# 740

#### BROJ RAZLICITIH TEMA

### broj razlicitih bigrama (prepoznatih tema) po periodu:
matched_by_bigram %>% distinct(timeframe, bigram) %>% count(timeframe)
# 58  77 105  67  68 

# broj razlicitih tema kada se gleda period posle skupa zajedno
matched_by_bigram %>% filter(timeframe == "time_4" | timeframe == "time_5") %>%
        distinct(bigram) %>% count()
# 81

# broj razlicitih tema u periodu pre skupa 
matched_by_bigram %>% filter(timeframe == "time_1" | timeframe == "time_2") %>%
        distinct(bigram) %>% count()
# 86

### Posle skupa, ukupno gledano, o temama skupa je tvitovano 740 puta, i to o 81 temi,
### dok je u periodu pre skupa sve zajedno tvitovano o 86 tema.

# koje su teme najzanimljivije, a koje su koriscene samo jednom?

matched_by_bigram %>% filter(timeframe == "time_4" | timeframe == "time_5") %>%
        group_by(bigram) %>% count() %>% arrange(desc(n)) %>% head(n = 10)
# tech women, up leadership, winner announc...

matched_by_bigram %>% filter(timeframe == "time_4" | timeframe == "time_5") %>%
        group_by(bigram) %>% count() %>% arrange(desc(n)) %>% tail(n = 10)

# o kojim temama je najvise tvitovao organizator? --------------------------------------
matched_by_bigram %>% filter(timeframe == "time_4" | timeframe == "time_5") %>%
        filter(screenName == "wieilc") %>%
        group_by(bigram) %>% count() %>% arrange(desc(n)) %>% summarise(sum(n))


matched_by_bigram %>% filter(timeframe == "time_4" | timeframe == "time_5") %>%
        group_by(bigram) %>% count() %>% filter(n == 1)
# 8 tema je pomenuto samo jednom


### BROJ UCESNIKA 
# broj razlicitih ucesnika koji je koristio bigrame
matched_by_bigram %>% filter(timeframe == "time_4" | timeframe == "time_5") %>% 
        distinct(screenName) %>% count()
# 224

# samo jednu temu je pomenulo
matched_by_bigram %>% filter(timeframe == "time_4" | timeframe == "time_5") %>% 
        group_by(screenName) %>% count() %>% filter(n==1) %>% nrow()
# 107 ucesnika

# koliko tema su pominjali ucesnici
matched_by_bigram %>% filter(timeframe == "time_4" | timeframe == "time_5") %>% 
        group_by(screenName) %>% count() %>% 
        ungroup() %>% arrange(desc(n))

matched_by_bigram %>% filter(timeframe == "time_4" | timeframe == "time_5")%>% 
        filter(screenName =="wieilc")

# distribucija pominjanja tema skupa
matched_by_bigram %>% filter(timeframe == "time_4" | timeframe == "time_5")%>%
        group_by(screenName) %>% count() %>% 
        ungroup() %>% arrange(desc(n)) %>% 
        ggplot(aes(1:nrow(.), n)) + geom_col()



### Moze se reci da se o temama skupa tvitovalo u izvesnoj meri i van perioda 
### skupa, odnosno te teme su generalno zanimljive ucesnicima skupa. Kada se 
### govori o raznovrsnosti tema o kojima se tvitovalo, moze se reci da je ona 
### velika, ali sveukupno komunikacija o temama skupa nije na nekom visokom 
### nivou, buduci da je 457 ucesnika u periodu skupa ukupno pomenulo teme skupa 
### 698 puta. U periodu nakon skupa, organizator je tvitovao samo o tri teme sa
### skupa, dok su sve ostale diskusije o temama skupa potekle od ucesnika skupa.