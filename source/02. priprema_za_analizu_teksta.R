### Priprema podataka za analizu teksta

library(dplyr)
library(stringr)
library(stringi)
library(tidytext)
library(tidyr)
library(widyr)

### U analizi teksta koristice se tvitovi iz tabele selected_tweets, koja sadrzi
### tvitove prikupljene za sve ucesnike koji su ispunili sledece uslove: tvitovali 
### su o skupu sa oznakama #WIELead ili @wieilc bilo kada u periodu 
### 19-26.05.2017. godine; u vreme preuzimanja tvitova imali su aktivan nalog, 
### sa dozvolom za preuzimanje tvitova (ne 'protected'); u vreme preuzimanja 
### tvitova su imali dostupne tvitove barem od 21.04.2017, sto je pocetak 
### perioda koji se analizira. Od svih prikupljenih tvitova, analiziraju se 
### tvitovi za period od 21.04.2017-23.06.217. godine.

### U analizi teksta koristice se podskup ove tabele sa samo onim kolonama koje
### su potrebne u analizi. Podskup ce svakako sadrzati id kolonu, pa ce biti
### moguce po potrebi doci i do vrednosti svih kolona za tvit.

selected_tweets <- readRDS("results/selected_tweets.RData")

tweets <- selected_tweets[, c("timeframe", "screenName", "id", "text", "isRetweet")]


### Iz teksta ce biti izbaceni linkovi i neki kodovi za specijalne znakove
### (&amp; &lt; i &gt;). Takodje, tekst je prebacen u mala slova ascii skupa

tweets <- tweets %>% 
        mutate(text = stri_trans_tolower(stri_enc_toascii(str_replace_all(text, "http[s]?[A-Za-z\\d/:.-_]+|&amp;|&lt;|&gt;", ""))))

### Po zapocinjanju analize, otkriveni su tvitovi na stranim jezicima (spanski
### predlog "de" se nasao medu najfrekventnijim recima posle izbacivanja
### (engleskih) stop reci). Medutim, ovi tvitovi ne bi trebalo da predstavljaju
### problem. Jednostavno prilikom spajanja sa kljucnim recima, nece upariti
### nijednu rec. Moglo bi biti problema kod ispitivanja najfrekventnijih reci iz
### tvitova (kao sto je bilo sa "de"). Inace, u tvitovima skinutim po kljucnim
### recima za konferenciju nema tvitova na stranim jezicima, ali skidanjem
### tvitova za svakog ucesnika, skinuti su i takvi tvitovi. Skidanjem samo
### tvitova na engleskom jeziku ne bi se dobili svi tvitovi za ucesnike za
### odredeni period, pa se ne bi moglo reci, na primer, koji procenat njihovih
### tvitova govori o konferenciji.

### Drugi problem je otkriven kada je pokusanjo izdvajanje tvitova sa 
### konferencije, odnosno onih sa oznakama #WIELead i @wieilc. Detaljnijim 
### pregledom tvitova je ustanovljeno da cak i u pocetnom skupu tvitova, koji su
### skidani upravo po ovim kljucnim recima, nemaju svi tvitovi neku od ove dve
### oznake.

### Radi se o tome da tviter skracuje tvitove koji su duzi od 140 karaktera. 
### Mada postoji format "extended tweets" koji neke elemente (pominjanja, 
### linkove, slike...) dodaje kao meta-podatke, tako da vise karaktera ostane 
### slobodno za tekst, paket twitteR pomocu kog su preuzimani tvitovi ne
### podrzava ovu opciju, niti je autor u vreme skidanja tvitova bio upoznat sa
### ovaim problemom

### Da bi se ukljucili tvitovi kao sto su, na primer:

# Next up - Stefanie Tompkins (Acting Deputy Director) DARPA speaking at the IEEE WIE International 
#       Leadership Confer… https://t.co/iK7lRJJorn
# We innovate step by step and support the advancement of #womeninengineering #proudtobeengineer 
#       proud to be #wier9… https://t.co/kvpOgfSpaY

### odluceno je da se malo prosiri uslov za ukljucivanje tako da obuhvati i samo
### pocetak ovih oznaka, kombinaciju "wie" i "ieee", kao i kombinaciju
### women...engineer... "wie" treba da bude ili u kombinaciji sa "ieee" ili da
### pocinje sa #|@, da bi se izbeglo nemacko "wie"

conf_regex <- "(?i)wielead|wieilc|[#|@]wie|women.+engineer|wie.+ieee|ieee.+wie"

table(tweets %>% filter(str_detect(text, pattern = conf_regex)) %>% select(timeframe)) 
# time_1 : time_5 => 169    241   1721    131    104 


tweets <- tweets %>% mutate(conf = ifelse(str_detect(text, pattern = conf_regex), TRUE, FALSE))

rt_regex <- "^rt @| rt @| rt: @|^rt: @|retweet* @| via @| \\(via @| thx @| ht @|^ht @| r @|^r @"
tweets <- tweets %>% 
        mutate(retweet = ifelse((isRetweet == TRUE | str_detect(text, rt_regex)), TRUE, FALSE)) %>% 
        select(text, id, screenName, timeframe, conf, retweet)

table(tweets$retweet)
# FALSE   TRUE 
# 42862 226301 

saveRDS(tweets, "results/tweets.RData") 


### Analiza programa skupa

### Za dalje analize ce biti potrebne teme skupa. Od dodatnih informacija o
### skupu, dostupan je bio program skupa u html formatu (na sajtu organizatora
### vise ne postoji ova verzija). Program se nalazi u direktorijumu "data"
### ("data/Agenda IEEE WIE International Leadership Conference.html")

### S obzirom na sadrzaj programa, koji se sastoji samo iz naslova predavanja i 
### imena predavaca, nije bilo moguce izdvajanje termina preko TF-IDF mere ili 
### izdvajanje tema preko LDA postupka za topic modelling. Kljucne reci i teme
### su identifikovani parsiranjem dokumenta i izdvajanjem termina iz naslova
### predavanja, tako da su teme predstavljene terminima onoliko precizno koliko
### su sami naslovi uspeli da ih prezentuju.

### Program skupa je u dokumentu podeljen na pet* tematskih celina, uz radionice
### i uvodna i zavrsna predavanja. Svaka od ovih pet celina ima svoju klasu;
### program je u nekoliko tabela, a predavanja su u <td> elementu sa atributom
### npr. class="TTrack1 ..."; naslovi predavanja su u okviru elementa <strong>,
### na primer:

### <td class="TTrack1 TStripe" width="14%"><strong>Robots, Disasters, and
### Refugees: How Unmanned Systems are<br> Helping</strong><br>  Robin Murphy,
### Professor, Texas A&amp;M</td>

### Radionice imaju klasu "TDinner" u okviru programa, a i dodatno su izdvojene
### u posebnu tabelu na kraju stranice; uvodna i zavrsna predavanja nemaju ovu
### oznaku klase.

### *Tematske celine su oznacene brojevima 1-5, ali ne jednoznacno, odnosno,
### track 5 je nekada "Women in...", a nekada "Entrepreneurship, track 1 -
### "Innovation" je u poslednjem bloku "Pitch Competition"; na kraju, ovih
### celina ima zapravo sedam: "Disruptive Technology", "Empowerment",
### "Entrepreneurship", "Executive Leadership", "Innovation", "Pitch
### Competition", "Women in...". Dodatne celine su "Workshops" i "Other", koja
### se odnosi na uvodna i zavrsna predavanja.

### HTML dokument je prvo podeljen na <td> elemente, u okviru kojih su izdvojeni
### <strong> elementi, odnosno naslovi predavanja, vodeci racuna da budu 
### dodeljeni odgovarajucoj celini. Na kraju su iz ovih naslova izdvojene 
### pojedinacne reci izbacivanjem stop-reci, reci kracih od tri slova, imena i 
### glagola.


### Funkcija headings_matches uzima jedan <td> element, pretrazuje da li ima
### <strong> elemenata; ako ih ima, preciscava tekst i vraca ih (kao vektor)
### pozivajucoj funkciji

headings_matches <- function(td_string){
        res <- vector()
        matches <- str_match_all(td_string, "<strong>(.+?)</strong>")[[1]]
        if (nrow(matches) > 0){
                for(i in 1:nrow(matches)){                 # sve sto je upareno: precistiti i ubaciti u vektor kljucnih reci
                        if (str_detect(matches[i,2], pattern = "^SJCC")) next      # SJCC su lokacije predavanja
                        match <- str_replace_all(matches[i, 2], pattern = "^(<[^>]+>)+", replacement = "") %>%   # tagovi na pocetku
                                str_replace_all(pattern = "&amp;", replacement = "and") %>%
                                str_replace_all(pattern = "<[^>]+>|&nbsp;", replacement = " ")
                        res <- c(res, match)
                }
        }
        return(res)
}


### Funkcija make_headings_df uzima putanju programa skupa i pravi data frame
### naslova predavanja i tematskih celina oznacenih u programu; na osnovu klasa
### <td> elemenata dodeljuje odgovarajuci naziv celine naslovima predavanja

make_headings_df <- function(prog_path){
        
        agenda <- str_c(readLines(prog_path, warn = FALSE), collapse = "")
        
        beg <- str_locate(agenda, "<table")[1,1]
        end <- str_locate(agenda, "(?i)<h2>workshops")[1,1] # u ovom delu programa su ponovljene radionice, sa drugim klasama
        ag <- str_sub(agenda, beg, end-1)   # ag sadrzi deo html-a sa programom
        
        ag_tds <- unlist(strsplit(ag, "</td>"))   # ag_tds je vektor <td> elemenata
        
        program <- data.frame()           # df kljucnih reci po naslovima trekova
        table <- vector()                 # tabela (broj treka) - (naslov), posto se asocijacije menjaju kroz dokument
        
        for(i in 1:length(ag_tds)){
                if (!str_detect(ag_tds[i], "<td")) next 
                if (str_detect(ag_tds[i], "TTrack")){     # ako ima broj treka, treba ga izvuci; da li je dat naslov treka?
                        
                        num <- str_match(ag_tds[i], "TTrack(\\d)")[1,2]
                        if (str_detect(ag_tds[i], "<strong>Track ?\\d:<br>(.+)</strong>")) {      # naslov treka? povezati sa brojem
                                table[num] <- str_match(ag_tds[i], "<strong>Track ?\\d:<br>(.+)</strong>")[1,2]
                        } else {        # ako naslov nije dat, koristi se stari naslov iz tabele, sve iz <strong> ide u rezultat
                                if(length(headings_matches(ag_tds[i])) > 0)
                                        program <- rbind(program, cbind(track = table[num], text = headings_matches(ag_tds[i])), 
                                                         stringsAsFactors = FALSE)
                        }
                        
                } else if(str_detect(ag_tds[i], "TDinner")) {       # TDinner su radionice
                        if(length(headings_matches(ag_tds[i])) > 0)
                                program <- rbind(program, cbind(track = "Workshops", text = headings_matches(ag_tds[i])), 
                                                 stringsAsFactors = FALSE)
                        
                } else {      # Uvodna i zavrsna predavanja
                        if(length(headings_matches(ag_tds[i])) > 0)
                                program <- rbind(program, cbind(track = "Other", text = headings_matches(ag_tds[i])), 
                                                 stringsAsFactors = FALSE)
                }     
        }
        return(program)
}



prog_path <- "data/Agenda IEEE WIE International Leadership Conference.html"

prog_headings <- make_headings_df(prog_path) 
prog_headings$track <- stri_trans_tolower(stri_enc_toascii(prog_headings$track))
prog_headings$text <- stri_trans_tolower(stri_enc_toascii(prog_headings$text))


### Izdvajanje reci iz naslova:

keywords <- prog_headings %>% group_by(track) %>% filter(!duplicated(text)) %>% 
        ungroup() %>% unnest_tokens(word, text, token = "words") %>% 
        anti_join(stop_words) %>%                                        # izbacivanje stop-reci
        filter(!str_detect(word, pattern = "^.{1,2}$|\\d")) %>%          # izbacivanje reci kracih od 3 slova i brojeva
        filter(!word %in% (lexicon::common_names ))                      # izbacivanje licnih imena


### Filtriranje prezimena prema listi iz paketa lexicon uparuje 27 reci, od 
### kojih su neke: leader, power, winner, bias, risk... Odluceno je da je bolje
### da ostane nekoliko prezimena, nego da sve ove reci budu izbacene.

filter(keywords, word %in% str_to_lower(lexicon::freq_last_names$Surname)) %>% distinct(word)


### Izbacivanje glagola:

conf_keywords <- keywords %>% distinct(track, word) %>% 
        left_join(lexicon::hash_grady_pos) %>% filter(!str_detect(pos, "Verb") | is.na(pos)) %>% 
        distinct(track, word)                                                       

### Izbacene su reci koje su samo glagoli, odnosno ne mogu imati neko drugo
### znacenje. Na primer, za "cancelled" postoji samo jedna kombinacija word -
### POS, ali "cloud" moze biti i imenica i glagol, a posto su sa left_join 
### napravljene sve kombinacije, kada su izbacene kombinacije rec - glagol,
### ostale su kombinacije rec - imenica, odnosno rec "cloud" je ostala u tabeli
### kljucnih reci.

### Teme imaju dosta zajednickih kljucnih reci, sto i odgovara cinjenici da se
### teme preklapaju, pogotovo kada su ovako bliske.

conf_keywords %>% group_by(word) %>% mutate(n = n()) %>% 
        pairwise_cor(track, word, n) %>% arrange(desc(correlation))


saveRDS(conf_keywords, "results/conf_keywords_df.RData")
saveRDS(prog_headings, "results/program_headings_df.RData")


### Buduci da su pojedinacne reci dosta siroke, iz naslova ce biti izdvojeni i 
### bigrami. Na primer, neke kljucne reci su "women", "scale", "steps", 
### "mindset"; ako se sve teme zajedno posmatraju kao vreca reci, moguce je 
### zamisliti tvit koji govori o mrsavljenju, a za koji bi se na osnovu ovih 
### kljucnih reci moglo reci da govori o temema skupa. Odredjivanje tema putem 
### bigrama ce biti malo precijznije, imace vise konteksta

### Naslovi se dele na bigrame, a onda se zadrzavaju samo oni u kojima nema stop
### reci, imena, brojeva i reci kracih od tri slova, sto bi trebalo da da 
### kvalitetne sintagme. collapse = FALSE sprecava spajanje svih naslova u jedan
### pre tokenizacije, odnosno ne dozvoljava da bigram bude sastavljen od 
### poslednje reci iz jednog naslova i prve reci iz sledeceg.

prog_headings$track <- stri_trans_tolower(stri_enc_toascii(prog_headings$track))
prog_headings$text <- stri_trans_tolower(stri_enc_toascii(prog_headings$text))

conf_bigrams <- prog_headings %>% 
        unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = FALSE) %>% 
        separate(bigram, c("word1", "word2"), sep = " ") %>%  
        filter_at(vars(word1,word2),          # filtriranje obe kolone/reci: izb. stop reci, imena, brojeva i kracih od 2
                  all_vars((!. %in% c(stop_words$word, lexicon::common_names)) & 
                                   (!str_detect(., pattern = "^.{1,2}$|\\d")))) 


### trigrami daju jos vecu preciznost u odredjivanju teme, ali bi zahtevanje 
### pojavljivanja bas ovako definisanih tema u tvitovima bio dosta strog kriterijum
### na primer, "emerging electronic technologies" ili "create true innovation"
### Iz ovog razloga, trigrami nisu izabrani kao predstavnici tema skupa
conf_trigrams <- prog_headings %>% unnest_tokens(trigram, text, 
                                                 token = "ngrams", n = 3, collapse = FALSE) %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        filter_at(vars(word1, word2, word3),
                  all_vars((!. %in% c(stop_words$word, lexicon::common_names)) & 
                                   (!str_detect(., pattern = "^.{1,2}$|\\d"))))

### Za izbacivanje glagola iz bigrama, napravljena je lista reci (iz recnika
### lexicon::hash_grady_pos) koje mogu da imaju samo znacenje glagola

verbs_only <- lexicon::hash_grady_pos %>% group_by(word) %>% 
        filter(all(str_detect(pos, "^Verb"))) 

### bigrami koji bi bili izbaceni ako bi se izbacili oni koji sadrze glagole:

conf_bigrams %>%
        filter_at(vars(word1, word2), any_vars(. %in% (verbs_only$word )))

### Izgleda da bi se izbacivanjem glagola izbacilo nekoliko vrlo dobrih fraza; u
### nekima je glagol u obliku glagolske imenice, kao "cognitive computing", ali 
### cak i tamo gde je u funkciji predikata, kao "creating influence", "defining 
### leadership" ili "embracing risk", fraze vrlo lepo predstavljaju teme. Jedino
### sto bi jos trebalo izbaciti iz ovih kljucnih bigrama su oni koji se odnose
### na otkazana predavanja.

conf_bigrams <- conf_bigrams %>% 
        filter_at(vars(word1, word2), all_vars(!. %in% ("cancelled")))


saveRDS(conf_bigrams, "results/conf_bigrams_df.RData")



