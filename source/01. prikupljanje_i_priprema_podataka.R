library(twitteR)
library(dplyr)

### Originalni tvitovi su skidani u periodu 22.05-27.05.2017. funkcijom
### searchTwitter iz paketa twitteR, za kljucne reci #WIELead i @wieilc; sam
### skup je odrzan 22.05. i 23.05.2017. godine. Funkcija searchTwitter kao
### rezultat vraca listu, koja moze da se prebaci u data frame pomocu funkcije
### twListToDF iz istog paketa. startertweets je data frame sa svim tvitovima
### prikupljenim na ovaj nacin; buduci da su neki tvitovi oznaceni sa obe ove
### kljucne reci, potrebno je izbaciti duplikate:

startertweets <- readRDS("data/startertweets")

uniquesttw <- distinct(startertweets, id, .keep_all = TRUE)  


### Iz ovog skupa su izabrani ucesnici koji su tvitovali sa oznakom skupa bilo 
### kada u vremenskom periodu od tri dana pre, do tri dana posle skupa. Razlog 
### za prosirenje je ukljucivanje ucesnika koji mozda nisu tvitovali u toku 
### trajanja skupa, ali su, na primer, posle skupa tvitovali o utiscima. Ovaj 
### period se poklapa sa prosirenim periodom trajanja skupa koji je koriscen 
### prilikom analize, uz pretpostavku da period najintenzivnije komunikacije 
### pocinje nesto pre, i zavrsava nesto posle samog skupa, ali bi konkretno 
### trajanje tog perioda moglo i drugacije da se odredi.

time_narrowed <- startertweets %>% mutate("date" = as.Date(created)) %>% 
        filter(date < "2017-05-27" & date > "2017-05-18")
participants <- unique(time_narrowed$screenName) 

### Za izabrane korisnike su preuzeti dostupni tvitovi pomocu funkcija
### get_tweets i dl_tweets

### Funkcija get_tweets uzima kao argumente listu korisnika za koje se skidaju
### tvitovi, broj tvitova po korisniku, da li treba da ukljuci retvitove i da li
### treba da stampa trenutno stanje - zbog uvida u izvrsavanje funkcije.
### Korisnike za koje API vrati gresku funkcija upisuje u vektor locked_users,
### pa je moguce ponovo pokusati skidanje tvitova za ove korisnike, za slucaj da
### nije u pitanju obrisani ili 'protected' nalog, nego trenutni problem sa
### pristupom podacima

get_tweets <- function(usrs, ntweets, rts = TRUE, verbose = FALSE) {
        all_tweets <- data.frame()
        if (!exists("locked_users", where = 1)) assign("locked_users", vector(), pos = 1) # pos 1 == .GlobalEnv
        
        for (i in 1 : length(usrs)) {
                tryCatch({
                        t1<-Sys.time()
                        if (verbose) cat("In tryCatch with", i, usrs[i], "fetching user timeline... ")
                        twlist <- userTimeline(user = usrs[i], n = ntweets, includeRts = rts)
                        t2 <- Sys.time()
                        if (verbose) cat("done in", t2-t1, "\n Converting to df.... ")
                        twlist <- twListToDF(twlist)
                        t3 <- Sys.time()
                        if (verbose) cat("done in", t3-t2, "\n Binding to resulting df... ")
                        all_tweets <- rbind(all_tweets, twlist)
                        t4 <- Sys.time()
                        if (verbose) cat("done in", t4-t3, "\n\n")
                        
                }, error = function(err){
                        if (verbose) cat("In error with", i, usrs[i], "\n\n")
                        assign("locked_users", c(locked_users, usrs[i]), pos = 1)
                }) # end of tryCatch
        }
        return(all_tweets)
}

### funkcija dl_tweets vodi racuna o twitterAPI limitu i poziva funkciju
### get_tweets; vraca data frame sa svim prikupljenim tvitovima

dl_tweets <-function(user_list, num_tweets, rts = TRUE, verbose = FALSE) { 
        res <- data.frame()
        limit <- as.numeric(getCurRateLimitInfo()[41, "remaining"])
        reset <- getCurRateLimitInfo()[41, "reset"]
        if (limit <= 1)  {
                if (verbose) cat("Waiting for twitter API limit to reset, due:", as.character(reset), "UTC\n\n")	
                Sys.sleep(60 * as.numeric((reset) - Sys.time()) + 10)
        }
        if (length(user_list) <= limit) res <- rbind(res, get_tweets(user_list, num_tweets, rts, verbose))
        else {
                res <- rbind(res, get_tweets(user_list[1:(limit-1)], num_tweets, rts, verbose))
                dl_tweets(user_list[limit:length(user_list)], num_tweets, rts, verbose)
        }
        return(res)
}


### Prvo su preuzeti svi dostupni tvitovi za izabrane ucesnike, a zatim su 
### izdvojeni samo oni ucesnici za koje su dostupni tvitovi najmanje od 21.04.
### iz razloga sto neki korisnici tvituju po 3200 tvitova za nekoliko dana, tako
### da nije bilo moguće pristupiti njihovim tvitovima toliko unazad, pa njihovo
### ponašanje ne bi bilo verodostojno prikazano za ceo posmatrani period. S
### druge strane, za ucesnike za koje su dostupni svi tvitovi barem od 21.04.
### nije postavljan uslov da moraju da imaju tvitove tokom celog perioda, jer
### njihova frekvencija tvitovanja predstavlja njihovo ponasanje koje moze da se
### analizira, a dostupno je u celosti za posmatrani period. Za tako odabrane
### ucesnike, ukupno 457, izdvojeni su tvitovi samo za posmatrani period.

# all_tweets <- dl_tweets(participants, 3200)
all_tweets <- rbind(readRDS("data/all_tweets1"), readRDS("data/all_tweets2"),
                    readRDS("data/all_tweets3"), readRDS("data/all_tweets4"))


my_subset <- function(tweets, from_incl, to_incl){
        tweets <- mutate(tweets, "date" = as.Date(tweets$created))
        selected_participants <- tweets[, c("screenName", "date")] %>% group_by(screenName) %>% 
                summarize("minDate" = min(date)) %>% filter(minDate <= from_incl)
        selected_participants <- as.vector(selected_participants$screenName)
        selected_tweets <- tweets %>% filter(screenName %in% selected_participants &
                                                     date >= from_incl & date <= to_incl)
                    
}

selected_tweets <- my_subset(all_tweets, "2017-04-21", "2017-06-23")

### U toku analize je pronadjeno da je bilo gresaka pri preuzimanju tvitova - da
### li zbog tviter API-ja ili zbog same funkcije za skidanje - uglavnom, bilo je
### duplikata, ali i neki tvitovi iz originalnog skupa tvitova prikupljanih
### tokom trajanja dogadaja, nisu bili u skupu tvitova prikupljenih kasnije,
### putem userTimeline funkcije. Ovi tvitovi su takodje ukljuceni u izabrane 
### tvitove za analizu, radi sto kompletnijeg skupa tvitova (vodeci racuna da 
### budu ukljuceni samo tvitovi korisnika koji zadovoljavaju uslov dostupnosti 
### tvitova), a duplikati su izbaceni.


tws_to_insert_ids <- setdiff(time_narrowed$id, selected_tweets$id)
selected_tweets <- rbind(selected_tweets, 
                         time_narrowed[time_narrowed$id %in% tws_to_insert_ids &
                                               time_narrowed$screenName %in% selected_tweets$screenName,])

selected_tweets <- selected_tweets[!duplicated(selected_tweets$id),]

### Svi posmatrani tvitovi su podeljeni na pet grupa, po vremenu tvitovanja: 
### osnovnu grupu ("time_3") cine tvitovi iz perioda trajanja skupa uz tri dana 
### pre i posle skupa, odnosno od 19.05.2017. do 26.05.2017. (ukljucujuci i te 
### datume), kao period najintenzivnije komunikacije u vezi sa skupom; ostali 
### periodi su simetricno rasporedeni pre i posle skupa, svaki u trajanju od po 
### dve nedelje.

selected_tweets$timeframe <- 
        ifelse(selected_tweets$date >= "2017-04-21" & selected_tweets$date <= "2017-05-04", "time_1", 
               (ifelse(selected_tweets$date >= "2017-05-05" & selected_tweets$date <= "2017-05-18", "time_2",
                       (ifelse(selected_tweets$date >= "2017-05-19" & selected_tweets$date <= "2017-05-26", "time_3",
                               (ifelse(selected_tweets$date >= "2017-05-27" & selected_tweets$date <= "2017-06-09", "time_4", "time_5")))))))


### Ovo je konacan skup tvitova koji ce biti koriscen u daljim analizama
saveRDS(selected_tweets, "results/selected_tweets.RData")

