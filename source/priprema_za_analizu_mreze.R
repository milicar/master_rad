library(dplyr)
library(igraph)

selected_tweets <- readRDS("results/selected_tweets.RData")

### Za analizu mreze, podaci su podeljeni na liste, tako da se svaka lista
### odnosi na jedan vremenski period.
split_tweets <- split(selected_tweets, selected_tweets$timeframe)

### Broj ucesnika u ovim vremenskim odeljcima je razlicit, sto je u redu, jer je
### uslov bio da najraniji dostupni tvit po korisniku bude 21.4. ili ranije, pa
### je tako moguce da je korisnik tvitovao 20.04. (sto nije uslo u posmatrane
### tvitove), ali nije tvitovao posle toga do konferencije; (dva korisnika koja
### se nisu pojavljivala u trecem vremenskom odeljku, sto je vreme samog
### dogadaja, na osnovu kojeg su tvitovi i prikupljani, ubacena su prethodnim
### postupkom dopune konacnog skupa tvitova).


### Funkcija za pravljenje matrice povezanosti, takode zasnovana na funkciji
### matrica_povezanosti Marka Galjaka 
### (https://github.com/gljk/Master-rad/blob/master/3.%20Listinzi%20koda.R),
### delimicno izmenjena:

make_adjacency_matrix <- function(twts) {
        partcp <-unique(twts$screenName)
        lpart <-length(partcp)
        mat <-matrix(nrow = lpart, ncol = lpart)
        colnames(mat) <- partcp
        rownames(mat) <- partcp
        for (i in 1:lpart) {
                l <-twts[twts$screenName == partcp[i], "text"]
                for (j in 1:lpart) {
                        if (j == i) next
                        k <- partcp[j]
                        mat[i, j] <-table(grepl(k, l))["TRUE"]
                }
        }
        mat[is.na(mat)] <-0
        return(mat)
}

### pravljenje liste matrica povezanosti; izvrsenje je trajalo oko 30 minuta za
### 457 ucesnika, 269163 tvita
conf_adj_mat_list <- lapply(split_tweets, make_adjacency_matrix)

### pravljenje liste grafova 
conf_graph_list <- lapply(conf_adj_mat_list, graph_from_adjacency_matrix, mode = "directed", weighted = TRUE)

### Mreze imaju razlicit broj cvorova, u zavisnosti od toga koliko je ucesnika
### tvitovalo u svakom vremenskom periodu, odnosno, period kada je skup odrzan
### sadrzi sve ucesnike/cvorove, jer se za njih posmatra mreza, a periodi pre i
### posle ukljucuju samo one ucesnike koji su tada tvitovali. Izolati u mrezama
### su ucesnici koji su tvitovali, ali nisu pominjali druge ucesnike.

conf_vnum <- sapply(conf_graph_list, function(x) length(V(x))) 
# time_1 -> time_5 : 396    430    457    427    417 
conf_iso_num <- sapply(conf_graph_list, function(x) length(V(x)[degree(x) == 0]))
# time_1 -> time_5 : 223    246     12    254    257

saveRDS(conf_graph_list, "results/list_of_conf_graphs.RData")
