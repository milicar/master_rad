library(dplyr)
library(igraph)
library(scales)

# Da li su učesnici komunicirali međusobno i pre skupa, ili ostvarena
# komunikacija predstavlja nove ili obnovljene kontakte?

conf_graph_list <- readRDS("results/list_of_conf_graphs.RData")

# Pre svega, broj ucesnika u komunikaciji je najveci u toku skupa,
conf_vnum <- sapply(conf_graph_list, function(x) length(V(x))) 
# time_1 -> time_5 : 396    430    457    427    417 

# povezanost
conf_connected <- sapply(conf_graph_list, is.connected)
conf_connected  # time_1 -> time_5 : FALSE

# gustina
conf_density <- sapply(conf_graph_list, edge_density)
conf_density   # time_1 -> time_5 : 0.001732515 0.001642544 0.006622135 0.001544788 0.001435390 

# Broj izolata je najmanji u toku skupa, a posle skupa je veci i nego sto je
# bio; to su oni koji nisu komunicirali sa drugima
conf_iso_num <- sapply(conf_graph_list, function(x) length(V(x)[degree(x) == 0]))
# time_1 -> time_5 : 223    246     12    254    257

#slabe komponente
conf_components_weak <- lapply(conf_graph_list, components, mode = "weak")

conf_comp_no_weak <- sapply(conf_components_weak, "[[", "no") # broj komponenti
conf_comp_no_weak  #  time_1 -> time_5 : 243    262     13    265    275 

conf_comp_max_weak <- sapply(conf_components_weak, function(x) { max(x$csize) }) # najveca komponenta
conf_comp_max_weak  # time_1 -> time_5 : 120    142    445    140    105 

conf_comp_max_weak_ratio <- conf_comp_max_weak *100/ length(V(conf_graph_list[[3]])) # procenat svih ucesnika u komp.
conf_comp_max_weak_ratio # => time_1 -> time_5 : 0.2625821 0.3107221 0.9737418 0.3063457 0.2297593 


### Ocekivano, broj komponenti je najmanji za vreme trajanja skupa, i tada je
### maksimalna komponenta najveca, obuhvata cak 97% ucesnika (u odnosu na 31%
### pre skupa), sto znaci da su gotovo svi ucesnici pomenuli barem nekog od
### drugih ucesnika. Ovo potvrduje rezultate o gustini mreze:
cor(conf_density, conf_comp_max_weak) # => [1] 0.9951469
cor(conf_density, conf_comp_no_weak) # => [1] -0.9982268

### Medutim, imajuci na umu retvitove, vredi pogledati sta se desava sa jakim
### komponentama, koje bi predstavljale intenzivniju komunikaciju (ne nuzno
### dvosmernu - cvorovi se smatraju povezanima ako postoji bar po jedna usmerena
### putanja u oba smera, ali ne obavezno duzine 1), ili, jos restriktivnije,
### svesti graf na neusmeren, zadrzavajuci samo reciprocne veze - dvosmernu
### komunikaciju.
conf_components_strong <- lapply(conf_graph_list, components, mode = "strong")

conf_comp_no_strong <- sapply(conf_components_strong, "[[", "no")
conf_comp_no_strong  # time_1 -> time_5 : 358    388    324    397    388 

conf_comp_max_strong <- sapply(conf_components_strong, function(x) { max(x$csize) })
conf_comp_max_strong  # time_1 -> time_5 :  10     22    132     21      9 

conf_comp_max_strong_ratio <- conf_comp_max_strong *100 / length(V(conf_graph_list[[3]]))
conf_comp_max_strong_ratio # time_1 -> time_5 : 0.02188184 0.04814004 0.28884026 0.04595186 0.01969365 

### Za jake komponente rezultati su donekle drugaciji - broj komponenti u vreme
### skupa je veliki, a maksimalna komponenta obuhvata 29% ucesnika (naspram 4.8%
### i 2.1% pre skupa). Rezultati za maksimalnu komponentu i dalje potvrduju
### rezultate o gustini:
cor(conf_density, conf_comp_max_strong) # => [1] 0.992854
cor(conf_density, conf_comp_no_strong) # => [1] -0.8882035


### Sama maksimalna komponenta ne govori o broju manjih komponenti koje bi mogle
### biti prostori intenzivnije komunikacije (mada veliki broj komponenti moze da
### uputi na odgovor). Jasniju sliku moze dati pregled po velicinama komponenti:

conf_comp_size_weak <- lapply(conf_components_weak, function(x) { table(x$csize) })
conf_comp_size_strong <- lapply(conf_components_strong, function(x) { table(x$csize) })
saveRDS(conf_comp_size_weak, "results/conf_comp_size_weak.RData")
saveRDS(conf_comp_size_strong, "results/conf_comp_size_strong.RData")

### kod za plotovanje distribucija komponenti je u skripti plot_distribucije.R

### Za istrazivanje dvosmerne komunikacije, graf cu pretvoriti u neusmereni,
### zadrzavajuci po jednu vezu za dve uzajamne:
conf_mutual_graph_list <- lapply(conf_graph_list, as.undirected, mode = "mutual")

conf_density_mutual <- sapply(conf_mutual_graph_list, edge_density)
conf_density_mutual  # time_1 -> time_5 : 0.0004858714 0.0004336749 0.0016891243 0.0003518378 0.0003228187 

conf_components_mutual <- lapply(conf_mutual_graph_list, components)

conf_comp_no_mutual <- sapply(conf_components_mutual, "[[", "no")
conf_comp_no_mutual # time_1 -> time_5 : 360    395    340    398    391 

conf_comp_max_mutual <- sapply(conf_components_mutual, function(x) { max(x$csize) })
conf_comp_max_mutual # time_1 -> time_5 :    10     13    114     21      4 
conf_comp_max_mutual_ratio <- conf_comp_max_mutual * 100/ length(V(conf_graph_list[[3]]))
conf_comp_max_mutual_ratio  # time_1 -> time_5 : 0.021881838 0.028446389 0.249452954 0.045951860 0.008752735



# distribucija velicina komponenti
conf_comp_size_mutual <- lapply(conf_components_mutual, function(x) { table(x$csize) })
saveRDS(conf_comp_size_mutual, "results/conf_comp_size_mutual.RData")

### kod za plotovanje distribucije komponenti je u skripti plot_distribucije.R


# dijametar i prosecna putanja


conf_avg_path_directed <- sapply(conf_graph_list, mean_distance, directed = TRUE, unconnected = TRUE)
# time_1 -> time_5 : 2.438159 3.399861 2.684841 2.831637 3.247514 
conf_avg_path_undirected <- sapply(conf_graph_list, mean_distance, directed = FALSE, unconnected = TRUE)
# time_1 -> time_5 : 4.946441 5.008145 2.655157 4.594015 4.862088 

conf_diam_directed <- sapply(conf_graph_list, diameter, directed = TRUE, unconnected = TRUE, weights = NA)
# time_1 -> time_5 :  8      9      6      8      9 
conf_diam_undirected <- sapply(conf_graph_list, diameter, directed = FALSE, unconnected = TRUE, weights = NA)
# time_1 -> time_5 :  11     14      6     11     10 



### Nevezano za odgovor na postavljeno pitanje, u prikaz mogu, od do sada
### dostupnih podataka, da ukljucim jos i tezinu ivica, a mogu da dodam kao
### atribut cvorova broj tvitova koje svaki ucesnik ima u pocetnom skupu tvitova
### (obelezenih sa #WIELead ili @wieilc). Na taj nacin moze da se prati gde se
### nalaze ucesnici koji su najvise tvitovali sa ovim kljucnim recima.

startertweets <- readRDS("data/startertweets")

nstarttw <- startertweets %>% distinct(id, .keep_all = TRUE) %>%
        filter(screenName %in% names(V(conf_graph_list[[3]]))) %>% count(screenName)


for(i in 1:length(conf_graph_list)){
        for(j in 1:length(V(conf_graph_list[[i]]))) { 
                V(conf_graph_list[[i]])[j]$nstarttw <- as.numeric(nstarttw[nstarttw$screenName == 
                                                                                   V(conf_graph_list[[i]])[j]$name, "n"])}
}

### Boje za kodiranje cvorova, od zute do crvene; atribut nstarttw ce, posle preracunavanja, da odredi indeks 
### boje - svetlozuta za najmanji, crvena za najveci broj tvitova
yellowred <- rev(rainbow(26, start = 0, end = 1/6)) 


pdf("visuals/1_num_keywords.pdf")
lapply(conf_graph_list, function(x) { plot(x, layout = layout_with_fr(x), main = x$title, frame = TRUE,
                                           sub = "users with number of tweets originally collected (with #WIELead or @wieilc)",
                                           vertex.label = NA, vertex.size = 2, vertex.color = yellowred[round(rescale(log(V(x)$nstarttw), to = c(0,26)))],
                                           edge.arrow.size = 0.15, edge.color = alpha("black", rescale(log(E(x)$weight), to = c(0.3, 1))))
        legend("bottomright", title = "users with n of tweets", legend = c("high n", "low n"), cex = 0.8, 
               pt.cex = 1, col = "black", pch = 21, pt.bg = c(yellowred[26], yellowred[1]))})
dev.off()


### Iako donekle prati logicki rezon da ce korisnici sa najvise tvitova u vreme
### skupa biti centralniji, a korisnici sa manje tvitova na periferiji, primetna
### su i neka odstupanja. Cvorovi crvene boje na periferiji grafa bi mogli da se
### objasne kao korisnici koji su dosta tvitovali koristeci 'hesteg' skupa, ali
### nisu pominjali druge ucesnike direktno, dok bi zuti cvorovi blizu centra
### grafa mogli da budu korinici koji su dosta komunicirali sa drugim 
### korisnicima, ali o nekim drugim temama. Nesto od ovoga ce se mozda
### razjasniti kasnijom analizom.

saveRDS(conf_graph_list, "results/list_of_conf_graphs_after_1st_with_attrs.RData")
