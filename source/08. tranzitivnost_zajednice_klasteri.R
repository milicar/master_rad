# Da li svi ucesnici ujednaceno komuniciraju ili se stvaraju grupe sa
# intenzivnijom komunikacijom?

library(dplyr)
library(igraph)
library(scales)

conf_graph_list <- readRDS("results/list_of_conf_graphs_after_1st_with_attrs.RData")

# stepen cvorova
deg_dist <- lapply(conf_graph_list, function(x) { 
        res <- list()
        res$dd_all <- degree.distribution(x, mode = "all")
        res$dd_in <- degree.distribution(x, mode = "in")
        res$dd_out <- degree.distribution(x, mode = "out")
        return(res)})

### Stampanje distribucija stepena je u skripti plot_distribucije.R


conf_v_degs <- lapply(conf_graph_list, function(x) { 
        res <- list()
        res$d_all <- degree(x, mode = "all")
        res$d_in <- degree(x, mode = "in")
        res$d_out <- degree(x, mode = "out")
        return(res)})

conf_degs_anlys <- lapply(conf_v_degs, function(x) {
        res <- list()
        res$mean <- sapply(x, mean)
        res$sd <- sapply(x, sd)
        return(res)
})

# prosek i st.dev stepena: ukupnog, odlaznog i dolaznog
conf_degs_anlys_mat <- matrix(unlist(conf_degs_anlys), ncol = 3, byrow = TRUE, dimnames = list(
        c("Period 1: prosek", "Period 1: st. dev.", "Period 2: prosek", "Period 2: st. dev.",
          "Period 3: prosek", "Period 3: st. dev.", "Period 4: prosek", "Period 4: st. dev.",
          "Period 5: prosek", "Period 5: st. dev."), 
        c("ukupni stepen", "dolazni stepen", "odlazni stepen")
))

max_degree <- max(degree(conf_graph_list$time_3))
degree(conf_graph_list$time_3)[degree(conf_graph_list$time_3) == max_degree]
# najveci stepen ima organizator

# prikaz mreze sa informacijom o ukupnom stepenu
# uz vec ranije naznacen broj tvitova o skupu u toku skupa

yellowred <- rev(rainbow(26, start = 0, end = 1/6)) 

lapply(1:length(conf_graph_list), function(y) { 
        filename <- paste0("visuals/4_01_mreza_ucesnika_sa_stepenom_za_period_", y, ".png")
        png(filename, width = 860, height = 860)
        x=conf_graph_list[[y]]
        t = paste0("Mreža učesnika skupa u periodu ", x$title)
        plot(x, layout = layout_with_fr(x), frame = TRUE, margin = c(0.2, 0, 0, 0),
             vertex.label = NA, 
             vertex.size = rescale(degree(x, mode = "all"), to = c(2, 10)), 
             vertex.color = yellowred[round(rescale(log(V(x)$nconftws), to = c(0,26)))],
             edge.arrow.size = 0.15, edge.color = alpha("black", rescale(log(E(x)$weight), to = c(0.3, 1))))
        title(t, cex.main = 2)
        legend("bottomright", ncol = 2, title = "broj tvitova o skupu i ukupni stepen", legend = c("veliki broj tvitova", "mali broj tvitova", "visok stepen", "nizak stepen"), 
               cex = 1.5, 
               pt.cex = c(2, 2,3.5, 2), col = "black", pch = 21, pt.bg = c(yellowred[26], yellowred[1], NA, NA))
        dev.off() 
}
)

### Koliko je tacno mreza centralizovana? Buduci da se ispituje pozicija ili moc
### ucesnika komunikacije, prilikom izracunavanja ovih metrika nisu uzeti u
### obzir oni ucesnici koji nisu komunicirali sa drugima, odnosno izbaceni su
### izolati iz grafa. Ukoliko bi se ispostavilo da je mreza u velikoj meri 
### centralizovana, onda bi to moglo da objasni brzo nestajanje mreze posle 
### skupa, kada oni cvorovi koji su drzali mrezu "na okupu" izgube svoj uticaj.


conf_no_iso_graph_list <- lapply(conf_graph_list, function(x) {res <- delete_vertices(x, V(x)[degree(x) == 0])} )

conf_centralizations_matrix <- matrix( data = c(
        sapply(conf_no_iso_graph_list, function(x) centr_betw(x, directed = TRUE)$centralization),
        sapply(conf_no_iso_graph_list, function(x) centr_betw(x, directed = FALSE)$centralization),
        sapply(conf_no_iso_graph_list, function(x) centr_clo(x, mode = "in")$centralization),
        sapply(conf_no_iso_graph_list, function(x) centr_clo(x, mode = "out")$centralization),
        sapply(conf_no_iso_graph_list, function(x) centr_clo(x, mode = "all")$centralization),
        sapply(conf_no_iso_graph_list, function(x) centr_degree(x, mode = "in")$centralization),
        sapply(conf_no_iso_graph_list, function(x) centr_degree(x, mode = "out")$centralization),
        sapply(conf_no_iso_graph_list, function(x) centr_degree(x, mode = "all")$centralization),
        sapply(conf_no_iso_graph_list, function(x) centr_eigen(x, directed = TRUE)$centralization),
        sapply(conf_no_iso_graph_list, function(x) centr_eigen(x, directed = FALSE)$centralization)),
        nrow = 10, ncol = 5, byrow = TRUE, dimnames = list( c("betweenness_centrality_directed","betweenness_centrality_undirected", 
                                                              "closeness_centrality_in", "closeness_centrality_out", "closeness_centrality_all", 
                                                              "degree_centrality_in", "degree_centrality_out", "degree_centrality_all",
                                                              "eigenvector_centrality_directed", "eigenvector_centrality_undirected"),
                                                            c("time_1", "time_2", "time_3", "time_4", "time_5"))
)

conf_centralizations_matrix

### Centralizacije se racunaju normalizovane prema maksimalnoj vrednosti za 
### mrezu date velicine; za mere intermedijarnosti, stepena i bliskosti 
### maksimalnu centralizaciju ima mreza oblika zvezde, dok je za meru 
### svojstvenog vektora to mreza sa samo jednom vezom i izolatima. Neke mere 
### centralizacije za mrezu u toku odrzavanja skupa su dosta visoke. 
### Centralizacija po intermedijarnosti (0.25) ukazuje na to da mozda postoje 
### cvorovi koji "kontrolisu" komunikaciju u mrezi, ali ova vrednost nije 
### previsoka. Centralizacija po bliskosti je srednje visoka ako posmatramo sve
### veze bez obzira na smer (0.64), sto bi znacilo da postoji jedan broj cvorova
### koji je blizi ostalim cvorovima u sveukupnoj komunikaciji. Centralizacija po
### dolaznom stepenu je srednje visoka (0.55), sto ukazuje na postojanje jednog 
### broja cvorova sa visokim dolaznim stepenom, a to su cvorovi sa velikim 
### uticajem (najverovatnije organizatori skupa). Centralizacija po svojstvenom 
### vektoru je izrazito visoka (0.96), i to u svim posmatranim periodima, sa 
### minimalnim razlikama za usmerene i neusmerene mreze. Ovo moze da znaci da 
### veoma uticajni cvorovi intenzivno komniciraju medusobno i na taj nacin 
### dobijaju visoku centralnost svojstvenog vektora, dok su manje uticajni 
### cvorovi iskljuceni iz komunikacije.


### Jasno je da komunikacija nije ujednacena u celoj mrezi. Da li se stvaraju
### manje grupe sa intenzivnijom komunikacijom? Na nivou mreze moze da se
### izracuna koeficijent klasterovanja, koji predstavlja odnos trouglova i 
### trijada u mrezi; funkcija transitivity u igraph paketu racuna ovu metriku ne 
### uzimajuci u obzir smer i tezinu veza. Izolati takode ne uticu na rezultat. 

### koeficijent klasterovanja

conf_transitivity <- sapply(conf_graph_list, transitivity, type = "global", isolates = NaN)
# time_1 -> time_5 : 0.20504475 0.15757879 0.06979682 0.16435882 0.21428571 

### Ovo je vrlo mala vrednost za period odrzavanja skupa i upucuje na strukturu
### bez mnogo klika ili jezgara. Dalje bi se mogle istraziti razlicite metrike
### koje upucuju na strukturu mreze, kao sto su otkrivanje grupa i klastera.

### Funkcije za nalazenje klika u igraph paketu tretiraju usmerene mreze kao 
### neusmerene, uz poruku upozorenja - zbog toga je koriscna neusmerena verzija
### mreza, sa vezom za svaki par uzajamnih veza. Ovde vise nije od interesa puka
### povezanost, vec uzajamna komunikacija, koja ima potencijala da opstane i
### posle skupa. Za izracunavanje klika je takode zgodno posmatrati mrezu bez
### izolata.


conf_no_iso_mutual <- lapply(conf_no_iso_graph_list, as.undirected, mode = "mutual")

conf_largest_clique <- sapply(conf_no_iso_mutual, clique_num) 
# velicina najvece klike (sa najvecim brojem clanova)
# time_1 -> time_5 :  3      3      4      3      3 

conf_largest_cliques_list <- lapply(conf_no_iso_mutual, largest_cliques) # lista najvecih klika
conf_n_largest_cliques <- sapply(conf_largest_cliques_list, length) # broj najvecih klika
# time_1 -> time_5 :  2      4      3      2      2

conf_max_cliques_list <- lapply(conf_no_iso_mutual, max_cliques, min = 3) # lista maksimalnih klika (koje nisu podgraf vece klike)
conf_n_max_cliques <- sapply(conf_no_iso_mutual, count_max_cliques, min = 3) # broj maksimalnih klika (od barem 3 cvora)
# time_1 -> time_5 :  2      4     49      2      2

### Klika je sama po sebi dosta restriktivna mera, a uz ovako definisane veze u
### mrezi, velicina najvece klike od cetiri clana mozda i nije tako losa. Isto
### vazi i za broj maksimalnih klika u periodu odrzavanja skupa.

conf_coreness <- lapply(conf_no_iso_mutual, coreness) 
# za svaki cvor, k jezgra kome pripada (stepen, ne broj clanova)
conf_coreness_freqs <- lapply(conf_coreness, table) 
# frekvencije coreness vrednosti cvorova
conf_coreness_max <- sapply(conf_coreness_freqs, function(x) max(as.numeric(names(x)))) 
# najveca coreness vrednost po mrezi
# time_1 -> time_5 :   2      2      3      2      2 

### Vrednosti ove metrike odgovaraju rezultatima za najvece klike u pogledu
### stepena cvorova u grupama, odnosno, nesto blazi kriterijumi povezanosti
### cvorova ne govore o tome da ima vecih grupa koje su cvrsto medusobno
### povezane, ali ne toliko da su klike. Sto se tice broja jezgara, igraph ne
### odreduje pripadnost odredenog cvora konkretnim jezgrima.

conf_coreness_freqs
conf_cliques_freqs <- lapply(conf_max_cliques_list, function(x){
        table(sapply(x, length))
})



# klasterizacija algoritmom edge-betweenness
conf_clust_eb <- lapply(conf_no_iso_graph_list, cluster_edge_betweenness)
sapply(conf_clust_eb, function(x) max(x$membership))
# time_1 -> time_5 :  75     25     45     62     51 


# klasterizacija algoritmom walktrap
conf_clust_wt <- lapply(conf_no_iso_graph_list, cluster_walktrap)
sapply(conf_clust_wt, function(x) max(x$membership))
# time_1 -> time_5 :  37     34     32     27     32 

### Klasterizacija daje veliki broj klastera, sto bi moglo da ukazuje na
### izuzetnu podeljenost mreze.

my_rainbow_wt <- rainbow(sapply(conf_clust_wt, function(x) max(x$membership)), start = 0, end = 1)
length(my_rainbow_wt)

for(i in 1:length(conf_no_iso_graph_list)){
        for(j in 1:length(V(conf_no_iso_graph_list[[i]]))) { 
                V(conf_no_iso_graph_list[[i]])[j]$clustwt <- conf_clust_wt[[i]]$membership[j]
        }
}

lapply(1:length(conf_no_iso_graph_list), function(x) { 
        filename <- paste0("visuals/4_02_klasteri_u_periodu_", x, ".png")
        g <- conf_no_iso_graph_list[[x]]
        t <- paste("walktrap clustering, broj klastera: ", max(V(g)$clustwt), "\nperiod", g$title)
        png(filename, width = 860, height = 860)
        important <- names(sort(degree(g), decreasing = TRUE)[1:5])
        plot(g, layout = layout_with_fr(g), frame = TRUE,
             vertex.label = ifelse(V(g)$name %in% important, V(g)$name, NA),
             vertex.label.cex = 1.5, vertex.label.color = "black", vertex.label.font = 2, 
             vertex.size = rescale(degree(g, mode = "all"), to = c(2, 10)), 
             vertex.color = my_rainbow_wt[V(g)$clustwt], edge.arrow.size = 0.15,
             edge.color = alpha("black", rescale(log(E(g)$weight), to = c(0.3, 1))))
        title(t, cex.main = 2)
        dev.off()
})

### Iz grafickog prikaza se vidi da je veliki broj klastera u periodima pre i
### posle skupa jednim delom uslovljen brojem komponenti, ali u vreme trajanja
### skupa to nije objasnjenje. Ovakav raspored cvorova ne daje lep pregled tako
### gusto prepletenih klastera i, mada postoje bolja resenja za prikaz klastera,
### bice pokusano ipak nesto drugo: ova mreza u vreme trajanja skupa izgleda kao
### ego-mreza jednog cvora, organizatora skupa, prema cijem nalogu je i vrsena
### pocetna pretragu, pa bi se moglo pokusati sa analizom mreze bez njega.

degree(conf_no_iso_graph_list[[3]], V(conf_no_iso_graph_list[[3]])$name == "wieilc", mode = "all")
# => 343
length(V(conf_no_iso_graph_list[[3]]))
# => 445

conf_no_wieilc <- delete_vertices(conf_no_iso_graph_list[[3]], V(conf_no_iso_graph_list[[3]])$name == "wieilc")

### Kada se 'izbaci' organizator, jos 34 cvora ostaju izolati, odnosno, to su
### ucesnici koji su komunicirali samo sa organizatorom skupa
length(V(conf_no_wieilc)[degree(conf_no_wieilc) == 0])
# => 34
conf_no_wieilc_no_iso <- delete_vertices(conf_no_wieilc, V(conf_no_wieilc)[degree(conf_no_wieilc) == 0])


# klasterizacija kada nema centralnog čvora
conf_clust_wt_no_wieilc <- cluster_walktrap(conf_no_wieilc_no_iso)
max(conf_clust_wt_no_wieilc$membership)
# => 46
conf_clust_eb_no_wieilc <- cluster_edge_betweenness(conf_no_wieilc_no_iso)
max(conf_clust_wt_no_wieilc$membership)
# 46

for(i in 1:length(V(conf_no_wieilc))) V(conf_no_wieilc)[i]$clustwt_nowieilc <- conf_clust_wt_no_wieilc$membership[i]

my_rainbow_wt80 <- rainbow(80, start = , end = 1)


important2 <- names(sort(degree(conf_no_wieilc), decreasing = TRUE)[1:5])
png("visuals/4_03_klasteri_bez_organizatora_period_skupa.png", width = 860, height = 860)
plot(conf_no_wieilc, layout = layout_with_fr(conf_no_wieilc), frame = TRUE, 
     main = "Mreža bez organizatora u periodu skupa\nbroj klastera: 80", cex.main = 2,
     vertex.label = ifelse(V(conf_no_wieilc)$name %in% important2, V(conf_no_wieilc)$name, NA),
     vertex.label.cex = 1.5, vertex.label.color = "black", vertex.label.font = 2,
     vertex.size = rescale(degree(conf_no_wieilc, mode = "all"), to = c(2, 10)), 
     vertex.color = my_rainbow_wt80[V(conf_no_wieilc)$clustwt_nowieilc], edge.arrow.size = 0.15,
     edge.color = alpha("black", rescale(log(E(conf_no_wieilc)$weight), to = c(0.3, 1))))
dev.off()


### Posle uklanjanja glavnog organizatora iz mreze, ostalo je 34 izolata (u 
### pocetnoj mrezi ih je bilo jos 12) ali se mreza nije raspala na komponente, 
### sto potvrduje neku srednju centralizovanost (ima vise srednje vaznih 
### cvorova). Ipak bez ovog cvora mreza je, izuzimajuci izolate, podeljena na 46
### klastera, u odnosu na 32 (za isti period i isti algoritam za celu mrezu),
### sto pokazuje znacaj tog cvora za povezivanje grafa. Osim toga, u
### vizuelizaciji se primecuje drvolika struktura, bez znacajnog prisustva
### klika. Zakljucak je da komunikacija u mrezi nije ujednacena, o cemu govore
### distribucija stepena i srednja, ali primetna centralizovanost mreze (mali
### broj uticajnih cvorova), nizak koeficijent klasterovanja i relativno mali
### broj klika i velicina jezgara (slaba medusobna povezanost "ne-centralnih"
### cvorova) i veliki broj klastera (velika heterogenost mreze).

saveRDS(conf_no_iso_graph_list, "results/list_of_conf_graphs_no_isolates_after_2nd.RData")
saveRDS(conf_no_wieilc, "results/conf_graph3_no_wieilc_after_2nd.RData")
