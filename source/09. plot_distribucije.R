library(dplyr)
library(ggplot2)
library(igraph)

conf_comp_size_weak <- readRDS("results/conf_comp_size_weak.RData")
conf_comp_size_strong <- readRDS("results/conf_comp_size_strong.RData")
conf_comp_size_mutual <- readRDS("results/conf_comp_size_mutual.RData")

conf_graph_list <- readRDS("results/list_of_conf_graphs_after_1st.RData")

################################## plotovanje slabih komponenti
res1 <- tibble(Freq = as.vector(conf_comp_size_weak$time_1), 
               size = as.numeric(names(conf_comp_size_weak$time_1)), 
               time = "Period 1")
res2 <- tibble(Freq = as.vector(conf_comp_size_weak$time_2), 
               size = as.numeric(names(conf_comp_size_weak$time_2)), 
               time = "Period 2")
res3 <- tibble(Freq = as.vector(conf_comp_size_weak$time_3), 
               size = as.numeric(names(conf_comp_size_weak$time_3)), 
               time = "Period 3")
res4 <- tibble(Freq = as.vector(conf_comp_size_weak$time_4), 
               size = as.numeric(names(conf_comp_size_weak$time_4)), 
               time = "Period 4")
res5 <- tibble(Freq = as.vector(conf_comp_size_weak$time_5), 
               size = as.numeric(names(conf_comp_size_weak$time_5)), 
               time = "Period 5")

allres <- rbind(res1, res2, res3, res4, res5)

png("visuals/3_02_distribucija_slabih_komponenti.png")
ggplot(allres, aes(x = size, y = Freq)) +
        geom_point() +
        facet_wrap(~time) +
        scale_x_continuous("Veličina komponente",
                           breaks = c(1, 3, 10, 30, 100, 300),
                           trans = "log10") +
        scale_y_continuous("Frekvencija",
                           breaks = c(1, 3, 10, 30, 100, 300, 1000),
                           trans = "log10") +
        ggtitle("Distribucija slabih komponenti") +
        theme_bw() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
              axis.text=element_text(size=12,face="bold"),
              axis.title=element_text(size=14,face="bold"),
              strip.text.x = element_text(size = 14, face = "bold"),
              legend.position = "none")
dev.off()

########################################## jake komponente
res1 <- tibble(Freq = as.vector(conf_comp_size_strong$time_1), 
               size = as.numeric(names(conf_comp_size_strong$time_1)), 
               time = "Period 1")
res2 <- tibble(Freq = as.vector(conf_comp_size_strong$time_2), 
               size = as.numeric(names(conf_comp_size_strong$time_2)), 
               time = "Period 2")
res3 <- tibble(Freq = as.vector(conf_comp_size_strong$time_3), 
               size = as.numeric(names(conf_comp_size_strong$time_3)), 
               time = "Period 3")
res4 <- tibble(Freq = as.vector(conf_comp_size_strong$time_4), 
               size = as.numeric(names(conf_comp_size_strong$time_4)), 
               time = "Period 4")
res5 <- tibble(Freq = as.vector(conf_comp_size_strong$time_5), 
               size = as.numeric(names(conf_comp_size_strong$time_5)), 
               time = "Period 5")

allres <- rbind(res1, res2, res3, res4, res5)

png("visuals/3_03_distribucija_jakih_komponenti.png")
ggplot(allres, aes(x = size, y = Freq)) +
        geom_point() +
        facet_wrap(~time) +
        scale_x_continuous("Veličina komponente",
                           breaks = c(1, 3, 10, 30, 100, 300),
                           trans = "log10") +
        scale_y_continuous("Frekvencija",
                           breaks = c(1, 3, 10, 30, 100, 300, 1000),
                           trans = "log10") +
        ggtitle("Distribucija jakih komponenti") +
        theme_bw() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
              axis.text=element_text(size=12,face="bold"),
              axis.title=element_text(size=14,face="bold"),
              strip.text.x = element_text(size = 14, face = "bold"),
              legend.position = "none")
dev.off()

########################################### reciprocne komponente

res1 <- tibble(Freq = as.vector(conf_comp_size_mutual$time_1), 
               size = as.numeric(names(conf_comp_size_mutual$time_1)), 
               time = "Period 1")
res2 <- tibble(Freq = as.vector(conf_comp_size_mutual$time_2), 
               size = as.numeric(names(conf_comp_size_mutual$time_2)), 
               time = "Period 2")
res3 <- tibble(Freq = as.vector(conf_comp_size_mutual$time_3), 
               size = as.numeric(names(conf_comp_size_mutual$time_3)), 
               time = "Period 3")
res4 <- tibble(Freq = as.vector(conf_comp_size_mutual$time_4), 
               size = as.numeric(names(conf_comp_size_mutual$time_4)), 
               time = "Period 4")
res5 <- tibble(Freq = as.vector(conf_comp_size_mutual$time_5), 
               size = as.numeric(names(conf_comp_size_mutual$time_5)), 
               time = "Period 5")

allres <- rbind(res1, res2, res3, res4, res5)

png("visuals/3_04_distribucija_komponenti_reciprocne_mreze.png")
ggplot(allres, aes(x = size, y = Freq)) +
        geom_point() +
        facet_wrap(~time) +
        scale_x_continuous("Veličina komponente",
                           breaks = c(1, 3, 10, 30, 100, 300),
                           trans = "log10") +
        scale_y_continuous("Frekvencija",
                           breaks = c(1, 3, 10, 30, 100, 300, 1000),
                           trans = "log10") +
        ggtitle("Distribucija komponenti recipročne mreže") +
        theme_bw() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
              axis.text=element_text(size=12,face="bold"),
              axis.title=element_text(size=14,face="bold"),
              strip.text.x = element_text(size = 14, face = "bold"),
              legend.position = "none")
dev.off()



############################################################################
##############################################################################3

# Distribucija stepena

priprema_plotovanje_distribucije_stepena <- function(timeframe){
        graf.degrees <- degree(timeframe, mode = "all")
        graf.degrees.out <- degree(timeframe, mode = "out")
        graf.degrees.in <- degree(timeframe, mode = "in")
        
        graf1.degree.histogram <- as.data.frame(table(graf.degrees))
        graf1.degree.out.histogram <- as.data.frame(table(graf.degrees.out))
        graf1.degree.in.histogram <- as.data.frame(table(graf.degrees.in))
        
        graf1.degree.histogram[,1] <- as.numeric(paste(graf1.degree.histogram[,1]))
        graf1.degree.histogram$mode <- "ukupni stepen"
        names(graf1.degree.histogram) <- c("degree", "Freq", "mode")
        graf1.degree.out.histogram[,1] <- as.numeric(paste(graf1.degree.out.histogram[,1]))
        graf1.degree.out.histogram$mode <- "odlazni stepen"
        names(graf1.degree.out.histogram)[1] <- "degree"
        graf1.degree.in.histogram[,1] <- as.numeric(paste(graf1.degree.in.histogram[,1]))
        graf1.degree.in.histogram$mode <- "dolazni stepen"
        names(graf1.degree.in.histogram) <- c("degree", "Freq", "mode")
        
        graf1.alldists <- rbind(graf1.degree.histogram, graf1.degree.in.histogram, graf1.degree.out.histogram)
        
}

lapply(1:5, function(x){
        g <- priprema_plotovanje_distribucije_stepena(conf_graph_list[[x]])
        t <- paste0("Distribucija stepena - Period ", x)
        filename <- paste0("visuals/4_04_distribucija_stepena_Period_", x, ".png")
        png(filename)
        print(ggplot(g, aes(x = degree, y = Freq)) +
                      geom_point() +
                      facet_wrap(~mode) + 
                      scale_x_continuous("Stepen čvora",
                                         breaks = c(1, 3, 10, 30, 100, 300),
                                         trans = "log10") +
                      scale_y_continuous("Frekvencija",
                                         breaks = c(1, 3, 10, 30, 100, 300, 1000),
                                         trans = "log10") +
                      ggtitle(t) +
                      theme_bw() + 
                      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
                            axis.text=element_text(size=12,face="bold"),
                            axis.title=element_text(size=14,face="bold"),
                            strip.text.x = element_text(size = 14, face = "bold"),
                            legend.position = "none"))
        dev.off()
        
})