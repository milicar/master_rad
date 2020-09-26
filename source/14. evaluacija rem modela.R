

# Null deviance residual
nullresidual <- 2 * log (200*199)

# Histogrami reziduala modela

periodi <- c("Period 1", "Period 2", "Period 3", "Period 4", "Period 5")
plotovanje_reziduala <- function(model, name){
        par(mfrow=c(1,5))
        lapply(1:length(model), function(x){
                title <- paste0("Reziduali za model ",name, "\n", periodi[x])
                hist(model[[x]]$residuals,
                     main = title, col ="grey", 
                     xlab = "\nreziduali\noznacen je rezidual nultog modela",
                     ylab = "frekvencija", cex.lab = 1.3, cex.axis = 0.9)
                abline(v=nullresidual , lty=2)
        })
}

modeli <- list(fit_structural, fit_struct_conf, fit_struct_topics)
lapply(1:3, function(x){
        filename <- paste0("visuals/5_01_reziduali_modela_", x, ".png")
        png(filename, width = 1000, height = 200)
        par(mfrow=c(1,5))
        plotovanje_reziduala(modeli[[x]], x)
        dev.off()
})


# Koji procenat reziduala je manji od reziduala nultog modela?
below_null_residual <- function(model){
        lapply(model, function(x){
                mean(x$residuals < nullresidual)
        })
}

below_null_residual(fit_structural)
below_null_residual(fit_struct_conf)
below_null_residual(fit_struct_topics)

#Koliko je model"iznenadjen" ? 1-null_residual
surprised_model <- function(model){
        lapply(model, function(x){
                mean(x$residuals > nullresidual)
        })
}

surprised_model(fit_structural)
surprised_model(fit_struct_conf)
surprised_model(fit_struct_topics)



############################### random guessing equivalent 
#e^(Di/2), where Di is the model deviance residual for event i, is a “random
#guessing equivalent"

# nulti model pogađa nasumično, znači u proseku bi pogodio 1 od 39800 (200 * 199) događaja
get_random_guessing_equivalent <- function(model){
        lapply(model, function(x){
                quantile(exp(x$residuals/2))        
        })        
}

get_random_guessing_equivalent(fit_structural)

# U trećem periodu, drugi model bi pogodio dogadjaj skoro svaki put. Ali greska
# je ipak velika..
get_random_guessing_equivalent(fit_struct_conf)

get_random_guessing_equivalent(fit_struct_topics)


############################################## rang odigranog dogadjaja u predvidjenom


plot(ecdf(fit_structural$time_3$observed.rank/(200*199) ),
     xlab =" Prediction Threshold ( Fraction of Possible Events )",
     ylab =" Fraction of Observed Events Covered  ", 
     main ="Tačnost predviđanja ")
abline(v=c(0.05 ,0.1 ,0.25),lty =2)


plotovanje_tacnosti_predvidjanja <- function(model, name){
        par(mfrow=c(1,5))
        lapply(1:length(model), function(x){
                title <- paste0("Tačnost predviđanja modela ",name, "\n", periodi[x])
                plot(ecdf(model[[x]]$observed.rank/(200*199)),
                     xlab =" Prag predviđanja\n(količnik mogućih događaja)",
                     ylab =" kolicnik odigranih dogadjaja", 
                     cex.lab = 1.3, cex.axis = 0.9,
                     main = title)
                abline(v=c(0.05 ,0.1 ,0.25),lty=2)
        })}

lapply(1:length(modeli), function(x){
        filename <- paste0("visuals/5_02_tacnost_predvidjanja_model_", x, ".png")
        png(filename, width = 1000, height = 200)
        plotovanje_tacnosti_predvidjanja(modeli[[x]], x)
        dev.off()
})

########################################################## tacno predvidjanje 

predicted_any <- function(model){
        lapply(model, function(x){
                mean(apply(x$predicted.match,1,any))                
        })        
}
predicted_any(fit_structural)
predicted_any(fit_struct_conf)
predicted_any(fit_struct_topics)

predicted_all <- function(model){
        lapply(model, function(x){
                mean(apply(x$predicted.match,1,all))                
        })        
}
predicted_all(fit_structural)
predicted_all(fit_struct_conf)
predicted_all(fit_struct_topics)

fraction_correct <- function(model){
        lapply(model, function(x){
                colMeans(x$predicted.match)
        })
}

unlist(fraction_correct(fit_structural))
unlist(fraction_correct(fit_struct_conf))
fraction_correct(fit_struct_topics)


################################################ stampanje koeficijenata

summary_modela <- function(model){
        lapply(model, function(x){
                summary(x)
        })
}
summary_modela(fit_structural)
summary_modela(fit_struct_conf)
summary_modela(fit_struct_topics)
