mitjana<-function(x){                                   # M'he creat una sèrie de funcions auxiliars que he 
    return(sum(x)/length(x))                            # necessitat per fer els càlculs corresponents.
}

covariancia<-function(x,y){
    aux<-(x-mitjana(x))*(y-mitjana(y))
    # print(aux)
    return(1/length(x)*sum(aux))
}

variancia<-function(x,mitjana){
    aux<-(x-mitjana)^2
    return(1/length(x)*sum(aux))
}

desv_tip<-function(x,mitjana){
    variancia<-variancia(x,mitjana)
    return(sqrt(variancia))
}

corr_pearson<-function(x,y){
    mitjana_x<-mitjana(x)
    mitjana_y<-mitjana(y)
    desv_tip_x<-desv_tip(x,mitjana_x)
    desv_tip_y<-desv_tip(y,mitjana_y)
    cov_xy<-covariancia(x,y)
    correlacio<-cov_xy/(desv_tip_x * desv_tip_y)
    return(correlacio)
}

fitxer<-read.table("iris.txt",header=TRUE,sep=",")    # Carreguem les dades
sepal_length<-fitxer[[1]]
sepal_width<-fitxer[[2]]
petal_length<-fitxer[[3]]
petal_width<-fitxer[[4]]
class<-fitxer[[5]]

# Estadística univariant
# Utilitzarem sepal_length com a variable per fer l'estudi univariant
cat("Mitjana",mitjana(sepal_length),"\n")
cat("Variància",variancia(sepal_length,mitjana(sepal_length)),"\n")
cat("Desviació típica",desv_tip(sepal_length,mitjana(sepal_length)),"\n")
print(summary(sepal_length))

# stem(sepal_length)
# hist(sepal_length)
boxplot(sepal_length)

# Estadística bivariant
# Observem com estan correlaciones les variables
cat("Correlació sepal-length/sepal-width",corr_pearson(sepal_length,sepal_width),"\n")
cat("Correlació sepal-length/petal-length",corr_pearson(sepal_length,petal_length),"\n")
cat("Correlació sepal-length/petal-width",corr_pearson(sepal_length,petal_width),"\n")
cat("Correlació sepal-width/petal-length",corr_pearson(sepal_width,petal_length),"\n")
cat("Correlació sepal-width/petal-width",corr_pearson(sepal_width,petal_width),"\n")
cat("Correlació petal-length/petal-width",corr_pearson(petal_length,petal_width),"\n")
# Notem que n'hi ha 3 que tenen una forta correlacio

# Trobem les rectes de regressió i veiem com s'adequen
line1<-lm(petal_width~petal_length)
plot(petal_length,petal_width)
abline(line1)
print(line1)