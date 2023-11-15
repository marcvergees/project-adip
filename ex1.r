# Resoldrem el següent càlcul: curtosi
# Prenem la funció 1/sqrt(|x|)


mitjana<-function(x){                                   # M'he creat una sèrie de funcions auxiliars que he 
    return(sum(x)/length(x))                            # necessitat per fer els càlculs corresponents.
}

variancia<-function(x,mitjana){
    aux<-(x-mitjana)^2
    return(1/length(x)*sum(aux))
}

desv_tip<-function(x,mitjana){
    variancia<-variancia(x,mitjana)
    print(variancia)
    return(sqrt(variancia))
}

curtosi<-function(){
    x_neg<-seq(-5,-0.01,by=0.1)                         # Subdividim en negatives i positives per evitar el 0
    x_pos<-seq(0.01,5,by=0.1)
    x<-c(x_neg,x_pos)                                   # Ajuntem totes les x
    f_x<-1/sqrt(abs(x))                                 # Calculem les seves imatges i fem l'estudi pertinent
    mitjana<-mitjana(f_x)                               # Trobem la mitjana a través de la nostra funció
    desv_tipica<-desv_tip(f_x,mitjana)                  # Ídem per la desviació típica
    aux<-(f_x-mitjana)^4                                # Ara ja estem preparats per trobar la curtosi
    plot(x,f_x,"p")
    return((1/length(f_x)*sum(aux))/desv_tipica^4-3)    # I aquí va el càlcul de la curtosi
}

print(curtosi())