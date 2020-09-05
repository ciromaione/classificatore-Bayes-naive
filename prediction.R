# Title     : prediction
# Created by: Ciro Maione
# Created on: 13/08/20


# funzione per effettuare previsioni con Bayes naive a partire da un modello probabilistico
# prende in input il modello e il dataset su cui effettuare le previsioni
predict <- function (model, dataset) {

  pred <- list() # lista che conterrà le predizioni
  all_words <- row.names(model$features_prob) # parole nel modello

  for (row in dataset) {
    # parole del documento da classificare
    row_words <- unlist(strsplit(row, " "))
    pos <- model$prior$positive # inizializzazione verosimiglianza alla classe positiva a P(positive)
    neg <- model$prior$negative # inizializzazione verosimiglianza alla classe negativa a P(negative)
    for (word in all_words) {
      # se la parola è nel documento allora moltiplico alle likelihood la probabilità P(w|classe)
      # presa dal modello altrimenti moltiplico la probabilità inversa
      if (word %in% row_words) {
        if(pos!=0 && !is.na(model$features_prob[word, "positive"]))
          pos <- pos * model$features_prob[word, "positive"]
        if(neg!=0 && !is.na(model$features_prob[word, "negative"]))
          neg <- neg * model$features_prob[word, "negative"]
      }
      else {
        if(pos!=0 && !is.na(model$features_prob[word, "positive"]))
          pos <- pos *(1 - model$features_prob[word, "positive"])
        if(neg!=0 && !is.na(model$features_prob[word, "negative"]))
          neg <- neg * (1 - model$features_prob[word, "negative"])
      }
    }

    # valuto la verosimiglianza per effettuare la classificazione
    if (pos > neg) pred <- append(pred, "positive")
    else if (pos < neg) pred <- append(pred, "negative")
    else pred <- append(pred, "no_class")
  }

  return(factor(unlist(pred)))
}