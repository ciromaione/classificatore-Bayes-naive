# Title     : cleaning
# Created by: Ciro Maione
# Created on: 13/08/20


# prende in input il dataset su cui effettuare la pulizia
# restituisce il dataset pulito
clean_ds <- function (dataset) {
  # rende tutti i caratteri minuscoli
  new <- tolower(dataset)

  # rimuove le url
  new <- gsub("http.*?(\\s|$)", " ", new)

  # rimuove i tag (#...) e gli username (@...)
  new <- gsub("[@#].*?(\\s|$)", " ", new)

  # rimuove gli apostrofi
  new <- gsub("'", " ", new)

  # rimuove i numeri
  new <- removeNumbers(new)

  # rimuove i segni di punteggiatura
  new <- removePunctuation(new)

  # rimuove le stopwords
  new <- removeWords(new, stopwords("it"))

  # rimuove gli spazi ripetuti
  new <- stripWhitespace(new)

  # stemming
  new <- stemDocument(new, language = "italian")

  return(new)
}