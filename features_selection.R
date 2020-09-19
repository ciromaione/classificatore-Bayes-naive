# Title     : features_selection
# Created by: Ciro Maione
# Created on: 13/08/20

# questa funzione costruisce la DocumentTermMatrix da usare per il training
# eseguendo una ridusione delle features basata sulla frequenza delle parole
# prende in input il la frequenza minima delle parole del dizionario ed il dataset
ds_dtm_by_frequency <- function (dataset, n_features) {
  # divisione dei documenti in positivi e negativi
  pos <- dataset[dataset$class == "positive", "text"]
  neg <- dataset[dataset$class == "negative", "text"]

  Npos <- length(pos) # numero di documenti positivi
  Nneg <- length(neg) # numero di documenti negativi

  # i documenti negativi vengono aggiunti alla fine di quelli positivi
  docs <- append(pos, neg)

  ## costruzione della matrice escludendo i termini che compaiono meno di min_freq volte
  # ds_dtm <- DocumentTermMatrix(Corpus(VectorSource(docs)), control = list(bounds = list(global = c(min_freq, Inf))))

  # costruzione del dizionario di termini da includere nella matrice
  tmp <- DocumentTermMatrix(Corpus(VectorSource(docs)))
  freq <- colSums(as.matrix(tmp))
  high.freq <- tail(sort(freq), n = n_features)
  hfp.df <- as.data.frame(sort(high.freq))
  dictionary <- rownames(hfp.df)

  # costruzione della matrice con dizionario dictionary
  ds_dtm <- DocumentTermMatrix(Corpus(VectorSource(docs)), control = list(dictionary=dictionary))

  # restituisce la matrice partizionata in documenti positivi e negativi
  return(list(pos=ds_dtm[1:Npos, ], neg=ds_dtm[(Npos+1):(Npos+Nneg), ]))
}


# questa funzione costruisce la DocumentTermMatrix da usare per il training
# eseguendo una riduzione delle features basata sul metodo if-idf
# prende in input il numero di features da selezionare ed il dataset
ds_dtm_by_tfidf <- function (dataset, n_features) {
  # divisione dei documenti in positivi e negativi
  pos <- dataset[dataset$class == "positive", "text"]
  neg <- dataset[dataset$class == "negative", "text"]

  Npos <- length(pos) # numero di documenti positivi
  Nneg <- length(neg) # numero di documenti negativi

  # i documenti negativi vengono aggiunti alla fine di quelli positivi
  docs <- append(pos, neg)

  # costruzione del dizionario di termini da includere nella matrice
  tmp <- DocumentTermMatrix(Corpus(VectorSource(docs)), control = list(weighting = weightTfIdf))
  freq <- colSums(as.matrix(tmp))
  high.freq <- tail(sort(freq), n = n_features)
  hfp.df <- as.data.frame(sort(high.freq))
  dictionary <- rownames(hfp.df)

  # costruzione della matrice con dizionario dictionary
  ds_dtm <- DocumentTermMatrix(Corpus(VectorSource(docs)), control = list(dictionary=dictionary))

  # restituisce la matrice partizionata in documenti positivi e negativi
  return(list(pos=ds_dtm[1:Npos, ], neg=ds_dtm[(Npos+1):(Npos+Nneg), ]))
}


# questa funzione costruisce la DocumentTermMatrix da usare per il training
# eseguendo una riduzione delle features basata sul metodo mutual information
# prende in input il numero di features da selezionare ed il dataset
ds_dtm_by_mutual_information <- function (dataset, n_features) {
  # divisione dei documenti in positivi e negativi
  pos <- dataset[dataset$class == "positive", "text"]
  neg <- dataset[dataset$class == "negative", "text"]

  Npos <- length(pos) # numero di documenti positivi
  Nneg <- length(neg) # numero di documenti negativi
  N <- Npos + Nneg # numero di documenti totali

  # i documenti negativi vengono aggiunti alla fine di quelli positivi
  docs <- append(pos, neg)

  # costruzione del dizionario dei termini da includere nella matrice
  dtm <- DocumentTermMatrix(Corpus(VectorSource(docs)))
  pos_count <- colSums(as.matrix(dtm[1:Npos, ]))
  neg_count <- colSums(as.matrix(dtm[(Npos+1):N, ]))
  tot_count <- pos_count + neg_count
  words <- colnames(dtm)
  mi <- list()
  p_pos <- Npos/N
  p_neg <- Nneg/N
  for (word in words) {
    fi_w_pos <- pos_count[[word]]/Npos
    fi_w_neg <- neg_count[[word]]/Nneg
    fi_w <- p_pos*fi_w_pos + p_neg*fi_w_neg

    mi[[word]] <- 0
    if (fi_w_pos != 0)
        mi[[word]] <- mi[[word]] + fi_w_pos * p_pos * log(fi_w_pos/fi_w)
    if (fi_w_pos != 1)
        mi[[word]] <- mi[[word]] + (1-fi_w_pos) * p_pos * log((1-fi_w_pos)/(1-fi_w))
    if (fi_w_neg!= 0)
        mi[[word]] <- mi[[word]] + fi_w_neg * p_neg * log(fi_w_neg/fi_w)
    if (fi_w_neg != 1)
      mi[[word]] <- mi[[word]]+ (1-fi_w_neg) * p_neg * log((1-fi_w_neg)/(1-fi_w))

  }
  high.freq <- tail(sort(unlist(mi)), n = n_features)
  dictionary <- rownames(as.data.frame(high.freq))

  # costruzione della matrice con dizionario dictionary
  ds_dtm <- DocumentTermMatrix(Corpus(VectorSource(docs)), control = list(dictionary=dictionary))

  # restituisce la matrice partizionata in documenti positivi e negativi
  return(list(pos=ds_dtm[1:Npos, ], neg=ds_dtm[(Npos+1):N, ]))
}
