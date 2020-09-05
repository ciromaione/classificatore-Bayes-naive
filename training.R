# Title     : training
# Created by: Ciro Maione
# Created on: 13/08/20

# questa funzione costruisce il modello probabilistico a partire dalla DocumentTermMatrix
# utilizza un laplace smooting per evitare probabilità nulle
train_model <- function (dtm) {
  Npos <- dtm$pos$nrow # numero di documenti positivi
  Nneg <- dtm$neg$nrow # numero di documenti negativi
  N <- Npos + Nneg # numero di documenti totale
  # parole nel modello
  words <- colnames(dtm$pos)

  # calcolo delle probabilità condizionate da usare per la predizione come numero di occorrenze
  # della parola nella classe diviso numero di documenti della classe con laplace smooting
  features_prob <- data.frame()
  for (word in words) {
    features_prob[word, "positive"] <- (sum(dtm$pos[, word]) + 1) / (Npos + N)
    features_prob[word, "negative"] <- (sum(dtm$neg[, word]) + 1) / (Nneg + N)
  }

  prior <- list(positive=Npos/N, negative=Nneg/N) # probabilità a priori

  return(list(prior=prior, features_prob=features_prob))
}