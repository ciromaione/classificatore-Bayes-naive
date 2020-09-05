# Title     : test dataset recensioni
# Created by: Ciro Maione
# Created on: 13/08/20

require(tm)

source("cleaning.R")
source("features_reduction.R")
source("training.R")
source("prediction.R")



doc <- read.csv("recensioni.csv")

# cleaning
doc$text <- clean_ds(doc$text)

n_doc <- nrow(doc)

# split in training e test
train <- doc[1:(n_doc*0.8), ]
test <- doc[(n_doc*0.8+1):n_doc, ]

test.pos <- test[test$class == "positive", "text"]
test.neg <- test[test$class == "negative", "text"]


for (n_features in seq(500,2000,100)){
  start <- Sys.time()
# features reduction
#dtm <- ds_dtm_by_frequency(train, n_features)
#dtm <- ds_dtm_by_tfidf(train, n_features)
dtm <- ds_dtm_by_mutual_information(train, n_features)

model <- train_model(dtm)

pred_pos <- predict(model, test.pos)
pred_neg <- predict(model, test.neg)
print("***************************")
print(n_features)
print("***************************")
print(table(pred_pos))
print(table(pred_neg))
print(as.numeric(Sys.time()-start, units="secs"))
print("---------------------------")
}