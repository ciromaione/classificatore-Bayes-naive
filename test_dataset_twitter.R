# Title     : test_dataset_twitter
# Created by: Ciro Maione
# Created on: 13/08/20


require(tm)

source("cleaning.R")
source("features_reduction.R")
source("training.R")
source("prediction.R")

doc <- read.csv("twitter.csv")

# cleaning
doc$text <- clean_ds(doc$text)

doc.pos <- doc[doc$class == "positive", ]
doc.neg <- doc[doc$class == "negative", ]

# split in training e test
n_doc <- nrow(doc.pos)
n_docn <- nrow(doc.neg)
train <- rbind(doc.pos[1:(n_doc*0.8), ], doc.neg[1:(n_docn*0.8), ])
test <- rbind(doc.pos[(n_doc*0.8+1):n_doc, ], doc.neg[(n_docn*0.8+1):n_docn, ])
test.pos <- test[test$class == "positive", "text"]
test.neg <- test[test$class == "negative", "text"]

## features reduction
##dtm <- ds_dtm_by_frequency(train, min_freq = 4)
##dtm <- ds_dtm_by_tfidf(train, 1500)
#dtm <- ds_dtm_by_mutual_information(train, 1000)
#
#print("inizio train")
#print(Sys.time())
#model <- train_model(dtm)
#print("fine train")
#print(Sys.time())
#
#print("inizio test")
#print(Sys.time())
#pred_pos <- predict(model, test.pos)
#pred_neg <- predict(model, test.neg)
#print("fine test")
#print(Sys.time())
#table(pred_pos)
#table(pred_neg)
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