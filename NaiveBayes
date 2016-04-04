#Loading Amazon Reviews DataSet
Orginal = read.csv("Amazon_reviews.csv")
str(Orginal)
#Selecting Only the required columns
reviews = Orginal[,c(1,2,3,4,5,6,7)]
str(reviews)
#Merging the review heading and the review
reviews$review=paste(reviews$rev_heading, reviews$review, sep = ",")

#Converting ratings into classes
reviews$rev_rating[reviews$rev_rating == 5] = "Very Positive"
reviews$rev_rating[reviews$rev_rating == 4] = "Positive"
reviews$rev_rating[reviews$rev_rating == 3] = "Neutral"
reviews$rev_rating[reviews$rev_rating == 2] = "Negative"
reviews$rev_rating[reviews$rev_rating == 1] = "Very Negative"
reviews$rev_rating = as.factor(reviews$rev_rating)

#Text mining & Building Corpus
library(tm)
library(quanteda)
myCorpus = Corpus(VectorSource(reviews$review))
myCorpus = corpus(myCorpus)
mydfm = dfm(myCorpus, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, removeSeparators = TRUE, 
            stem = TRUE, ignoredFeatures = c(stopwords("english")))
dtm = as.DocumentTermMatrix(mydfm, weighting = weightTfIdf)
dtm2 = removeSparseTerms(dtm, 0.97)
#Splitting the data into train & test
library(caTools)
library(mlbench)
library(e1071)
Label = as.data.frame(as.matrix(dtm2))
Label$rev_rating = reviews$rev_rating
Label$prod_id = reviews$prod_id
Label$review_id = reviews$review_id
set.seed(100)

sample = sample.split(Label$rev_rating, SplitRatio = 0.75)
train = subset(Label, sample==TRUE)
test = subset(Label, sample == FALSE)

#NaiveBayes Model
model = naiveBayes(rev_rating ~ .-(prod_id+review_id), data = train, laplace = 3)
summary(model)
pred = predict(model,test)
table(test$rev_rating, pred)
sum(diag(table(test$rev_rating, pred)))
