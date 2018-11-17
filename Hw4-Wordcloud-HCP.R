#Homework 04
#Name : Hari chandana Paruchuri

#Setting up Directories
setwd("C:/Users/hari chandana/Desktop/hwo4")

#Loading Packages
install.packages('tm')
library(tm)
library(plyr)
install.packages('wordcloud')
library(wordcloud)
require(RCurl)
install.packages('SnowballC')
library(SnowballC)
install.packages("twitteR", dependencies=T)
library(twitteR)
require(twitteR)


#connection of twitter and R by generating keys and tokens in apps.twitter.com with my Twitter account
#Written watching the youtube videos https://www.youtube.com/watch?v=lT4Kosc_ers

key <- 'IbSNZFHlzZfytHIlRLy6by2f5'
Secretkey <- 'MPKlNzNVqNxNa2wWtd9Ta5Ozpbn2wqyZ8t5YxHkz735p8HfUot'
token <- '731737681630134273-rk6aFHOhWaFpvyox24uF1g8QEhWvFRd'
SecretToken <- 'NYFevHw7uVU3R6fgVulwdypDslVvdFxssOI6xR1U3XIBM'

#Setting up twitter authentication to search and get data.
setup_twitter_oauth(key,Secretkey,token,SecretToken)

#searching twitter on the word "data analytics"
allData = searchTwitter("Data Analytics", n=1500)
allData = strip_retweets(allData)
textData <- sapply(allData, function(x) x$getText())

#Cleaning the data of punctuations,websites , digits and tagging people (mentions) 
#https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-2-create-word-cloud/
textData = gsub("http\\w+", "", textData)
textData = gsub("[[:digit:]]", "", textData)
textData = gsub("@\\w+","", textData)

#length(textData)
#textData

corpusData <- Corpus(VectorSource(textData))

#inspect the corpus data
inspect(corpusData)

finalData <- tm_map(corpusData, stripWhitespace)
finalData <- tm_map(corpusData, content_transformer(tolower))
finalData <- tm_map(corpusData, removeWords, stopwords("english"))
finalData <- tm_map(corpusData, removeNumbers)
finalData <- tm_map(corpusData, removePunctuation)
finalData <- tm_map(corpusData, removeWords, c("the", "its", " and "," for ", "RT","kno+","what","want","where","you","your","the"))

wordcloud(finalData, random.order=FALSE, max.words = 100, scale=c(5,0.5),rot.per=0.35,use.r.layout=FALSE, colors=rainbow(40))

#frequency of words in the tweets and writing it to csv file for plotting it in tablaeu 

#Code taken from https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/wordcloud1 step 4 and step 5 and https://rpubs.com/brandonkopp/creating-word-clouds-in-r
tdm <- TermDocumentMatrix(finalData)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m),decreasing=TRUE)
df <- data.frame(word = names(word_freqs),freq=word_freqs)


#writing it to a file to be used for tableau projectionsw
write.csv(df,"WordCloudData.csv")
