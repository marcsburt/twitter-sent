library("twitteR")
library("ROAuth")
library("RCurl")
library("bitops")
library("rjson")
library("httr")
library("tm")
library("SnowballC")
library("ggplot2")
library("wordcloud")

setwd("/home/marcburt/BU DA/Web_Analytics/twitter_sent")

##### load credentials

load("twitter_credentials")
setup_twitter_oauth(t.api.key, t.api.secret, access_token=NULL, access_secret=NULL)

### Search for Texas and Maine tweets

texas.tweets <- searchTwitter('#Texas', n = 500)
maine.tweets <- searchTwitter('#Maine', n = 500)

### Create corpus of tweets

#texas
texas.text <- lapply(texas.tweets, function(t){t$getText()})
texas.source <- VectorSource(texas.text)
texas.corp <- Corpus(texas.source)

#maine
maine.text <- lapply(maine.tweets, function(t){t$getText()})
maine.source <- VectorSource(maine.text)
maine.corp <- Corpus(maine.source)

# preprocessing while still in corpus form using tm_map() functions and gsub for one off transformations

clean.corpuses <- function(corpus){
	#remove: URL, caps, punct, stop words, numbers, stem everything, whitespace
	#output -> create term document matrix
	cleaned <- tm_map(corpus, content_transformer(function(x) iconv(x, to = 'ASCII', sub="" ))) ##remove non-ASCII
	cleaned <- tm_map(cleaned, content_transformer(function(x) gsub("http[[:alnum:]]*", "", x))) ## remove URL
	cleaned <- tm_map(cleaned, content_transformer(tolower))
	cleaned <- tm_map(cleaned, removePunctuation)
	cleaned <- tm_map(cleaned, removeWords, stopwords("english"))
	cleaned <- tm_map(cleaned, removeNumbers)
	cleaned <- tm_map(cleaned, stemDocument)
	cleaned <- tm_map(cleaned, stripWhitespace)
	tdm <- TermDocumentMatrix(cleaned)
	return(tdm)

}

texas.tdm <- clean.corpuses(texas.corp)
maine.tdm <- clean.corpuses(maine.corp)

### word frequencies

word.freq <- function(tdm){
	freq <- rowSums(as.matrix(tdm))
	freq <- sort(freq, decreasing = TRUE)
	return(freq)
}

texas.freq <- word.freq(texas.tdm)
maine.freq <- word.freq(maine.tdm)
texas.top <- head(texas.freq, 20)
maine.top <- head(maine.freq, 20)



png('combined.png')
par(mfrow =c(1,2))
plot(texas.top, type = 'hist', xaxt = 'n', pch = 19, las = 2)
axis(1, at = seq(1:20), labels = names(texas.top), las = 2)
plot(maine.top, type = 'hist', xaxt = 'n', pch = 19, las = 2)
axis(1, at = seq(1:20), labels = names(maine.top), las = 2)
dev.off()

set.seed(3417)
png('combined.cloud.png')
par(mfrow = c(2,1))
wordcloud(names(texas.freq), texas.freq, min.freq = 6, colors = brewer.pal(4, "Set1"))
wordcloud(names(maine.freq), maine.freq, min.freq = 6, colors = brewer.pal(4, "Set3"))
dev.off()


sentiment <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  score <- sum(pos.matches) - sum(neg.matches)
  cat (" Positive: ", words[pos.matches], "\n")
  cat (" Negative: ", words[neg.matches], "\n")
  return (score)
}


pos.words = scan('positive-words.txt', what = 'character', comment.char= ';')
neg.words = scan('negative-words.txt', what = 'character', comment.char= ';')

sentiment(names(texas.freq), pos.words, neg.words)
sentiment(names(maine.freq), pos.words, neg.words)
