library(RCurl)
library(ROAuth)
library(twitteR)
library(plyr)
library(stringr)
library(httr)
library(base64enc)
#encoding
library(httpuv)
library(devtools)
library(wordcloud)

library(tm)

setwd("F:/MOVIIIII")
s=readline();
# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
reqURL <- "https://api.twitter.com/oauth/request_token"  #important at the moment that it is https -> Twitter needs a secure connection
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <-"Gfw4xh3Rnf97lau95RBscxT95" #if you don't have this values, you can get them in twitter developer page – create an API (it doesn’t cost anything and you get the values pretty easy)
consumerSecret <- "RFIvq37bjcBeKpiK3gPiICQg4a2F9koPZs0xTpW8zUBE5Ezspj"
twitCred <- setup_twitter_oauth(consumerKey , consumerSecret, access_token=NULL, access_secret=NULL)
#twitCred$handshake() # the program will ask you for a PIN - this is obtained by Authorising the App in your browser.
#registerTwitterOAuth(twitCred)
tweets = searchTwitter(s, n=200,lang = "en") 
length(tweets) #it tells you how many tweets do you download (it should be 1500 – notice n=1500 on the previous line)

tweets.text = sapply(tweets, function(t)t$getText())

myCleanedText <- sapply(tweets.text, function(x) iconv(enc2utf8(x), sub = "byte"))

mach_corpus = Corpus(VectorSource(myCleanedText))

#tm_map(mach_corpus, function(x) function(x) iconv(x, to='UTF-8', sub='byte'))
# create document term matrix applying some transformations
tdm = TermDocumentMatrix(mach_corpus, control = list(removePunctuation = TRUE,stopwords = c("https",stopwords("english")),removeNumbers = TRUE))

m = as.matrix(tdm)

word_freqs = sort(rowSums(m), decreasing=TRUE) 

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

sentiment = function(sentences, pos.words, neg.words, vpos.words, vneg.words, .progress = 'none')
{
  final_scores <- matrix("", 0, 5)
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list or a vector as an 'l' for us
  # we want a simple array of scores back, so we use 'l' + 'a' + 'ply' = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words, vpos.words, vneg.words) {
    initial_sentence <- sentence
    # clean up sentences with R’s regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
  #  sentence= gsub('[^[:graph:]]','',sentence)
   sentence = gsub('(RT|via)((?:\b\\W*@\\w+)+)','',sentence)
    sentence = gsub('http[^[:blank:]]+', '', sentence)
    sentence = gsub('@\\w+', '', sentence)
   # sentence = gsub('[ t]{2,}', '', sentence)
    sentence = gsub('^\\s+|\\s+$', '', sentence)
  # sentence = gsub('[^[:alnum:]]', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    vpos.matches=match(words, vpos.words)
    vneg.matches=match(words, vneg.words);
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = sum(!is.na(pos.matches))
    neg.matches = sum(!is.na(neg.matches))
    vpos.matches = sum(!is.na(vpos.matches))
    vneg.matches = sum(!is.na(vneg.matches))
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    #score = sum(pos.matches) - sum(neg.matches)
    score <- c(vneg.matches, neg.matches, pos.matches, vpos.matches)
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, pos.words, neg.words,vpos.words,vneg.words, .progress=.progress )
  
  return(scores)
}


pos = scan('pos.txt', what='character',comment.char=';')
neg = scan('neg.txt', what='character',comment.char=';')
vneg = scan('vneg.txt', what='character',comment.char=';')
vpos = scan('vpos.txt', what='character',comment.char=';')

tweetResult <- as.data.frame(sentiment( myCleanedText, pos, neg, vpos, vneg))
tweetResult$`2` = as.numeric(tweetResult$`2`)
tweetResult$`3` = as.numeric(tweetResult$`3`)
tweetResult$`4` = as.numeric(tweetResult$`4`)
tweetResult$`5` = as.numeric(tweetResult$`5`)
counts = c(sum(tweetResult$`2`),sum(tweetResult$`3`),sum(tweetResult$`4`),sum(tweetResult$`5`))
names = c("WORST","BAD","GOOD","VERY GOOD")
mr = list(counts,names)
colors = c("red", "yellow", "green", "violet")


barplot(mr[[1]], main="Movie Review", xlab="Number of votes",legend=mr[[2]],col=colors)
lbls=names
pcts<-NULL
pct <- round((counts/sum(counts)*100),digits = 1)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(counts,labels = lbls, col=rainbow(length(lbls)),
    main="Movie Review")

#pie(counts, labels = names, main="Movie Review")
sum1=(sum(tweetResult$`2`)+sum(tweetResult$`3`)+sum(tweetResult$`4`)+sum(tweetResult$`5`))
rate=((1*sum(tweetResult$`2`))+(2*sum(tweetResult$`3`))+(3*sum(tweetResult$`4`))+(4*sum(tweetResult$`5`)))
review=rate/sum1
review=round(review,digits = 2)
#probability of movie being worst given these tweets and movie reviews
pw=(sum(tweetResult$`2`)/sum(counts))*25
pb=(sum(tweetResult$`3`)/sum(counts))*25
pg=(sum(tweetResult$`4`)/sum(counts))*25
pv=(sum(tweetResult$`5`)/sum(counts))*25
 cat("Probability of movie being worst is: ",round(pw,digits=2),"%\n")
 cat("Probability of movie being bad is: ",round(pb,digits=2),"%\n")
 cat("Probability of movie being good is ",round(pg,digits=2),"%\n")
 cat("Probability of movie being very good is ",round(pv,digits=2),"%\n")
 