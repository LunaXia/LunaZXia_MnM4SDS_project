install.packages("twitteR")

#load library
library(twitteR)

#load credentials
consumer_key <- "gVLvIy8FwDmOYGTeytKIjxTO3"
consumer_secret<- "INAQAClbUP4ei6OvF6ffmw64uWEbmqaINUJ67KCfQTrtTObbSY"
access_token <- "1126647436758401026-KGA62hhEVcvr8ykvOfqPySOGtTU4XZ"
access_secret <- "7sOmVG32we0L4E2LlTc5HmzhmojGbGuxe4tjZOrNWRwS3"

#set up to authenticate
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)


#fetch tweets associated with that hashtag , 12 tweets-n in 
#(en)glish-lang since the indicated date yy/mm/dd
tweets <- twitteR::searchTwitter("#Maryland Day",n =100,lang ="en",since = "2018-01-01")



# Another search example 
searchTwitter('apartment hunting', geocode='40.7361,-73.9901,5mi', n=5000,retryOnRateLimit=1)


#Operator and Behavior


#Another example
terms <- c("iphonex", "iPhonex", "iphoneX", "iPhoneX", "iphone10", "iPhone10","iphone x", "iPhone x", "iphone X", "iPhone X", "iphone 10", "iPhone 10", "#iphonex", "#iPhonex", "#iphoneX", "#iPhoneX", "#iphone10", "#iPhone10")
terms_search <- paste(terms, collapse = " OR ")
iphonex <- searchTwitter(terms_search, n=1000, lang="en")
iphonex <- twListToDF(iphonex)
write.table(iphonex,"/Users/Ruibo/temp/iphonex.csv", append=T, row.names=F, col.names=T,  sep=",")


#strip retweets
strip_retweets(tweets)

n.tweet <- length(tweets)
n.tweet

#convert to data frame using the twListtoDF function
df <- twListToDF(tweets) #extract the data frame save it locally
saveRDS(df, file="tweets.rds")
df1 <- readRDS("tweets.rds")