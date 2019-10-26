# r-file-

install.packages('sentimentr')


#sentiment score 
sentiment_by('I am not very good', by = NULL)


sentiment("Excellent", "Good", "Perfect", "Great", "Smooth", "Compfy", "Quicker", "Joy", "Professional", "Comfortable")


#new src 
setwd("file here")
library("stringr")
library(sentimentr)
library(wordcloud2)
library(text2vec) # needed for tokenizer
library(tm) # needed for stopwords
library(ggplot2)

# use readLines to return a character vector (each element is a line in the file)
script <- readLines("") #file that contains the data for Jetblue 
cat(script, sep = "\n")

pattern <- "[[:upper:]]+.*:"

characters <- str_match_all(script, pattern)
unique(characters)

characters <- gsub(":","",unlist(characters))
characters <- gsub("FLASHBACK REGINA","REGINA", unlist(characters))
characters <- gsub("Mom","MOM", unlist(characters))
characters <- gsub("MS NORBERRY","MS NORBURY", unlist(characters))
characters <- gsub("ASIAN GUY","ASIAN BOY", unlist(characters))
characters <- gsub("CADY(VO)","CADY", unlist(characters), fixed = TRUE)
characters <- gsub("CADY (VO)","CADY", unlist(characters), fixed = TRUE)
characters <- gsub("Dad","DAD", unlist(characters))
characters <- gsub("GETCHEN","GRETCHEN", unlist(characters))
characters <- gsub("Cady","CADY", unlist(characters))
characters <- gsub("MIAN","DAMIAN", unlist(characters))
charNo <- c("ALL", "ANNOUNCEMENTS", "EVERYONE", "SONG)", "SINGING")
remove <- characters %in% charNo
characters <- characters[!remove]

t <- table(characters)
t

sort(t)

scene1<- script[1:132]
scene2<- script[133:1336]
scene3<- script[1337:1523]
scene4<- script[1524:1830]

script1 <- paste0(script, collapse = "\n")
script1
cat(script1)

ReginaPattern <- "REGINA: .*?\n"
Regina <- str_extract_all(script1, ReginaPattern)
Regina <- Regina[[1]]

cat(Regina)
Regina <- gsub("REGINA: ", "", Regina)

words <- word_tokenizer(tolower(Regina))
words <- unlist(words)

t <- table(words)
sort(t)

remove <- words %in% stopwords()
words <- words[!remove]

sort(t)

#Word Cloud
t <- table(words)
df <- data.frame(t)

wordcloud2(df, size = .5)

keep <- df$Freq >= 2
wordcloud2(df[keep,], size = .5)

#Sentiment
#\xfc\xbe\x98\x96\x8c\xbc
#\xfc\xbe\x98\x96\x8c\xbc
Regina <- gsub("\xfc\xbe\x98\x96\x8c\xbc", "", Regina)
Regina <- gsub("\xfc\xbe\x8e\x96\x8c\xbc", "", Regina)
Regina <- gsub("\xfc\xbe\x98\x93\xa0\xbc", "a", Regina)

txt <- "you are a skeez"
sentiment(txt)

l <- lexicon::hash_sentiment_jockers_rinker

x <- c(l$x, "skeez")
y <- c(l$y, -1)

l <- as_key( data.frame(x,y))
sentiment(txt, polarity_dt = l)

tmp <- Regina

g <- get_sentences(Regina)
s <- sentiment_by(g)

i <- which.min(s$ave_sentiment)
Regina[i]

i <- which.max(s$ave_sentiment)
Regina[i]

highlight(s)

label_sentiments <- function(x){
  y <- rep("neutral", length(x))
  y[x > 0.05] <- "positive"
  y[x < -0.05] <- "negative"
  y
}

sentiments <- label_sentiments(s$ave_sentiment)
df <- data.frame(sentiments)

ggplot(df, aes(x=sentiments, fill= sentiments)) + geom_bar() +
  theme(legend.position = "none") + ggtitle("Sentiment Analysis of REGINA") +
  ylab("Number of Lines")



