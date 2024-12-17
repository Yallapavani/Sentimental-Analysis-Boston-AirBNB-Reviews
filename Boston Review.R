install.packages("dplyr")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("readr")
install.packages("tm")  #Text analytics, text mining.
install.packages("wordcloud")  #Creating wordcloud
install.packages("syuzhet")
install.packages("NLP")
install.packages("RColorBrewer")
install.packages("lubridate")
install.packages("scales")
install.packages("reshape2")


library(dplyr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(readr)
library(NLP)
library(RColorBrewer)
library(tm)
library(wordcloud)
library(syuzhet)
library(lubridate)
library(scales)
library(reshape2)


#Importing data.
df <- read.csv("https://query.data.world/s/julwdvohuuexuhnirwtbrr5luh573j", header=TRUE, stringsAsFactors=FALSE)
glimpse(df)
dim(df)
df$id

#Let's rename id column to review id to avoid confusion.
df_renamed <- rename(df, "review_id" = "id")
glimpse(df_renamed)

#Removing data column as it is not of much use
df_1 <- df_renamed[, -c(3,5)]
glimpse(df_1)

#Let's see the number of unique listings.
unique_listings <- df_1 %>% count(listing_id)
unique_listings

unique <- as.data.frame(unique_listings)
glimpse(unique)
dim(unique)

#So we can see that there are 4993 unique listings in the data set.
#Let's arrange them in order of most to least reviews.

arrange(unique, desc(n))

#Let's select the listing with the most number of unique reviews and see what the sentiment about it is.
glimpse(df_1)

df_2 <- df_1 %>% filter(listing_id == 66288)
glimpse(df_2)

#Let's check if each review_id is unique.
df_2 %>% count(review_id) %>% arrange(desc(review_id))

#So we checked that all reviews are unique.
#Our data is cleaned and is ready for analysis.

glimpse(df_2)

#Now we are only going to need the comments column for our sentiment analysis.
corpus_1 <- iconv(df_2$comments)
corpus_2 <- Corpus(VectorSource(corpus_1))

#To see corpus
inspect(corpus_2[1:5])

#Cleaning Corpus.
corpus_3 <- tm_map(corpus_2, tolower)

#Removing Punctuation.
corpus_4 <- tm_map(corpus_3, removePunctuation)

#Removing Numbers.
corpus_5 <- tm_map(corpus_4, removeNumbers)

#Removing stop-words (Like is, the, are)
corpus_6 <- tm_map(corpus_5, removeWords, stopwords("english"))

#Removing spaces from the start and end of any text.
corpus_7 <- tm_map(corpus_6, stripWhitespace)

#Now let's inspect our corpus.
inspect(corpus_7[1:5])

#Now we'll pass our data in na new data frame
reviews <- corpus_7

#Now we'll create a term document matrix.
term_doc <- TermDocumentMatrix(reviews)
tdm <- as.matrix(term_doc)

#Let's view the first 10 rows and 5 columns of tdm.
tdm[1:10, 1:5]  #each row represents each word that appears in the reviews, and each coloumn represents how many times in a document does that word comes.
#So like the word 'close' comes  1 time in the first review, 0 times the in the 2nd, 3rd, 4th and 5th.


#Bar plot of words
w <- rowSums(tdm)

#Now we'll subset this for only those words which occur more than 10 times.
w_1 <- subset(w, w>=25)
w_1
s_w_1 <- sort(w_1)
#Let's plot it.
barplot(s_w_1, las = 2, col = "lightblue")

#So as we can see words like sean, and place occur more than 400 times but don't provide any sentiment so we'll remove them.
corpus_8 <- tm_map(corpus_7, removeWords, c("sean", "place", "boston", "stay", "seans", "location", "host", "also", "even", "experience", "night", "stayed", "apartment", "youre", "can", "located", "studio", "room", "just", "one", "two"))

reviews_1 <- corpus_8
term_doc_1 <- TermDocumentMatrix(reviews_1)
tdm_1 <- as.matrix(term_doc_1)

w_2 <- rowSums(tdm_1)
w_3 <- subset(w_2, w_2>=25)
w_4 <- sort(w_3)
barplot(w_4, las = 2, col = "lightblue")

#Now let's create a wordcloud.
w_5 <- sort(rowSums(tdm_1), decreasing = T)
set.seed(200)
word_cloud <- wordcloud(words = names(w_5),
                        freq = w_5,
                        max.words = 50,
                        random.order = T,
                        min.freq = 5,
                        colors = brewer.pal(8, "Dark2"),
                        scale = c(4, 0.4))

#Now let's obtain sentiment scores.
sentiment_data <- iconv(df_2$comments)

s <- get_nrc_sentiment(sentiment_data)
s[1:10,]

#Calculate review wise score.
s$score <- s$positive - s$negative
s[1:10,]

#So we can see the score of individual reviews and thus the sentiment in it.
#We can calculate a final score to find the overall sentiment of the property based on all reviews.

final_score <- sum(s$score)
final_score

#So as we can see that the overall score is positive, so this property has a generally good or positive sentiment and reviewers like it.

#Let's convert our score into csv to visualize it in Tableau.
write.csv(x = s, file = "C:/Users/HP/OneDrive/Desktop/s_a.csv")

#Check product Sentiment

#Check overall sentiment of the property.
review_score <- colSums(s[,])
print(review_score)

#Plot property sentiment
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment')

#So we can see that the scores for most of the negative sentiments like anger, disgust, and sadness are pretty low.
#Whereas most of the positive sentiments like joy and trust have high scores, which makes it a positively inclined sentiment.

#Now we'll do the same analysis for 10 more properties, (the top 10 with most number of reviews) and visualize our results in Tableau.
