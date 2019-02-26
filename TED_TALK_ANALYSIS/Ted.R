library(readr)
library(anytime)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(stringr)
library(igraph)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(tm)
library(qdap)
library(rJava)
library(RWeka)
library(tmap)
library(plotrix)
library(magrittr)
library(broom)
library(tidyr)
library(tidytext)
library(radarchart)

#Check Missing Values
any(is.na(ted_main))

ted_main$film_date <- anydate(ted_main$film_date)
ted_main$published_date <- anydate(ted_main$published_date)

# Joining datasets
ted <- left_join(ted_main, transcripts, by = "url")

# Cleaning Data
names(ted)
dim(ted)
ted$transcript[ted$transcript == ""] <- NA
complete.cases(ted)
is.na(ted[59,])
ted <- na.omit(ted)

# Convert time to minutes
ted$duration <- round(ted$duration/60, digits = 2)

# Convert Views in Millions
ted$views <- round(ted$views/1000000,2)

# Find Top 500 Ted Talks
ted_top <- ted %>% arrange(desc(views))
head(ted_top$views)
ted_top <- ted_top[1:500,]

# Bottom 500 Ted Talks
ted_bot <- ted %>% arrange((views))
head(ted_bot$views)
ted_bot <- ted_bot[1:500,]

# Creating transcript data
ted_top_t1 <- data.frame(ted_top$transcript)
ted_top_t2 <- ted_top_t1$ted_top.transcript
str(ted_top_t)

ted_bot_t1 <- data.frame(ted_bot$transcript)
ted_bot_t2 <- ted_bot_t1$ted_bot.transcript
str(ted_bot_t)

# Cleaning Transcripts

qdap_clean <- function(x) {
  x <- replace_abbreviation(x)
  x <- replace_contraction(x)
  x <- replace_number(x)
  x <- replace_ordinal(x)
  x <- replace_symbol(x)
  x <- tolower(x)
  x <- bracketX(x)
  return(x)
}

ted_top_t <- qdap_clean(ted_top_t2)
ted_bot_t <- qdap_clean(ted_bot_t2)

# Source and create the corpus
tedtop_corp <- VCorpus(VectorSource(ted_top_t))
tedbot_corp <- VCorpus(VectorSource(ted_bot_t))

# tm_clean the corpus

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "like","TED", "ted","data","said","information","heh","information","hundred","one","thousand","one","two","fifty","years","nine","four","five","six"))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

tedtop_corp <- clean_corpus(tedtop_corp)
tedtop_corp[[100]][1]
tedbot_corp <- clean_corpus(tedbot_corp)

#Creating Bi-grams tokenizer
tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

#Creating Tokenized TDM
tedtop_tdm <- TermDocumentMatrix(tedtop_corp,control = list(tokenize = tokenizer))
tedbot_tdm <- TermDocumentMatrix(tedbot_corp,control = list(tokenize = tokenizer))

#Creating Tokenized Matrix
tedtop_m <- as.matrix(tedtop_tdm)
tedbot_m <- as.matrix(tedbot_tdm)

#Created Term Frequency
tedtop_f <- rowSums(tedtop_m)
tedbot_f<- rowSums(tedbot_m )

#Creating Weighted TDM
tedtop_tdm1 <- TermDocumentMatrix(tedtop_corp,control = list(weighting = weightTfIdf))
tedbot_tdm1 <- TermDocumentMatrix(tedbot_corp,control = list(weighting = weightTfIdf))

#Creating Weighted Matrix
tedtop_m1 <- as.matrix(tedtop_tdm1)
tedbot_m1 <- as.matrix(tedbot_tdm1)

# Calculate the rowSums: term_frequency
tedtop_f1 <- rowSums(tedtop_m1)
tedbot_f1 <- rowSums(tedbot_m1)

# Top Words by each
topf <- sort(tedtop_f1,decreasing = TRUE)
topf[1:25]
barplot(topf[1:10], col = "indianred3",las =2)

botf <- sort(tedbot_f1,decreasing = TRUE)
botf[1:25]
barplot(botf[1:10], col = "tan",las =2)

# Word Correlations
findAssocs(tedtop_tdm1,"music",0.2)

# Dendogram
tedtop2 <- removeSparseTerms(tedtop_tdm, .835)
tedbot2 <- removeSparseTerms(tedbot_tdm, .875)
tedtop2
tedbot2
tedtop21 <- removeSparseTerms(tedtop_tdm1, .30)
tedtop21
# Create hc as a cluster of distance values
hc <- hclust(dist(tedtop21), method = "complete")

# Produce a plot of hc
plot(hc)



#Overall WordCloud
texts <- transcripts$transcript
corpus <- Corpus(VectorSource(texts))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, c("and", "this", "there")) 
corpus <- Corpus(VectorSource(corpus))
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
d <- d[-which(d$word %in% c("and","this","that")),]
options(repr.plot.width = 8, repr.plot.height = 8)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Comparison Cloud & Commonality
ted_bot100 <- ted_bot[1:50,]
ted_top100 <- ted_top[1:50,]
ted_a <- data.frame(ted_top100$transcript,ted_bot100$transcript )

ted_aq_corpus <- VCorpus(VectorSource(ted_a))
ted_aq_corpus
tedaq_clean <- clean_corpus(ted_aq_corpus)
ted_aq_tdm <- TermDocumentMatrix(tedaq_clean)
colnames(ted_aq_tdm) <- c("Top 500", "Bottom 500") 
all_aq <- as.matrix(ted_aq_tdm )
head(all_aq) 
all_aq

#ted_all <- bind_rows(ted_top100,ted_bot100 )


commonality.cloud(all_aq, colors = "steelblue1",max.words = 70)
comparison.cloud(all_aq, max.words = 90, colors = c("palevioletred2", "hotpink4"),scale = c(1,1.5),title.size = 2.5)

#Pyramid
all <- data.frame(MyData)
dim(all)
head(all)
top25_df <- all  %>%
  # Keep rows where word appears everywhere
  filter_all(all_vars(. > 0)) %>% 
  # Get difference in counts
  mutate(difference = TOP - BOT) %>% 
  # Keep rows with biggest difference
  top_n(25, wt = difference) %>% 
  # Arrange by descending difference
  arrange(desc(difference))
top25_df
head(top25_df)
pyramid.plot(
  top25_df$TOP, 
  top25_df$BOT, 
  labels = top25_df$Terms, 
  top.labels = c("TOP", "Words", "BOT"), 
  main = "Words in Common", 
  unit = NULL,
  gap = 150,
)

#POLARITY
ted_bot6 <- ted_bot[1:6,]
ted_top6 <- ted_top[1:6,]

ted_top6 %$% polarity(ted_top6$transcript)
ted_bot6 %$% polarity(ted_bot6$transcript)

# The document sentiment has a range spread from
# -2 to 2, where -2 is really negative, -1 is negative, 1 is positive, 
# and 2 is really positive.
(ted_talk <- ted_top6 %$% polarity(transcript, main_speaker)) 
counts(ted_talk)
plot(ted_talk)

(ted_talk2 <- ted_bot6 %$% polarity(transcript, main_speaker)) 
counts(ted_talk2)
plot(ted_talk2)

# Tidy
tedtop_dtm <- DocumentTermMatrix(tedtop_corp)
tedtop_dtm 


tedbot_dtm <- DocumentTermMatrix(tedbot_corp)
tedbot_dtm 

ted_tidy <- tidy(tedtop_dtm)
ted_tidy2 <- tidy(tedbot_dtm)
ted_tidy3 <- tidy(tedtop_tdm1)

ted_tidy[1:5,]
ted_tidy2[1:5,]
ted_tidy3[33:45,]

#BING
bing <- get_sentiments("bing")
ted_bing_words <- inner_join(ted_tidy, bing, by = c("term" = "word"))
ted_bing_words
ted_bing_words %>%
  count(sentiment)


ted_bing_words2 <- inner_join(ted_tidy2, bing, by = c("term" = "word"))
ted_bing_words2
ted_bing_words2 %>%
  count(sentiment)

  # Set index to numeric document
ted_bing_words <- ted_bing_words %>%
mutate(index = as.numeric(document))

ted_bing_words2 <- ted_bing_words2 %>%
  mutate(index = as.numeric(document))

  # Count by sentiment, index
top_tidy_count <- ted_bing_words  %>%
count(sentiment, index)
tail( top_tidy_count)

top_tidy_count2 <- ted_bing_words2  %>%
  count(sentiment, index)
tail( top_tidy_count2)

  # Spread sentiments
tedtop_sp <- top_tidy_count %>%
spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,line_number = row_number())
head(tedtop_sp)

tedtop_sp2 <- top_tidy_count2 %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,line_number = row_number())
head(tedtop_sp2)

# Timeline of the sentiment 

ggplot(tedtop_sp, aes(index, polarity)) + 
  # Add a smooth trend curve
  geom_smooth()

ggplot(tedtop_sp2, aes(index, polarity)) + 
  # Add a smooth trend curve
  geom_smooth()

#NRC Plutchik
nrc <- get_sentiments("nrc")

ted_plutchik <- ted_tidy %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>%
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count))
 
ted_plutchik2 <- ted_tidy2 %>% 
  inner_join(nrc, by = c("term" = "word")) %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  group_by(sentiment) %>% 
  summarize(total_count = sum(count))

# Plot total_count vs. sentiment
ggplot(ted_plutchik, aes(x = sentiment, y = total_count)) +
  # Add a column geom
  geom_col(fill="darkslateblue") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")


ggplot(ted_plutchik2, aes(x = sentiment, y = total_count)) +
  # Add a column geom
  geom_col() +
 
#NRC 

# Plot polarity vs. line_number
ggplot(tedtop_sp, aes(line_number, polarity)) + 
  # Add a smooth trend curve
  geom_smooth() +
  # Add a horizontal line at y = 0
  geom_smooth(yintercept = 0, color = "red") +
  # Add a plot title
  ggtitle("Top Ted Talk Chronological Polarity") 

#Frequency Chart of POSITIVE & NEGATIVE

tedtop_freq <- ted_tidy %>%
  inner_join(bing, by = c("term" = "word")) %>% 
  count(term, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative) %>% 
  filter(abs(polarity) >= 200) %>% 
  mutate(
    pos_or_neg = ifelse(polarity > 0, "positive", "negative")
  )

# Plot polarity vs. (term reordered by polarity), filled by pos_or_neg
ggplot(tedtop_freq, aes(reorder(term, polarity), polarity, fill = pos_or_neg)) +
  geom_col() + 
  ggtitle("TOP TED TALKS: Sentiment Word Frequency") + 
  
  # Rotate text and vertically justify
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))




tedtop_freq2 <- ted_tidy2 %>%
  inner_join(bing, by = c("term" = "word")) %>% 
  count(term, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative) %>% 
  filter(abs(polarity) >= 200) %>% 
  mutate(
    pos_or_neg = ifelse(polarity > 0, "positive", "negative")
  )

# Plot polarity vs. (term reordered by polarity), filled by pos_or_neg
ggplot(tedtop_freq2, aes(reorder(term, polarity), polarity, fill = pos_or_neg)) +
  geom_col() + 
  ggtitle("TOP TED TALKS: Sentiment Word Frequency") + 
  
  # Rotate text and vertically justify
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))

#Emotional introspection

ted_plutchik3 <- ted_tidy %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>%
  filter(!grepl("positive|negative", sentiment)) %>% 
  count(sentiment, term) %>% 
  spread(sentiment, n, fill = 0) %>% 
  data.frame(row.names = "term")
options(repr.plot.width = 6, repr.plot.height = 6)
comparison.cloud(ted_plutchik3, max.words = 100,title.size = 1.5,scale=c(2.5,.9),title.colors = "red3",title.bg.colors = "gray88")

ted_plutchik4 <- ted_tidy2 %>% 
  # Join to nrc lexicon by term = word
  inner_join(nrc, by = c("term" = "word")) %>%
  filter(!grepl("positive|negative", sentiment)) %>% 
  count(sentiment, term) %>% 
  spread(sentiment, n, fill = 0) %>% 
  data.frame(row.names = "term")
options(repr.plot.width = 6, repr.plot.height = 6)
comparison.cloud(ted_plutchik4, max.words = 80,title.size = 0.8,scale=c(1.7,.5))

#Kernel Density
afinn <- get_sentiments("afinn")
ted_kernel <- ted_tidy %>% 
  # Inner join to afinn lexicon
  inner_join(afinn, by = c("term" = "word"))

head(ted_kernel)

# Plot score, filled by book
ggplot(ted_kernel, aes(score)) + 
  # Set transparency to 0.3
  geom_density(alpha = 0.3,fill= "orange2") + 
  ggtitle("AFINN Score Densities") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")


