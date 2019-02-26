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
library(date)
library(knitr) 
library(kableExtra) 
library(formattable)
library(lubridate)
library(ggthemes)



# MOST VIEWED TALK PER YEAR
ted_analysis1 <- data.frame(ted_main$published_date,ted_main$name,ted_main$views)
head(ted_analysis1)


ted_analysis1$ted_main.published_date <- format(as.Date(ted_analysis1$ted_main.published_date, format="%Y-%m-%d"),"%Y")


ted_analysis1.1 <- ted_analysis1 %>% group_by(ted_main.published_date,ted_main.name) %>% 
summarise( total_views = round(ted_main.views /1000000 ,2)) %>%
arrange(ted_main.published_date,desc(total_views)) %>% 
mutate(rk = rank(desc(total_views))) %>% 
filter(rk <= 1) 

colnames(ted_analysis1.1)[1] <- "Year"
colnames(ted_analysis1.1)[2] <- "Title"
colnames(ted_analysis1.1)[4] <- "Rank"

ted_analysis1.2 <- data.frame(ted_analysis1.1)

ted_analysis1.2 %>%
  arrange(Year) %>%
  mutate(Year = color_tile("lightblue", "lightgreen")(Year)) %>%
  mutate(Rank = color_tile("lightgreen", "lightgreen")(Rank)) %>%
  select(Year, Title ,Rank)  %>%
  kable("html", escape = FALSE, align = "c", caption = "Most Viewed Ted Talks per year") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)
#Occupations OF SPEAKERS
temp <- ted %>% group_by(speaker_occupation) %>% tally(sort=TRUE)
attach(temp)
wordcloud(speaker_occupation,n,scale=c(2,0.52),min.freq=5,random.order = TRUE,random.color = TRUE,rot.per = 0.3,colors=c("#d7dd35","#465c8b","#3f3250","#ccdfcb"))

# TED TALKS BY DATE ANALYSIS
ted$date <- anydate(ted$film_date)
ted$month <- month(ted$date)
ted$year <- year(ted$date)
ted$wkday <- weekdays(ted$date,abbreviate = TRUE)
ted$pubdate <- anydate(ted$published_date)
ted$pubmonth <- month(ted$pubdate)
ted$pubyear <- year(ted$pubdate)
ted$pubwkday <- weekdays(ted$pubdate,abbreviate = TRUE)

ted$filming_month <- as.numeric(format(ted$film_date, format= "%m"))
ted$filming_year <- as.numeric(format(ted$film_date, format = "%Y"))
month_df <- as.data.frame(table(ted$filming_month))
colnames(month_df) <- c("Month", "Talks")
month_df$Month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

year_df <- as.data.frame(table(ted$filming_year))
#year_df
colnames(year_df) <- c("Year", "Talks")
options(repr.plot.width = 8, repr.plot.height = 4)
p11 <- ggplot(data = year_df, aes(Year, Talks, group = 1)) +
  geom_line(color = "blue") +
  geom_point() +
  geom_text(aes(label = Talks), vjust = 1.6, color = "black", size = 3) +
  
  ggtitle("Ted Talks on different years")
p11


keeps <- c("filming_month", "filming_year")
heatmap_df <- ted %>% subset(select = keeps) %>% arrange(filming_year)
heatmap_df <- as.data.frame(table(heatmap_df))
heatmap_df$filming_month <- month.abb[heatmap_df$filming_month]
heatmap_df$filming_month <- as.character(heatmap_df$filming_month)

heatmap_df$filming_month <- factor(heatmap_df$filming_month, levels=unique(month_df$Month))
#heatmap_df 

ggplot(heatmap_df, aes(filming_year, factor(filming_month))) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(fill = heatmap_df$Freq, label = heatmap_df$Freq)) +
  scale_fill_gradient2(low = "darkred", 
                       mid = "white", 
                       high = "midnightblue", 
                       midpoint = round(mean(heatmap_df$Freq))) +
  ylab("Year") +
  xlab("Months") +
  ggtitle("Heatmap showing occurence of TED Talks") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Freq")

# Most TALKS by speaker

speaker_df <- as.data.frame(table(ted$main_speaker))
speaker_df <- arrange(speaker_df, desc(Freq))
head(speaker_df)

# Occupations

occupation_df <- as.data.frame(table(ted$speaker_occupation))
occupation_df <- arrange(occupation_df, desc(Freq))
occupation_df <- head(occupation_df, 10)
colnames(occupation_df) <- c("Occupation", "Appearances")
occupation_df$Occupation <- as.character(occupation_df$Occupation)
occupation_df$Occupation <- factor(occupation_df$Occupation, levels=occupation_df$Occupation)
options(repr.plot.width = 5, repr.plot.height = 4)
p12 <- ggplot(data = occupation_df, aes(factor(Occupation), Appearances, fill  =Occupation)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = Appearances), vjust = 1.6, color = "white", size = 3) +
  ggtitle("Occupations of Ted Speakers vs their Appearances") +
  xlab("Occupation") 
p12

# Views by Occupation
top_occupation_views <- filter(ted, ted$speaker_occupation %in% occupation_df$Occupation)
options(repr.plot.width = 8, repr.plot.height = 4)
p13 <- ggplot(top_occupation_views, aes(x=speaker_occupation, y=views, fill = speaker_occupation)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  ggtitle("Views based on Speaker Occupation") 
p13

# tags and themes
perfect_tag <- function(x){
  x <- unlist(strsplit(x, "'"))
  val = x[2]
  for (i in 3:length(x))
    if (nchar(x[i]) >2)
      val <- c(val, x[i])
  return (val)
  
}
ted$processed_tags <-  lapply(ted$tags, perfect_tag)

processed_tags <- ted$processed_tags
length(processed_tags)
processed_tags <- unlist(processed_tags, recursive=FALSE)
length(processed_tags)
processed_tags <- as.data.frame(table(processed_tags))
processed_tags <- arrange(processed_tags, desc(Freq))
head(processed_tags, 10)

processed_tags$processed_tags <- as.character(processed_tags$processed_tags)
processed_tags$processed_tags <- factor(processed_tags$processed_tags, levels=processed_tags$processed_tags)
p15 <- ggplot(data = head(processed_tags, 10), aes(processed_tags, Freq, fill = processed_tags)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = Freq), vjust = 1.6, color = "white", size = 3) +

  ggtitle("Most popular themes of Ted Talks")
p15
