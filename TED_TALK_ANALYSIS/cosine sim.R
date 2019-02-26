install.packages('stm')
library(stm)
library(visNetwork)
top_talker <- ted_top %>% group_by(main_speaker) %>% summarise(n_views = n()) %>%arrange(desc(n_views))
# pre-process text
transc_meta <- ted_top %>% select(-transcript) # every variable other than the text

process_text <- textProcessor(ted_top_t)

process_text$docs.removed # empty docs are removed

output <- prepDocuments(process_text$documents, process_text$vocab, process_text$meta, lower.thresh = 3, upper.thresh = 2000) 
output$docs.removed # empty docs are removed
# Fit a topic model, K = number of topics
# stm comes with data-driven methods to select K (?searchK)
n_keyword <- 20
stm_fit1 <- stm(documents = output$documents, vocab = output$vocab, K = n_keyword, max.em.its = 40) # you should probably increase max.em.its or let it converge 

# examine the topics (we may not get the same results due to the random initialization)
labelTopics(stm_fit1)
cloud(stm_fit1, topic = 1)
cloud(stm_fit1, topic = 5)

dev.off()
par(mfrow = c(2, 4),mar = c(.5, .5, 1, .5))
transcripts1 <- ted_top_t[-process_text$docs.removed]


z <- ted_top_t[-process_text$docs.removed]
#Problem
for(k in 1:n_keyword){
  thoughts = findThoughts(stm_fit1, texts = z, topics = k, n = 1)$docs[[1]]
  plotQuote(stringi::stri_unescape_unicode(thoughts), main = paste("keywords ", k))
}


# construct similary network of artists
keyword_proportions <- stm_fit1$theta
colnames(keyword_proportions) <- paste("keyword", 1:n_keyword, sep = "")
transc_meta <- transc_meta[-process_text$docs.removed, ]



transc_meta <- cbind(transc_meta, keyword_proportions)

dim(transc_meta)
dim(keyword_proportions)

head(transc_meta)
head(talker_keyword)
transc_meta[1,]

talker_keyword <- transc_meta %>% group_by(main_speaker) %>% summarise_at(vars(starts_with("keyword")), mean)
talker_keyword <- talker_keyword %>% filter(main_speaker %in% top_talker$main_speaker[1:20])

# calculate cosine similarity between artists
install.packages('lsa')
library('lsa')

cosine_sim1 <- cosine(t(as.matrix(talker_keyword[,2:9])))
# euclidean distance
sim1 <- as.matrix(1/dist(as.matrix(talker_keyword[,2:9])))
sim1
# from similarity to adj matrix method 1. Using threshold
adj_matrix1 <- sim1 > 5
colnames(adj_matrix1) <- talker_keyword$main_speaker
rownames(adj_matrix1) <- talker_keyword$main_speaker
talker_graph <- graph_from_adjacency_matrix(adj_matrix1, mode = "undirected")
talker_graph <- simplify(talker_graph)

dev.off()
talker_graph <- delete.vertices(talker_graph,"Graham Hill")
plot(talker_graph, vertex.size = 0, layout = layout_nicely(talker_graph))
V(talker_graph)

vis_talker <- toVisNetworkData(talker_graph)
head(vis_talker$nodes)
head(vis_talker$edges)
visNetwork(
  nodes = vis_talker$nodes, 
  edges = vis_talker$edges, 
  width = 600, 
  height = 600
) %>%
visIgraphLayout(layout = "layout_in_circle")
