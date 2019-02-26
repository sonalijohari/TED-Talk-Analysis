get_related_pairs <- function(ix, df){
  row <- df[ix, ]
  self_title <- as.character(row$title)
  related <- row$related_talks
  s <- as.character(related)
  bits <- strsplit(s, ",")
  rel <- c()
  for(b in bits[[1]]){
    #print(b)
    if(length(grep("'title'", b))>0){
      b <- strsplit(b, ": ")[[1]]
      title <- noquote(b[[2]])
      title <- gsub("'", "", title)
      title <- gsub("\"", "", title)
      rel <- c(rel, noquote(title))
    }
  }
  return(data.frame(source=rep(self_title, length(rel)), target=rel))
}

# Get the related pairs for all talks
related <- lapply(1:nrow(ted), function(x) get_related_pairs(x, ted))
# Flatten into a big data frame with pairs
all_related <- Reduce(function(a, b) rbind(a, b), related)
# Remove duplicates
all_related <- all_related[-which(duplicated(all_related)),]
# Make igraph graph
graph <- graph_from_edgelist(as.matrix(all_related), directed=FALSE)
graph_df <- graph.data.frame(layout_nicely(graph))
head(all_related)
V(graph_df)[[1:5]]
E(graph_df)[[1:5]]


# Count number of Edges
gsize(graph_df)
# Count number of vertices
gorder(graph_df)
