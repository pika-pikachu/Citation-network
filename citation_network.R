# Resources:

## visnetwork documentation
# https://datastorm-open.github.io/visNetwork/
## rcrossref documentation
# https://cran.r-project.org/web/packages/rcrossref/rcrossref.pdf  
## rcrossref tutorial
# https://ropensci.org/tutorials/rcrossref_tutorial/

setwd("C:/Users/Vincent/Documents/Github/Citation-network")


# data <- read.csv("lens-export.csv")
data <- read.csv("bcp_math.csv", stringsAsFactors = F)
full_data <- read.csv("lens-export.csv", stringsAsFactors = F)

# explodes references into links
link_creator <- function(line){
  source_uuid <- as.character(line$Lens.ID)
  references <- c(sapply(strsplit(as.character(line$References), split = ";"), 
                        function(x) gsub(" ", "", x, fixed = TRUE)))
  if( line$References == "" ){
    n_ref <- 0
    return(matrix(0, nrow = 0, ncol = 2))
  } else {
    n_ref <- length(references)
    vec = matrix(0, nrow = n_ref, ncol = 2)
    vec[,1] <- source_uuid
    vec[,2] <- references
    return(vec)
  }
}

full_data$apa <- paste0(full_data$Author.s,". (",full_data$Publication.Year,")",". ",full_data$Title,". ", full_data$Source.Title)



# full test 
# n = 11277
# user  system elapsed 
# 197.47    25.07   223.86

x = matrix(0,0,2)
n <- dim(data)[1]
system.time(
  for(i in 1:n){
    link <- link_creator(data[i,])
    x <- rbind(x,link)
    #print(paste0("iteration ",i))
    cat("\r", "iteration ",i,", ",i / n*100,"% complete ")
  }
)

colnames(x) <- c("source","target")
x <- data.frame(x)


require(igraph)
require(visNetwork)

## fixed 
## https://igraph.org/r/doc/

graph <- graph.data.frame(x, directed = TRUE) 


table(degree(graph, mode = "in"))
table(degree(graph, mode = "out"))
table(degree(graph, mode = "all"))


V(graph)$degree <- degree(graph, mode = "in") 
graph_trim <- delete.vertices(graph, which(V(graph)$degree <= 2))

table(degree(graph_trim, mode = "all") )
V(graph_trim)$degree <- degree(graph_trim, mode = "all") 
graph_trim <- delete.vertices(graph_trim, which(V(graph_trim)$degree == 0))


nodes_trim <- get.data.frame(graph_trim, what="vertices")
nodes_trim <- data.frame(id = nodes_trim$name, title = nodes_trim$name)
nodes_trim <- nodes_trim[order(nodes_trim$id, decreasing = F),]

# size by the number of citations
nodes_trim$size <- degree(graph_trim, mode = "in")

nodes_trim <- merge(nodes_trim, full_data[,c("Lens.ID","apa", "Author.s", "DOI")], by.x = "id", by.y = "Lens.ID", all.x = T, all.y =F)
nodes_trim <- nodes_trim[,c("id","apa","size","Author.s","DOI")]
colnames(nodes_trim) <- c("id", "title","size","Author.s","DOI")



# colouring papers authored by Borovkov or Novikov
index <- sapply(nodes_trim$Author.s, function(x) grepl("Borovkov", x) | grepl("Novikov", x))
nodes_trim$group <- 0
nodes_trim$group[index] <- 1



edges_trim <- get.data.frame(graph_trim, what="edges")[1:2]

network_trim2_plot <- visNetwork(nodes_trim, edges_trim, height = "1080px", width = "1920px") %>%
  # visEdges(arrows = "to") %>%
  visOptions(highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  # visIgraphLayout(layout = "layout_in_circle") %>%
  visIgraphLayout(layout = "layout_nicely") %>% #defaults to FR
  # visIgraphLayout(layout = "layout_with_dh") %>% # slow
  # visIgraphLayout(layout = "layout_with_drl") %>% #banana shaped
  # visIgraphLayout(layout = "layout_with_fr") %>%
  # visIgraphLayout(layout = "layout_with_gem") %>% # slow, star like
  # visIgraphLayout(layout = "layout_with_graphopt") %>% # a messier FR
  # visIgraphLayout(layout = "layout_with_kk") %>% # FR, but with a hole in the middle
  # visIgraphLayout(layout = "layout_with_lgl") %>% # FRish
  # visIgraphLayout(layout = "layout_with_mds") %>% #looks good, but slower with big data set
  visInteraction(hover = TRUE, navigationButtons = TRUE) #%>% 
  # visIgraphLayout(layout = "layout_with_sugiyama") %>% # two sided, not useful?
  # visNodes(size = 10)

visSave(network_trim2_plot, file = "network_trim2.html", selfcontained = FALSE, background = "black")





# convert DOI to title

require(rcrossref)

z = cr_cn(dois = data$DOI[1:4], 
          format = "text", 
          style = "apa",
          .progress = "text",
          select = "title"
          )

cat(z = cr_cn(dois = data$DOI[1:4], format = "bibtex"))

