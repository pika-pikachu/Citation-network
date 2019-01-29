
setwd("C:/Users/Vincent/Downloads")
data <- read.csv("lens-export.csv")
?read.csv

head(data)
data$References[1]

unlist(strsplit(data$References[1], split=";"))

# splitting the reference field
strsplit(as.character(data$References[1]), split = ";")
sapply(strsplit(as.character(data$References[1]), split = ";"), function(x) gsub(" ", "", x, fixed = TRUE))



link_creator <- function(line){
  source_uuid <- as.character(line$Lens.ID)
  references <- sapply(strsplit(as.character(data$References[1]), split = ";"), 
                        function(x) gsub(" ", "", x, fixed = TRUE))
  n_ref <- dim(references)[1]
  vec = matrix(0, nrow = n_ref, ncol = 2)
  vec[,1] <- source_uuid
  vec[,2] <- references[,1]
  return(vec)
}


link_creator(data[1:4,])
data_trunc <- data[1:4,]
    
lapply(data_trunc, function(x) link_creator(x))

# trunc test
x = matrix(0,1,2)
n_trunc <- dim(data_trunc)[1] 
for(i in 1:n_trunc){
  link <- link_creator(data_trunc[i,])
  x <- rbind(x,link)
}

# full test 
# n = 11277
# user  system elapsed 
# 32.52    5.54   38.09

x = matrix(0,1,2)
n <- dim(data)[1]
system.time(
  for(i in 1:n){
    link <- link_creator(data[i,])
    x <- rbind(x,link)
  }
)





