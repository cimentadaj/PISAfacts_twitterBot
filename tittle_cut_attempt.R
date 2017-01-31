
cut <- 50

# sentence <- title
# start <- 1
# cutoff <- 40

# This function accepts a sentence (or better, a title) and cuts it between
# start and cutoff ( just as substr). But if the cutoff is not an empty space
# it will search +-1 index by index from the cutoff point until it reaches
# an empty space. It will return from start to the new cutoff
sentence_cut <- function(sentence, start, cutoff) {
  
  if (cutoff == nchar(sentence)) return(substr(sentence, start, cutoff))
  
  excerpt <- substr(sentence, start, cutoff)
  actual_val <- cutoff
  neg_val <- pos_val <- actual_val
  
  if (!substr(excerpt, actual_val, actual_val) == " ") {
    
    expr <- c(substr(title, neg_val, neg_val) == " ", substr(title, pos_val, pos_val) == " ")
    
    while (!any(expr)) {
    neg_val <- neg_val - 1
    pos_val <- pos_val + 1
    
    expr <- c(substr(title, neg_val, neg_val) == " ", substr(title, pos_val, pos_val) == " ")
    }
    
    cutoff <- ifelse(which(expr) == 1, neg_val, pos_val)
    excerpt <- substr(title, start, cutoff)
    return(excerpt)
    
  } else {
    
    return(excerpt)
    
  }
}

# How many lines should this new title have? Based on the cut off
sentence_vecs <- round(nchar(title) / cut, 0)
list_excerpts <- replicate(sentence_vecs, vector("character", 0))

# Create an empty list with the amount of lines for the excerpts
# to be stored.

test <- sentence_cut(title, 1, cut)
test2 <- sentence_cut(title, nchar(test), 90)
test3 <- sentence_cut(title, nchar(test) + nchar(test2), 90)

ggplot(mtcars, aes(mpg, cyl)) + geom_point() + labs(x = paste(test, test2, sep = "\n"))

# Experimental section. Trying to dynamically divide the sentence into sentence_vec chunks
# and fill them up with the corresponding chunks, just as in the previous example where 3
# vectors where needed.

# THe ideal would be dynamically populate the list excerpts with the test objects from above
for (x in seq_len(sentence_vecs)) {
  sentence_cut(title, ifelse(x == 1, 1, nchar(list_excerpts[[x - 1]])),
                      ifelse(x == length(list_excerpts), nchar(title), cut <- cut + 20))
}

list_excerpts[[1]] <- sentence_cut(title, 1, cut)
list_excerpts[[2]] <- sentence_cut(title, nchar(list_excerpts[[1]]), nchar(title))

x <- 1
test <- sentence_cut(title, ifelse(x == 1, 1, nchar(list_excerpts[[x - 1]])),
             ifelse(x == length(list_excerpts), nchar(title), cut))

