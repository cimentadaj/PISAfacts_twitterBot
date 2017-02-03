cut <- 60

# sentence <- title_question
# start <- 1
# cutoff <- cut

# This function accepts a sentence (or better, a title) and cuts it between
# start and cutoff ( just as substr). But if the cutoff is not an empty space
# it will search +-1 index by index from the cutoff point until it reaches
# an empty space. It will return from start to the new cutoff
sentence_cut <- function(sentence, start, cutoff) {
  
  if (nchar(sentence) <= cutoff) return(substr(sentence, start, cutoff))
  
  excerpt <- substr(sentence, start, cutoff)
  actual_val <- cutoff
  neg_val <- pos_val <- actual_val
  
  if (!substr(excerpt, actual_val, actual_val) == " ") {
    
    expr <- c(substr(sentence, neg_val, neg_val) == " ", substr(sentence, pos_val, pos_val) == " ")
    
    while (!any(expr)) {
    neg_val <- neg_val - 1
    pos_val <- pos_val + 1
    
    expr <- c(substr(sentence, neg_val, neg_val) == " ", substr(sentence, pos_val, pos_val) == " ")
    }
    
    cutoff <- ifelse(which(expr) == 1, neg_val, pos_val)
    excerpt <- substr(sentence, start, cutoff)
    return(excerpt)
    
  } else {
    
    return(excerpt)
    
  }
}

# How many lines should this new title have? Based on the cut off
sentence_vecs <- round(nchar(title_question) / cut, 0)
list_excerpts <- replicate(sentence_vecs, vector("character", 0))

# Create an empty list with the amount of lines for the excerpts
# to be stored.

# list_excerpts[[1]] <- sentence_cut(title_question, 1, cut)
# list_excerpts[[2]] <- sentence_cut(title_question, nchar(list_excerpts[[1]]), nchar(list_excerpts[[1]]) + cut)

for (list_index in seq_along(list_excerpts)) {
  non_empty_list <- Filter(f = function(x) !(is_empty(x)), list_excerpts)
  
  start <- ifelse(list_index == 1, 1, sum(map_dbl(non_empty_list, nchar)))
  
  list_excerpts[[list_index]] <-
    sentence_cut(title_question, start, ifelse(list_index == 1, cut, start + cut))
}

final_title <- paste(list_excerpts, collapse = "\n")