library(tidyverse)
library(forcats)
library(haven)
library(intsvy)
library(countrycode)
library(cimentadaj)
library(lazyeval)

# Function recevies a variable with an attribute label
# and recodes the variable to availabe lables
# Returns the same variable with new codings
variable_labeller <- function(variable) {
  
  if (!is.null(attr(variable, "labels"))) {
    
    var_attr <- attributes(variable)
    label_changer <- reverse_name(attr(variable, "labels"))
    variable <- label_changer[variable]
    
    attributes(variable) <- var_attr
    variable
  }
}

pisa_2015 <- read_spss("/Users/cimentadaj/Downloads/PISA/CY6_MS_CMB_STU_QQQ.sav")
pisa2015 <- pisa_2015
names(pisa2015) <- tolower(names(pisa2015))
pisa2015$region <- countrycode(pisa2015$cnt, "iso3c", "continent")

# Saving country names and their equivalency
country_attributes <- attr(pisa2015$cnt, "labels")

# Reversing the 3 CHR code to names so I can search for countries
# in a lookup table
country_names <- reverse_name(country_attributes)

# Lookup 3 CHR code and change them for long country names
pisa2015$cnt <- country_names[pisa2015$cnt]
attr(pisa2015$cnt, "labels") <- country_attributes

# Labels I don't want to use
missing_labels <- c("Valid Skip",
                    "Not Reached",
                    "Not Applicable",
                    "Invalid",
                    "No Response")

# Get variables which have a 'labels' attribute
# and have other labels besides the missing_labels
# vector

subset_vars <- which(map_lgl(pisa2015, function(x)
  !is.null(attr(x, "labels")) && length(setdiff(names(attr(x, "labels")), missing_labels)) >= 1))

# Sample 1 variable from the valid variables from subset_vars
# Combine it with the country variable and turn it all into a data.frame
valid_df <- as.data.frame(pisa2015[c("cnt", "region", sample(names(subset_vars), 1))])

random_countries <- unique(valid_df$cnt)

# Labels of the random variable from valid_df free of the missing labels
(test <- setdiff(names(attr(valid_df[, names(valid_df)[3], drop = T], "labels")), missing_labels))

# While the length of the test vector is > 4, sample a new variable.
# This is done because we don't want really long labels
while (length(test) > 4) {
  valid_df <- as.data.frame(pisa2015[c("cnt", "region", sample(names(subset_vars), 1))])
  test <- setdiff(names(attr(valid_df[, names(valid_df)[3], drop = T], "labels")), missing_labels)
}

# Get the labels from the random variable
(labels <- reverse_name(attr(valid_df[, names(valid_df)[3], drop = T], 'labels')))
var_name <- names(valid_df)[3]

try_df <-
  valid_df %>%
  filter(!is.na(region)) %>%
  pisa.table(var_name, data = ., by = "cnt") %>%
  filter(complete.cases(.))

try_df[var_name] <- labels[try_df[, var_name]]

len_labels <- length(unique(try_df[, var_name]))


## Section: Get the title
title_question <- attr(valid_df[, var_name], 'label') # Title
cut <- 60 # Arbitrary cutoff

# This function accepts a sentence (or better, a title) and cuts it between
# the start and cutoff arguments ( just as substr). But if the cutoff is not an empty space
# it will search +-1 index by index from the cutoff point until it reaches
# the closest empty space. It will return from start to the new cutoff
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
sentence_vecs <- ceiling(nchar(title_question) / cut)
list_excerpts <- replicate(sentence_vecs, vector("character", 0))

# Create an empty list with the amount of lines for the excerpts
# to be stored.

for (list_index in seq_along(list_excerpts)) {
  non_empty_list <- Filter(f = function(x) !(is_empty(x)), list_excerpts)
  
  start <- ifelse(list_index == 1, 1, sum(map_dbl(non_empty_list, nchar)))
  
  list_excerpts[[list_index]] <-
    sentence_cut(title_question, start, ifelse(list_index == 1, cut, start + cut))
}

final_title <- paste(list_excerpts, collapse = "\n")

(first_graph <-
  try_df %>%
  ggplot(aes(fct_reorder2(cnt, scchange, Percentage), Percentage)) +
  geom_point(aes_string(colour = var_name)) +
  labs(y = final_title, x = NULL) +
  scale_colour_discrete(name = NULL) +
  theme(legend.position = "top") +
  scale_y_continuous(labels = paste0(seq(0, 100, 10), "%"), breaks = seq(0, 100, 10)) +
  guides(colour = guide_legend(nrow = ifelse(len_labels <= 2, 1,
                                    ifelse(len_labels <= 4 & len_labels > 2, 2, 3)))) +
  coord_flip())

setwd("/Users/cimentadaj/Downloads/twitter")
ggsave("first_graph.png")

# devtools::install_github("geoffjentry/twitteR")
library(twitteR)

api_key             <- Sys.getenv("twitter_api_key")
api_secret          <- Sys.getenv("twitter_api_secret")
access_token        <- Sys.getenv("twitter_access_token")
access_token_secret <- Sys.getenv("twitter_access_token_secret")
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

tweet("", mediaPath = "./first_graph.png")

# to automate
# https://www.r-bloggers.com/programming-a-twitter-bot-and-the-rescue-from-procrastination/
