library(tidyverse)
library(forcats)
library(haven)
library(intsvy)
library(countrycode)
library(cimentadaj)
library(lazyeval)

# # Function recevies a variable with an attribute label
# # and recodes the variable to availabe lables
# # Returns the same variable with new codings
# variable_labeller <- function(variable) {
#   
#   if (!is.null(attr(variable, "labels"))) {
#     
#     var_attr <- attributes(variable)
#     label_changer <- reverse_name(attr(variable, "labels"))
#     variable <- label_changer[variable]
#     
#     attributes(variable) <- var_attr
#     variable
#   }
# }

pisa_2015 <- read_spss("/Users/cimentadaj/Downloads/PISA/PISA2015/CY6_MS_CMB_STU_QQQ.sav")

country_var <- "cnt" # country variable name in lower case
country_types <- "iso3c" # type of country name
missing_labels <- c("Valid Skip",
                    "Not Reached",
                    "Not Applicable",
                    "Invalid",
                    "No Response") # Labels I want to ignore

int_data <- pisa_2015
names(int_data) <- tolower(names(int_data))
int_data$region <- countrycode(int_data[[country_var]], country_types, "continent")

# Saving country names and their equivalency
country_labels <- attr(int_data[[country_var]], "labels")

# Reversing the 3 CHR code to names so I can search for countries
# in a lookup table
country_names <- reverse_name(country_labels)

# Lookup 3 CHR code and change them for long country names
int_data[, country_var] <- country_names[int_data[[country_var]]]
attr(int_data[[country_var]], "labels") <- country_labels

# Get variables which have a 'labels' attribute
# and have other labels besides the missing_labels
# vector

subset_vars <- 
  int_data %>%
  map_lgl(function(x)
    !is.null(attr(x, "labels")) &&
    length(setdiff(names(attr(x, "labels")), missing_labels)) >= 1) %>%
  which()

# Sample 1 variable from the valid variables from subset_vars
# Combine it with the country variable and turn it all into a data.frame
valid_df_fun <- function(data, vars_select) {
  data %>%
  select_("cnt", "region", sample(names(vars_select), 1)) %>%
  as.data.frame()
}

valid_df <- valid_df_fun(int_data, subset_vars)

random_countries <- unique(valid_df$cnt)

# Labels of the random variable from valid_df free of the missing labels
var_labels <- attr(valid_df[, names(valid_df)[3], drop = T], 'labels') # Get labels

# Get unique labels
valid_labels <- function(variable_label, miss) {
  variable_label %>%
    names() %>%
    setdiff(miss)
}

len_labels <- length(valid_labels(var_labels, missing_labels)) # length of unique labels

# While the length of the test vector is > 4, sample a new variable.
# This is done because we don't want a lot of labels
while (len_labels > 4) {
  valid_df <- valid_df_fun(int_data, subset_vars)
  var_labels <- attr(valid_df[, names(valid_df)[3], drop = T], 'labels')
  len_labels <- length(valid_labels(var_labels, missing_labels))
}

# Make 100% sure we get the results:
stopifnot(len_labels <= 4)

(labels <- reverse_name(var_labels)) 
# Reverse vector names to objects and viceversa for 
# later recoding.

var_name <- names(valid_df)[3]

try_df <-
  valid_df %>%
  filter(!is.na(region)) %>%
  pisa.table(var_name, data = ., by = "cnt") %>%
  filter(complete.cases(.))

try_df[var_name] <- labels[try_df[, var_name]]

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

# Create an empty list with the amount of lines for the excerpts
# to be stored.
list_excerpts <- replicate(sentence_vecs, vector("character", 0))

for (list_index in seq_along(list_excerpts)) {
  
  non_empty_list <- Filter(f = function(x) !(is_empty(x)), list_excerpts)
  
  # If this is the first line, the start should 1, otherwise the sum of all characters
  # of previous lines
  start <- ifelse(list_index == 1, 1, sum(map_dbl(non_empty_list, nchar)))
  
  # Because start gets updated every iteration, simply cut from start to start + cut
  # The appropriate exceptions are added when its the first line of the plot.
  list_excerpts[[list_index]] <-
    sentence_cut(title_question, start, ifelse(list_index == 1, cut, start + cut))
}


final_title <- paste(list_excerpts, collapse = "\n")

label_class <-
  c("2" = "labeltwo", '3' = "labelthree", '4' = "labelfour")[as.character(len_labels)]

class(try_df) <- c(class(try_df), label_class)

source("./ggplot_funs.R")
(first_graph <-
  pisa_graph(data = try_df,
             y_title = final_title,
             fill_var = var_name,
             length_labels = len_labels))

# TODO: You need to cut legend labels as you cut the
# final_title

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
