library(tidyverse)
library(haven)
library(intsvy)
library(countrycode)

reverse_name <- function(x) setNames(names(x), as.character(x))

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
  !is.null(attr(x, "labels")) &&
    length(setdiff(names(attr(x, "labels")),
                   missing_labels)) >= 1))

# Sample 1 variable from the valid variables from subset_vars
# Combine it with the country variable and turn it all into a data.frame
valid_df <- as.data.frame(pisa2015[c("cnt", sample(names(subset_vars), 1))])

# Labels of the random variable from valid_df free of the missing labels
(test <- setdiff(names(attr(valid_df[, names(valid_df)[2], drop = T], "labels")), missing_labels))

# While the length of the test vector is > 4, sample a new variable.
# This is done because we don't want really long labels
while (length(test) > 4) {
  valid_df <- as.data.frame(pisa2015[c("cnt", sample(names(subset_vars), 1))])
  test <- setdiff(names(attr(valid_df[, names(valid_df)[2], drop = T], "labels")), missing_labels)
}

# Get the labels from the random variable
(labels <- reverse_name(attr(valid_df[, names(valid_df)[2], drop = T], 'labels')))

valid_df %>%
  filter(cnt %in% c("Brazil", "France", "Dominican Republic", "Colombia")) %>%
  pisa.table(names(valid_df)[2], data = ., by = "cnt") %>%
  mutate(st103q08na = labels[st103q08na]) %>%
  ggplot(aes(reorder(cnt, -Percentage), Percentage, fill = st103q08na)) +
  geom_col(position = "dodge") +
  labs(y = NULL, x = attr(valid_df$st103q08na, 'label')) +
  scale_fill_discrete(name = NULL)
  
