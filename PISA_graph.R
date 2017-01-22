library(tidyverse)
library(haven)
library(intsvy)
library(countrycode)



reverse_name <- function(x) setNames(names(x), as.character(x))
if (!is.null(attr(pisa_2015$st123q04na, "labels"))) {

var_attr <- attributes(pisa_2015$st123q04na)
label_changer <- reverse_name(attr(pisa_2015$st123q04na, "labels"))
pisa_2015$st123q04na <- label_changer[pisa_2015$st123q04na]

attributes(pisa_2015$st123q04na) <- var_attr

}


pisa_2015 <- read_spss("/Users/cimentadaj/Downloads/PISA/CY6_MS_CMB_STU_QQQ.sav")
names(pisa_2015) <- tolower(names(pisa_2015))

country_names <- names(attr(pisa_2015$cnt, "labels"))
names(country_names) <- attr(pisa_2015$cnt, "labels")
pisa_2015$cnt <- country_names[pisa_2015$cnt]


pisa_2015 %>%
  filter(cnt %in% c("Brazil", "France", "Dominican Republic", "Colombia")) %>%
  pisa.table("eat_breakfast", data = ., by = "cnt") %>%
  filter(eat_breakfast == 1) %>%
  ggplot(aes(cnt, Percentage)) +
  geom_col() +
  coord_flip()
  
