pisa_graph <- function(data, y_title, fill_var, length_labels) UseMethod("pisa_graph")

pisa_graph.labeltwo <- function(data, y_title, fill_var, length_labels) {
  
  dots <- setNames(list(interp(~ fct_reorder2(x, y, z),
                               x = quote(cnt),
                               y = as.name(fill_var),
                               z = quote(Percentage))), "cnt")
  
  data %>%
  mutate_(.dots = dots) %>%
  ggplot(aes(cnt, Percentage)) +
    geom_point(aes_string(colour = fill_var)) +
    labs(y = y_title, x = NULL) +
    scale_colour_discrete(name = NULL) +
    theme(legend.position = "top") +
    guides(colour = guide_legend(
      nrow = ifelse(length_labels <= 2, 1,
             ifelse(length_labels <= 4 & length_labels > 2, 2, 3)))) +
    coord_flip()
}

pisa_graph.labelthree <- function(data, y_title, fill_var, length_labels) {

  dots <- setNames(list(interp(~ fct_reorder2(x, y, z),
                               x = quote(cnt),
                               y = as.name(fill_var),
                               z = quote(Percentage))), "cnt")
  
  data %>%
    filter(cnt %in% sample(unique(cnt), 15)) %>%
    mutate_(.dots = dots) %>%
    ggplot(aes_string("cnt", fill_var)) +
    geom_point(aes_string(colour = fill_var,
                          size = "Percentage")) +
    labs(y = y_title, x = NULL) +
    scale_colour_discrete(guide = F) +
    scale_size_continuous(guide = F) +
    coord_flip()
}

pisa_graph.labelfour <- function(data, y_title, fill_var, length_labels) {
  pisa_graph.labelthree(data, y_title, fill_var, length_labels)
}
