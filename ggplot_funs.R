pisa_graph <- function(data, x, y) UseMethod("pisa_graph")

pisa_graph.labeltwo <- function(df, x, y, colour_var) {
  print(expr_text(eval(colour_var)))
  print(expr_text(x))
  print(expr_text(y))
  ggplot(aes_string(expr_text(x), expr_text(y))) +
    geom_point(aes_string(colour = expr_text(colour_var)))
}

class(try_df) <- c("labeltwo", class(try_df))


pisa_graph(try_df, cnt, Percentage, var_name)

class(mtcars) <- c("labeltwo", class(mtcars))



try_df %>%
  mutate_(.dots = dots) %>%
  ggplot(aes(cnt, Percentage)) +
  geom_point(aes_string(colour = var_name)) +
  labs(y = final_title, x = NULL) +
  scale_colour_discrete(name = NULL) +
  theme(legend.position = "top") +
  scale_y_continuous(labels = paste0(seq(0, 100, 10), "%"),
                     breaks = seq(0, 100, 10),
                     limits = c(0, 100)) +
  guides(colour = guide_legend(nrow = ifelse(len_labels <= 2, 1,
                                             ifelse(len_labels <= 4 & len_labels > 2, 2, 3)))) +
  coord_flip()
