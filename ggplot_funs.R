pisa_graph <- function(data, y_title, fill_var) UseMethod("pisa_graph")

#' Dispatch method for labeltwo class
#'
#' @param data Dataframe with labeltwo class
#' @param y_title The title for the y variable
#' @param fill_var The variable with which to fill the plot
#'
#' @return A plot
#' @export
#'
#' @examples
pisa_graph.labeltwo <- function(data, y_title, fill_var) {
  
  dots <- setNames(list(interp(~ fct_reorder2(x, y, z),
                               x = quote(cnt),
                               y = as.name(fill_var),
                               z = quote(Percentage))), "cnt")
  unique_cnt <- length(unique(data$cnt))
  
  data %>%
    filter(cnt %in% sample(unique(cnt), ifelse(unique_cnt >= 15, 15, 10))) %>%
    mutate_(.dots = dots) %>%
    ggplot(aes(cnt, Percentage)) +
    geom_point(aes_string(colour = fill_var)) +
    labs(y = y_title, x = NULL) +
    scale_colour_discrete(name = NULL) +
    guides(colour = guide_legend(
      nrow = 1)) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "top")
}

#' Dispatch method for labelthree class
#'
#' @param data Dataframe with labelthree class
#' @param y_title The title for the y variable
#' @param fill_var The variable with which to fill the plot
#'
#' @return A plot
#' @export
#'
#' @examples
pisa_graph.labelthree <- function(data, y_title, fill_var) {

  dots <- setNames(list(interp(~ fct_reorder2(x, y, z),
                               x = quote(cnt),
                               y = as.name(fill_var),
                               z = quote(Percentage))), "cnt")
  
  data %>%
    filter(cnt %in% sample(unique(cnt), ifelse(unique_cnt >= 15, 15, 10))) %>%
    mutate_(.dots = dots) %>%
    ggplot(aes_string("cnt", fill_var)) +
    geom_point(aes_string(colour = fill_var,
                          size = "Percentage")) +
    labs(y = y_title, x = NULL) +
    scale_colour_discrete(guide = F) +
    scale_size_continuous(guide = F) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "top")
}

#' Dispatch method for labelfour class
#'
#' @param data Dataframe with labelfour class
#' @param y_title The title for the y variable
#' @param fill_var The variable with which to fill the plot
#'
#' @return A plot
#' @export
#'
#' @examples
pisa_graph.labelfour <- function(data, y_title, fill_var) {
  pisa_graph.labelthree(data, y_title, fill_var)
}
