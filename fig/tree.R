# https://daranzolin.github.io/2018-01-07-probability-trees/

library(DiagrammeR)

bayes_probability_tree <- function(prior, true_positive, true_negative) {
  
  if (!all(c(prior, true_positive, true_negative) > 0) && !all(c(prior, true_positive, true_negative) < 1)) {
    stop("probabilities must be greater than 0 and less than 1.",
         call. = FALSE)
  }
  c_prior <- 1 - prior
  c_tp <- 1 - true_positive
  c_tn <- 1 - true_negative
  
  round4 <- purrr::partial(round, digits = 3)
  
  b1 <- round4(prior * true_positive * 1000)
  b2 <- round4(prior * c_tp * 1000)
  b3 <- round4(c_prior * c_tn * 1000)
  b4 <- round4(c_prior * true_negative * 1000)
  
  bp <-  round4(b1/(b1 + b3))
  
  labs <- c("Total", prior, c_prior, true_positive, c_tp, true_negative, c_tn, b1, b2, b4, b3)
  
  tree <-
    create_graph() %>%
    add_n_nodes(
      n = 11,
      type = "path",
      label = labs,
      node_aes = node_aes(
        shape = "circle",
        fontsize=15,
        height = 1,
        width = 1,
        x = c(0, 3, 3, 6, 6, 6, 6, 8.5, 8.5, 8.5, 8.5),
        y = c(0, 2, -2, 3, 1, -3, -1, 3, 1, -3, -1))) %>% 
    add_edge(
      from = 1,
      to = 2,
      edge_aes = edge_aes(
        label = "Prior",
        fontsize=15
      )
    ) %>% 
    add_edge(
      from = 1, 
      to = 3,
      edge_aes = edge_aes(
        label = "Complimentary Prior",
        fontsize=15
      )
    ) %>% 
    add_edge(
      from = 2,
      to = 4,
      edge_aes = edge_aes(
        label = "True Positive",
        fontsize=15
      )
    ) %>% 
    add_edge(
      from = 2,
      to = 5,
      edge_aes = edge_aes(
        label = "False Negative",
        fontsize=15
      )
    ) %>% 
    add_edge(
      from = 3,
      to = 7,
      edge_aes = edge_aes(
        label = "False Positive",
        fontsize=15
      )
    ) %>% 
    add_edge(
      from = 3,
      to = 6,
      edge_aes = edge_aes(
        label = "True Negative",
        fontsize=15
      )
    ) %>% 
    add_edge(
      from = 4,
      to = 8,
      edge_aes = edge_aes(
        label = "*1000*0.04 =",
        fontsize=14
      )
    ) %>% 
    add_edge(
      from = 5,
      to = 9,
      edge_aes = edge_aes(
        label = "*1000*0.04 =",
        fontsize=14
      )
    ) %>% 
    add_edge(
      from = 7,
      to = 11,
      edge_aes = edge_aes(
        label = "*1000*0.96 =",
        fontsize=14
      )
    ) %>% 
    add_edge(
      from = 6,
      to = 10,
      edge_aes = edge_aes(
        label = "*1000*0.96 =",
        fontsize=14
      )
    ) 
  message(glue::glue("The probability of having (prior) after testing positive is {bp}"))
  print(render_graph(tree))
  invisible(tree)
}
bayes_probability_tree(prior = 0.04, true_positive = 0.60, true_negative = 0.98)
