make_markov_art <- function(
    size = 101, # size of the art piece; values > 1000 are not recomendet
    asp = 1, # aspect ratio of piece; has to be >= 1
    seed = NULL, # the random seed for the Markov chain
    save = FALSE # should the piece be saved as a .png?
) {

  # Get pixel_order of pixles from spiral traversal algorithm
  if (asp == 1) {
    pixel_order <- spiral_traversal(size)
  } else if (asp > 1) {
    pixel_order <- spiral_traversal_rect(size, asp)
  } else {
    stop("Aspect ratio must be greater than or equal to 1!")
  }

  # Initiate matrix to be filled
  n_rows <- pixel_order$n_rows
  n_cols <- pixel_order$n_cols
  markov_art <- matrix(NA, nrow = n_rows, ncol = n_cols)

  # Generate Markov chain
  set.seed(seed)
  wiener <- cumsum(c(0, rnorm((n_rows * n_cols) - 1, mean = 0, sd = 1)))

  # Fill matrix in appropriate pixel_order
  for (i in 1:(n_rows * n_cols)) {
    markov_art[pixel_order$order[i, 1], pixel_order$order[i, 2]] <- wiener[i]
  }
  return(markov_art)
}
