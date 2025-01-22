#' Make a Matrix of Values as a Basis for Random Artpiece Based on Very Simple Markov Chain
#'
#' @param size Integer value > 1 specifying the dimensions of the matrix to be traversed
#' @param asp If rectangular canvas is desired, integer value > 1 specifying the aspect ratio of output matrix.
#' This determines the number of columns by size * asp
#' @param seed Random seed for the Markov Chain
#'
#' @return A matrix of integer values
#' @export
#'
#' @examples
#' make_markov_art(size = 11, asp = 1, seed = 42)
make_markov_art <- function(
    size = 101, # size of the art piece; values > 1000 are not recomendet
    asp = 1, # aspect ratio of piece; has to be >= 1
    seed = NULL, # the random seed for the Markov chain
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
