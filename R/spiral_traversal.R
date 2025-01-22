#' Get Order of Matrix-Cell-Coordinates From Inside Out Using Spiral Traversal Algorithm for Square Matrices
#'
#' @param size Integer value specifying the dimensions of the matrix to be traversed
#'
#' @return A list with three elements, containing the coordinates of the matrix cells in
#' the spiral order as a 2 x (size^2) matrix; the number of rows as an integer value;
#' the number of columns as an integer value
#' @export
#'
#' @examples
#' spiral_traversal(5)
spiral_traversal <- function(size) {
  # Ensure that dimensions of matrix are odd
  ifelse(size %% 2 == 0,
         size <- size + 1,
         size <- size)

  # Get pixel_order of pixles from spiral traversal algorithm
  mid <- median(1:size)
  pixel_order <- matrix(mid, nrow = size^2, ncol = 2)
  dimnames(pixel_order) <- list(NULL, c("row", "col"))
  pointers <- c(mid - 1, mid - 1, mid + 1, mid + 1)
  names(pointers) <- c("left", "top", "bottom", "right")
  i <- 1

  while (pointers["bottom"] <= size && pointers["right"] <= size) {
    # left column, bottom -1 to top row
    new_rows <- c((pointers["bottom"] - 1):pointers["top"])
    new_cols <- rep_len(pointers["left"], length(new_rows))
    pixel_order[c(i + seq_len(length(new_rows))), 1] <- new_rows
    pixel_order[c(i + seq_len(length(new_cols))), 2] <- new_cols
    i <- i + length(new_rows)
    # top row, left +1 to right column
    new_cols <- c((pointers["left"] + 1):pointers["right"])
    new_rows <- rep_len(pointers["top"], length(new_cols))
    pixel_order[c(i + seq_len(length(new_rows))), 1] <- new_rows
    pixel_order[c(i + seq_len(length(new_cols))), 2] <- new_cols
    i <- i + length(new_rows)
    # right column, top +1 to bottom row
    new_rows <- c((pointers["top"] + 1):pointers["bottom"])
    new_cols <- rep_len(pointers["right"], length(new_rows))
    pixel_order[c(i + seq_len(length(new_rows))), 1] <- new_rows
    pixel_order[c(i + seq_len(length(new_cols))), 2] <- new_cols
    i <- i + length(new_rows)
    # bottom row, right -1 to left column
    new_cols <- c((pointers["right"] - 1):pointers["left"])
    new_rows <- rep_len(pointers["bottom"], length(new_cols))
    pixel_order[c(i + seq_len(length(new_rows))), 1] <- new_rows
    pixel_order[c(i + seq_len(length(new_cols))), 2] <- new_cols
    i <- i + length(new_rows)

    pointers[1:2] <- pointers[1:2] - 1
    pointers[3:4] <- pointers[3:4] + 1
  }
  pixel_order <- list(order = pixel_order, n_rows = size, n_cols = size)
  return(pixel_order)
}
