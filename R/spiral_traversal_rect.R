spiral_traversal_rect <- function(size, asp) {
  # Get pixel_order of pixles from spiral traversal algorithm
  ifelse(size %% 2 == 0,
         n_cols <- size + 1,
         n_cols <- size)
  n_rows <- round(n_cols * asp)
  ifelse(n_rows %% 2 == 1,
         n_rows <- n_rows + 1,
         n_rows <- n_rows)
  mid_c <- median(1:n_cols)
  mid_r <- median(1:n_rows)
  dif_c_r <- 0.5 * (n_rows - n_cols)
  pixel_order <- matrix(NA, nrow = (n_cols * n_rows), ncol = 2)
  dimnames(pixel_order) <- list(NULL, c("row", "col"))
  pointers <- c(mid_c - 1, mid_r - dif_c_r - 1, mid_r + dif_c_r + 1 ,mid_c + 1)
  names(pointers) <- c("left", "top", "bottom", "right")

  # Fill first (middle) column
  new_rows <- c((pointers["top"] + 1):(pointers["bottom"] - 1))
  new_cols <- rep_len(mid_c, length(new_rows))
  pixel_order[c(seq_len(length(new_rows))), 1] <- new_rows
  pixel_order[c(seq_len(length(new_cols))), 2] <- new_cols

  i <- as.numeric(pointers["bottom"] - pointers["top"] - 1)

  # Fill rest
  while (pointers["bottom"] <= n_rows && pointers["right"] <= n_cols) {
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
  pixel_order <- list(order = pixel_order, n_rows = n_rows, n_cols = n_cols)
  return(pixel_order)
}
