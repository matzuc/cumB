#' Calculate Cumulative Biomass
#'
#' This function calculates the cumulative biomass for given data, filtering rows based on a threshold
#' for the TL column, and then arranging and creating cumulative values.
#'
#' @param data A dataframe containing the data.
#' @param TL_col A character string specifying the column name representing TL.
#' @param B_col A character string specifying the column name representing B.
#' @param threshold A numeric value to filter rows based on the TL column.
#'
#' @return A dataframe with rows filtered, arranged, and with columns `cumB` and `cumBst` added.
#' `cumB` contains the cumulative values of the B column, and `cumBst` contains the standardized cumulative values of the B column.
#' #' @importFrom dplyr filter arrange mutate
#' #' @importFrom ggplot2 filter arrange mutate
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(TL = c(1, 2.5, 3, 4), B = c(10, 20, 30, 40))
#' result <- cumulative_B(dat, "TL", "B", 2.4)
#' }
#' @export
cumBdata <- function(data, TL_col, B_col, threshold = 2.4) {
  library(dplyr)

  result <- data |>
    filter(!!sym(TL_col) > threshold) |>
    arrange(!!sym(TL_col)) |>
    rename(TL = !!sym(TL_col), B = !!sym(B_col)) |>
    mutate(cumB = cumsum(B)) |>
    mutate(cumBst = cumB / max(cumB))

  class(result) <- c("cumBdata_class", class(result))
  return(result)
}




# Method for printing the object
print.cumBdata_class <- function(data, ...) {
  cat("cumBdata_class Object\n")
  cat("Number of species/groups: ", nrow(data), "\n")
  cat("TL range: ", paste(min(data[, "TL"]),
                              "-",
                          max(data[, "TL"]),
                          "; TL threshold at: " , threshold),  "\n")

}



summary.cumBdata_class <- function(object, ...) {
  # Define the columns you're summarizing
  required_cols <- c("TL", "B", "cumB", "cumBst")

  # Identify missing required columns to ensure they exist
  missing_cols <- setdiff(required_cols, names(object))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse=", "))
  }

  # Compute summaries for the specified columns
  summaries <- lapply(object[, required_cols], summary)

  # Print the summary for each required column
  cat("Summary of cumBdata_class Object\n")
  for (col in names(summaries)) {
    cat("\nSummary of ", col, ":\n")
    print(summaries[[col]])
  }

  # Identify and list the unused columns
  unused_cols <- setdiff(names(object), required_cols)
  if (length(unused_cols) > 0) {
    cat("\nColumns not used in this summary: ", paste(unused_cols, collapse=", "), "\n")
  } else {
    cat("\nAll columns were used in the summary.\n")
  }

  invisible(summaries)
}



# Method for the basic plots

plot_cumBdata_class <- function(x,  ...) {
  # Verify that the necessary columns exist
  if (!("TL" %in% names(x) && "cumBst" %in% names(x))) {
    stop("One or more required columns are missing from the dataframe.")
  }

  # Prepare the expressions for ggplot
  TL_sym <- rlang::sym(TL_col)

  # Create the ggplot using tidy evaluation
  p <- ggplot(x, aes(x = TL, y = cumBst)) +
    geom_line() +
    geom_point() +
    labs(x = TL_col, y = "Standardized Cumulative Biomass",
         title = paste("Relationship between", TL_col, "and Standardized Cumulative Biomass")) +
    theme_minimal()

  print(p)
}
