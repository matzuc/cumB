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
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(TL = c(1, 2.5, 3, 4), B = c(10, 20, 30, 40))
#' result <- cumulative_B(dat, "TL", "B", 2.4)
#' }
#' @export
cumBdata <- function(data, TL_col, B_col, threshold = 2.4) {
  library(dplyr)

  data %>%
    filter(!!sym(TL_col) > threshold) %>%
    arrange(!!sym(TL_col)) %>%
    mutate(cumB = cumsum(!!sym(B_col))) %>%
    mutate(cumBst = cumB / max(cumB))

}
