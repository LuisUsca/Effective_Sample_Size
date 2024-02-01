#' resample length data w/replacement
#'
#' @param lfreq_un expanded length frequency data 
#'
#' @return
#' @export boot_length
#'
#' @examples
boot_length <- function(lfreq_un) {
  
  lfreq_un %>%
    tidytable::mutate(length = sample(length, .N, replace = TRUE), 
                      .by = c(year, species, trip_id))
}