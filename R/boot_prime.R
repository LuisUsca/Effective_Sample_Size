#' resample primary sampling unit w/replacement
#'
#' @param data dataframe within which to resample primary sampling unit (i.e., haul or trip)
#'
#' @return
#' @export boot_prime
#'
#' @examples
boot_prime <- function(data) {
  
  data %>% 
    tidytable::select(year, species, trip_id) %>% 
    tidytable::distinct() %>% 
    tidytable::mutate(trip_id = sample(trip_id, .N, replace = TRUE),
                      .by = c(year, species))
}