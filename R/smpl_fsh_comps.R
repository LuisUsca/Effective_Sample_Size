#' primary length bootstrap function to compute comps
#'
#' @param data length frequency and catch data
#' @param yrs year filter returns years >= (default = NULL)
#' @param bin length bin size (default = 1 cm)
#' @param exp_meth compute age/length comps as 'marginal' or 'expanded' (default NULL)
#' @param boot_trip switch for resampling primary sampling unit, like hauls or trip (default = FALSE)
#' @param boot_lengths switch for resampling lengths (default = FALSE)
#'
#' @return
#' @export smpl_fsh_comps
#'
#' @examples
#' 

smpl_fsh_comps <- function(data, yrs = NULL, bin = 1, exp_meth = NULL, boot_trip = FALSE, boot_lengths = FALSE) {
  # globals ----
  # year switch
  if (is.null(yrs)) yrs <- 0
  
  # prep data ----
  # first pass of filtering

  data.table::setDT(data) %>%
      tidytable::drop_na() %>% 
      tidytable::filter(year >= yrs) %>% # filter years to be included 
      # add a trip ID for unique total weights, month, and day
      tidytable::mutate(ident = paste0(month, day, total_weight)) %>% 
      tidytable::mutate(trip_id = cur_group_id(), .by = c(ident)) %>% 
      tidytable::select(-ident) -> .lfreq
  
  .lfreq %>% 
    tidytable::uncount(frequency) -> .lfreq_un # extend out data for each individual length
  
  # add a trip ID for unique total weights, month, and day
  

  # randomize primary sampling unit (hauls/trips) ----  
  if(isTRUE(boot_trip)) {
    boot_prime(.lfreq)  %>% 
      tidytable::left_join(.lfreq_un) -> .lfreq_un_prime
  } else{
    .lfreq_un_prime <- .lfreq_un
  }
  
  # randomize lengths ----
  if(isTRUE(boot_lengths)) {
    boot_length(.lfreq_un_prime) -> .lfreq_un_prime_len
  } else{
    .lfreq_un_prime -> .lfreq_un_prime_len
  }
  
  # bin length data ----
  .lfreq_un_prime_len %>% 
    tidytable::mutate(length = bin * ceiling(length / bin)) -> .lfreq_un__prime_len_bin
  
  # length comp ----
  
  # clean data and compute length comp
  .lfreq_un__prime_len_bin %>%
      tidytable::summarise(frequency = tidytable::n(), .by = c(year, trip_id, total_weight, species, length)) -> .lfreq_samp

  lcomp(data = .lfreq_samp, exp_meth = exp_meth) -> .lcomp

  list(length = .lcomp)
  
}