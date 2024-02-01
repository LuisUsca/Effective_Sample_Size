#' compute fishery length comp
#'
#' @param data length comp data
#' @param exp_meth compute length comps as 'marginal' or 'expanded' (default NULL)
#'
#' @return
#' @export lcomp
#'
#' @examples
lcomp <- function(data, exp_meth = NULL) {
  
  # compute marginal length comps ----
  if(exp_meth == 'marginal'){
    # combined sex 'total' length comps
    lcomp <- data %>% 
      tidytable::drop_na() %>% 
      tidytable::summarise(freq = sum(frequency), .by = c(year, length)) %>% # can also add a species grouping here
      tidytable::mutate(tot_freq = sum(freq),
                        lcomp = freq / tot_freq, .by = c(year)) %>% 
      tidytable::mutate(comp_type = 'total') %>% 
      tidytable::select(year, comp_type, length, lcomp)
    # sex-specific length comps option
    # tidytable::bind_rows(data %>% 
    #                        tidytable::filter(sex != 'unknown') %>% 
    #                        tidytable::summarise(freq = sum(frequency), .by = c(year, length, sex)) %>% 
    #                        tidytable::mutate(tot_freq = sum(freq),
    #                                          lcomp = freq / tot_freq, .by = c(year, sex)) %>% 
    #                        tidytable::select(year, sex, length, lcomp) %>% 
    #                        tidytable::rename(comp_type = 'sex'))
  }
  
  # compute expanded length comp, weighting length frequency by catch ----
  if(exp_meth == 'expanded'){

    # combined sex 'total' length comps
    lcomp <- data %>%
      tidytable::drop_na() %>% 
      tidytable::summarize(freq = sum(frequency), .by = c(year, total_weight, length)) %>% 
      tidytable::mutate(tot_freq = sum(freq), .by = c(year, total_weight)) %>% 
      tidytable::mutate(t_lcomp = freq / tot_freq, # compute trip specific length comps
                        len_extrap = t_lcomp * total_weight) %>%  # compute weighted length frequencies per haul
      tidytable::summarize(wtd_freq = sum(len_extrap), .by = c(year, length)) %>% 
      tidytable::mutate(tot_wtd_freq = sum(wtd_freq), .by = c(year)) %>% 
      tidytable::mutate(lcomp = wtd_freq / tot_wtd_freq, .by = c(year)) %>%   # compute catch weighted length composition
      tidytable::mutate(comp_type = 'total') %>%
      tidytable::select(year, comp_type, length, lcomp)
      # sex-specific length comps option
      # tidytable::bind_rows(data %>%
      #                        tidytable::drop_na() %>% 
      #                        tidytable::summarize(freq = sum(frequency), .by = c(year, total_weight, length, sex)) %>% 
      #                        tidytable::mutate(tot_freq = sum(freq), .by = c(year, total_weight, sex)) %>% 
      #                        tidytable::mutate(t_lcomp = freq / tot_freq, # compute trip specific length comps
      #                                          len_extrap = t_lcomp * total_weight) %>%  # compute weighted length frequencies per haul
      #                        tidytable::summarize(wtd_freq = sum(len_extrap), .by = c(year, length, sex)) %>% 
      #                        tidytable::mutate(tot_wtd_freq = sum(wtd_freq), .by = c(year, sex)) %>% 
      #                        tidytable::mutate(lcomp = wtd_freq / tot_wtd_freq, .by = c(year, sex)) %>%   # compute catch weighted length composition
      #                        tidytable::mutate(comp_type = 'total') %>%
      #                        tidytable::rename(comp_type = 'sex')) %>% 
      #                        tidytable::select(year, comp_type, length, lcomp)

  }
  
  lcomp
}