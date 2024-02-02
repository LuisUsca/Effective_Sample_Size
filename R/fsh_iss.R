#' replicate fishery input sample size function
#'
#' @param iters number of iterations (500 recommended)
#' @param data  input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param exp_meth compute length comps as 'marginal' or 'expanded' (default NULL)
#' @param boot_trip resample primary sampling unit w/replacement (default = FALSE)
#' @param boot_lengths resample lengths w/replacement (default = FALSE)
#' @param save name to save output
#'
#' @return
#' @export fsh_iss
#'
#' @examples

fsh_iss <- function(iters = 1, data, yrs = NULL, bin = 1, 
                    exp_meth, boot_trip = FALSE, boot_lengths = FALSE, save){
  
  # create storage location
  if(!dir.exists(here::here('output'))){
    dir.create(here::here('output'), recursive = TRUE)
  }

  # restructure data
  data <- tidytable::as_tidytable(data) 
  
  # get original age/length pop'n values
  og <- smpl_fsh_comps(data, 
                       yrs, 
                       bin, 
                       exp_meth,
                       boot_trip = FALSE, 
                       boot_lengths = FALSE) 
  
  ogl <- og$length
  
  # run resampling iterations
  rr <- purrr::map(1:iters, ~ smpl_fsh_comps(data,
                                             yrs, 
                                             bin,  
                                             exp_meth,
                                             boot_trip = boot_trip, 
                                             boot_lengths = boot_lengths))
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # compute effective sample size of bootstrapped age/length
  r_length %>%
    tidytable::map(., ~ess_size(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") -> .ess_size

  # compute harmonic mean of iterated effective sample size, which is the input sample size (iss)
  
  .ess_size %>% 
    tidytable::summarise(iss = psych::harmonic.mean(ess, na.rm=T),
                         .by = c(year, species, comp_type)) %>% 
    tidytable::filter(iss > 0) %>% 
    tidytable::left_join(data %>% 
                           tidytable::drop_na() %>% 
                           summarise(total_ss = sum(frequency), .by = c(year, species))) -> iss_size

  # write input sample size results
    vroom::vroom_write(.ess_size, here::here("output", paste0(save, "_iter_ess_sz.csv")), delim = ",")
    vroom::vroom_write(iss_size, here::here("output", paste0(save, "_iss_sz.csv")), delim = ",")    

}