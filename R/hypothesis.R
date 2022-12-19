#' Hypothesis after bootstrap
#'
#' @param ctsib_fit model fitting
#' @param bootstrap results for running bootstrap function
#'
#' @return a list with p values for variables that we want to test
#' @export
#'
#' @examples
#' ## CTSIB ##
#' bootstrap_fit <- bootstrap(100,data)
#' hypothesis(ctsib_fit,bootstrap_fit)
hypothesis <- function(ctsib_fit,bootstrap){
  ## test Surfacenorm ##
  ## t_value_norm is the t value of ctsib_fit for term "Surfacenorm", then we compare it with
  ## the t value of bootstrap for term "Surfacenorm" and compute the p value.

  t_value_norm = ctsib_fit$test_stat["Surfacenorm"]
  p_norm <- bootstrap %>% filter(term == "Surfacenorm") %>% summarise(p = mean(abs(statistic) > abs(t_value_norm)))


  ## test Visiondome ##
  ## t_value_dome is the t value of ctsib_fit for term "Visiondome", then we compare it with
  ## the t value of bootstrap for term "Visiondome" and compute the p value.
  t_value_dome = ctsib_fit$test_stat["Visiondome"]
  p_dome <- bootstrap %>% filter(term == "Visiondome") %>% summarise(p = mean(abs(statistic) > abs(t_value_dome)))

  ## test Visionopen ##
  ## t_value_norm is the t value of ctsib_fit for term "Visionopen", then we compare it with
  ## the t value of bootstrap for term "Visionopen" and compute the p value.
  t_value_open = ctsib_fit$test_stat["Visionopen"]
  p_open <- bootstrap %>% filter(term == "Visionopen") %>% summarise(p = mean(abs(statistic) > abs(t_value_open)))

  list(p_surface_norm = p_norm, p_vision_dome = p_dome, p_vision_open = p_open)
}
