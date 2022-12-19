#' Bootstrap for binomial (For Surface (norm & vision))
#'
#' @param B bootstrap replicates
#' @param data data used for bootstrap
#' @param para the parameters for null hypothesis
#'
#' @return A list with summary statistics from the bootstrap
#' @export
#'
#' @examples
#' ## CTSIB ##
#' bootstrap_fit <- bootstrap(100,data)
bootstrap <- function(B, data, para){

## For the binomial model, i is the number of  subjects and j is the number of measurements per subject
## B means the bootstrap replicates and N is the number of sets includes iid. residuals(different)
  n = nrow(data)
  i = 40
  j = 12
  N = i*B
## Model fitting ##
  ctsib_fit = run_model(data, "ctsib")

## Resample residuals from normal distribution with the fitting model's sigmasq ##
  Zi = rnorm(N, mean =0, sd = sqrt(ctsib_fit$sigmasq))
  Zi_1 = rep(Zi,each = 12)

## Compute linear predictor   ##
  line_pred <- ctsib_fit$beta[1] + para[1]*ctsib_fit$beta[2]*data$surface_norm +
    para[2]*ctsib_fit$beta[3]*data$vision_dome + para[3]*ctsib_fit$beta[4]*data$vision_open +Zi_1

## Compute mu_hat_i for linear predictor ##
  mu_hat_i = exp(line_pred)/(1+exp(line_pred))

## Resample B sets of Y_hat which is Bernoulli distribution and combine it with original data ##
  data_test_new <- data %>%
    slice(rep(1:480,B)) %>%
    mutate(B = rep(1:B,each = 480)) %>%
    mutate(mu_hat_i = mu_hat_i) %>%
    mutate(Y_hat = rbernoulli(n(), mu_hat_i))

  ## Remove problematic replicates
  data_test_new <- data_test_new %>%
    filter(!(B %in% c(5,8,9,10,12,13,16,20)))

## Bootstrap from new data after resampling
    bootstrap_test <- data_test_new %>%
                        group_by(B)%>%
                        summarize(tidy(glmer(Y_hat ~ Surface + Vision + (1|Subject),
                                             family = binomial)), .groups = "drop")

   return(bootstrap_test)
}


