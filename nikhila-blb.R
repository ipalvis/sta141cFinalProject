#' @import tidyverse
#' @import purrr
#' @import stats
#' @importFrom magrittr %>%
#' @details
# Logistic Regression with Little Bag of Bootstraps
"_PACKAGE"

#' @export
blbglm <- function(formula, data, m = 2, B = 10, parallel = FALSE) {
  data_list <- split_data(data, m)
  estimates <- map(
    data_list,
    ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
  ) %>% reduce(`+`)/m
  
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blbglm"
  invisible(res)
}


#' split data into m parts of approximated equal sizes
split_data <- function(data, m) {
  indx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(indx)
}


#' compute the estimates
glm_each_subsample <- function(formula, data, n, B) {
  each_sub <- replicate(B, glm_each_boot(formula, data, n), simplify = FALSE)
  len <- length(each_sub)
  coef_sub <- sapply(each_sub, function(x) x$coef)
  coef_sub_av <- rowSums(coef_sub)/len
  
}


#' compute the regression estimates for a blb dataset
glm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  boot = glm1(formula, data, freqs)
}


#' estimate the regression estimates based on given number of repetitions
glm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick wrong variables from a parent scope.
  environment(formula) <- environment()
  fit <- glm(formula, data, weights = freqs, family = binomial("logit"), control = list(maxit = 50))
  list(coef = blbcoef(fit), se = blbse(fit))
}


#' compute the coefficients from fit
blbcoef <- function(fit) {
  coef <- fit$coefficients
}

#' compute SE from fit
blbse <- function(fit) {
  se <- summary(fit)$coefficients[, 2]
  
}


#' compute sigma from fit
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "ynum")
  e <- fitted(fit) - y
  w <- fit$weights
  # YOUR CODE to compute sigma
  
  sqrt(sum(e^2*w)/(p-1))
}


#' @export
#' @method print blblm
print.blbglm <- function(x, ...) {
  cat("blblm model")
  
}


#' @export
#' @method sigma blblm
sigma.blbglm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  coef_names <- names(est[[1]][[1]]$coef)
  #print(coef_names)
  
  
}

#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  print(est['coef'])
  
}


#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(fit$formula), "term.labels")
  }
  # YOUR CODE to compute the confidence intervals
}

#' @export
#' @method predict blblm
predict.blblm <- function(object, newdata, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), newdata)
  if (confidence) {
    # YOUR CODE to compute the predictions and their confidence intervals
  } else {
    # YOUR CODE to compute the predictions
  }
}
