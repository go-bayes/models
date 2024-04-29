SL.template <- function(Y, X, newX, family, obsWeights, id, ...) {
  # load required packages
  # require('pkg')
  if(family$family == 'gaussian') {
  
  }
  if(family$family == 'binomial') {
  
  }
  # pred is the predicted responses for newX (on the scale of the outcome)
  pred <- numeric()
  # fit returns all objects needed for predict.SL.template
  fit <- list(object = )
  # declare class of fit for predict.SL.template
  class(fit) <- 'SL.template'
  # return a list with pred and fit
  out <- list(pred = pred, fit = fit)
  return(out)
}