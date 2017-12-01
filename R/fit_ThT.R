#' Boltzmann model for fitting time series data
#'
#' @param time_
#' @param val_
#' @param start  start = list(y0 = 500, A = 500, k = 1.1, t2 = 10)
#'
#' @return
#' @export
#'
#' @examples
Boltzmann <- function(time_, val_, start = list(y0 = 500, A = 500, k = 1.1, t2 = 10)) {
    minpack.lm::nlsLM(y ~ y0 + A/(1+exp(-k*(t-t2))), data.frame(t = time_, y = val_),
                      start = start)
}
#' Fitting ThT time series data with Boltzmann model. return fit data and model
#'
#' @param time_
#' @param val_
#'
#' @return list(data.frame, model)
#' @export
#'
#' @examples
ThT_fit <- function(time_, val_){
    mod <- Boltzmann(time_, val_)
    return(
        list(
            df = data.frame(
                realHour = time_,
                val.m = predict(mod, list(x = time_))
            ),
            model = mod
        )
    )
}
#' ThT_fit function to get only fit data.
#'
#' @param time_
#' @param val_
#'
#' @return numeric vector of fit data
#' @export
#'
#' @examples
ThT_fit_ThT <- function(time_, val_){
    return(ThT_fit(time_, val_)$df$val.m)
}
