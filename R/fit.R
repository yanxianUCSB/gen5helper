#' Get half time by linear fitting
#'
#' get half time according to http://www.amylofit.ch.cam.ac.uk
#' The algorithm for the extraction of the half times proceeds as follows: first the middle
#' part of the curve is selected, by determining when the average over several points is first
#' above 0.3 and when the average is last below 0.7. The number of points to be averaged
#' over depends on the number of points in the curve. A straight line is then fitted to this
#' middle part of the curve, the point at which it crosses the value of 0.5 is recorded as the
#' half time. (source: DOI: nprot.2016.010)
#' @param time vector of time
#' @param val vector of values
#'
#' @return half time
#' @export
get.halftime <- function(time, val){
    if(!(length(time)==length(val) & length(time)>1)) {
        stop('Length of time and val should be equal and >= 2')
    }
    val <- normalize(val)
    val.smoothed <- smooth.mean(val, ceiling(length(val)/100))
    lm.D9 <- lm(val ~ time, subset=(which(0.3<=val.smoothed & val.smoothed<=0.7)))
    return( (0.5 - lm.D9$coefficients[1]) / lm.D9$coefficients[2])
}  # time and normalized var 0.3-0.7

#' UI fit ThT
#'
#' User interface for fitting ThT kinetic curve
#'
#' @param START initial guess for fit.Boltzmann
#'
#' @return NULL
#'
ui.fit.ThT <- function(START = list(A = 3000, y0 = 1000, k = 1, t2 = 5)){
    # ds <- readRDS(file = select.list(title = 'Which annotated data?',
    #                                  choices = list.files())) %>%
    #     filter(readingType == select.list(title = 'Which reading do you want to fit?',
    #                                       choices = unique(readingType)))
    # df <- fit.boltzmann(ds, A0 = START$A, k0 = START$k, t20 = START$t2)
    # # df <- filter.fit(df, TOL = 3)
    # saveRDS(df, 'fitted.RDS')
    # cat('fitting using Boltzmann model >>\n')
    # cat('saved as fitted.RDS \n')
    # ifelse0(select.list(title = 'do you want to plot it now?',
    #                     choices = c('Yes','No')) == 'Yes',
    #         ui.plot.fit(),
    #         print('Thanks for using ThTFit!\n'))
}

#' UI plot fit
#'
#' User interface for ploting fitted ThT kinetic
#'
#' @return NULL
ui.plot.fit <- function(){
    # warning('ui.plot.fit NotImplemented yet')
}

#' Fit readings with Boltzmann model
#'
#' fit.boltzmann() using Boltzmann model to fit readings and time intervals with
#' unit of hours, using start as initial guesses. It appends A, y0, k, t2 and val.predict, while preserving existing
#' variables.
#'
#' @param .data data.frame with x as time, y as value
#' @param A0 initial guess of amplititue, default 1
#' @param k0 initial guess, default 1
#' @param t20 initial guess, default 1
#'
#' @return data.frame with fitted parameter and predicted value
#' @importFrom dplyr mutate %>%
#' @importFrom stats coef predict
#' @export
#'
fit.boltzmann <- function(.data, A0 = 1, k0 = 1, t20 = 1) {
        tryCatch({
            mod <- Boltzmann(.data$x, .data$y, A0=A0, k0=k0, t20=t20)
            ds3 <- .data %>% mutate(
                A = coef(mod)['A'],
                k = coef(mod)['k'],
                t2 = coef(mod)['t2'],
                r2 = summary(mod)$r.squared,
                predict = predict(mod))
        }, warning=function(w){
        }, error=function(e){
            ds3 = NULL
        }, finally = {})
    return(ds3)
}

#' Boltzmann model for fitting time series data
#'
#' @param time_ time series
#' @param val_ normalized value
#' @param A0 amplititude
#' @param k0 rate constant
#' @param t20 halt time
#'
#' @return a model
#' @export
Boltzmann <- function(time_, val_, A0 = 1, k0 = 1, t20 = 1) {
    minpack.lm::nlsLM(y ~ A/(1+exp(-k*(t-t2))),
                      data.frame(t = time_, y = val_),
                      start = list( A = A0, k = k0, t2 = t20))
}
#' Boltzmann model for fitting time series data
#'
#' @param time_ NotExported
#' @param val_ NotExported
#' @param start NotExported
#'
#' @return NotExported
Boltzmann_double <- function(time_, val_, start = list(y0 = 500,
                                                       A = 500, k = 1.1, t2 = 10,
                                                       A2 = 500, k2 = 1.1, t22 = 10)) {
    minpack.lm::nlsLM(y ~ y0 +
                          A/(1+exp(-k*(t-t2))) +
                          A2/(1+exp(-k2*(t-t22))),
                      data.frame(t = time_, y = val_),
                      start = start)
}
