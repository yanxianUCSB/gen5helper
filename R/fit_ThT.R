#' UI fit ThT
#'User interface for fitting ThT kinetic curve
#' @return
#' @export fitted.RDS fitted dataset
#'
#' @examples
ui.fit.ThT <- function(){
    ds <- readRDS(file = select.list(title = 'Which annotated data?',
                                     choices = list.files())) %>%
        filter(readingType == select.list(title = 'Which reading do you want to fit?',
                                          choices = unique(readingType)))
    df <- fit.boltzmann(ds, start = list(A = 3000, y0 = 1000, k = 1, t2 = 5))
    # df <- filter.fit(df, TOL = 3)
    saveRDS(df, 'fitted.RDS')
    cat('fitting using Boltzmann model >>\n')
    cat('saved as fitted.RDS \n')
    ifelse0(select.list(title = 'do you want to plot it now?',
                        choices = c('Yes','No')) == 'Yes',
            ui.plot.fit(),
            print('Thanks for using ThTFit!\n'))
}
#' UI plot fit
#'User interface for ploting fitted ThT kinetic
#' @return
#' @export
#'
#' @examples
ui.plot.fit <- function(){
    warning('ui.plot.fit NotImplemented yet')
}
#' filter fit
#'
#' @param df
#' @param TOL
#'
#' @return
#' @export
#'
#' @examples
filter.fit <- function(df, TOL = 1) {
    df %>%
        filter(y0.sd < TOL * y0.m,
               t2.sd < TOL * t2.m,
               A.sd < TOL * A.m,
               k.sd < TOL * k.m)
}
#' Title
#'
#' @param ds  gen5helper annotated data.frame
#' @param start  a list of initial fitting para
#'
#' @return  ds with extra fitting features
#' @export
#'
#' @examples
fit.boltzmann <- function(ds, start = list(A = 3000, y0 = 1000, k = 10, t2 = 1)) {
    df <- bind_rows(lapply(unique(ds$row), function(ROW){
        ds1 <- ds %>%
            filter(row == ROW)
        bind_rows(lapply(unique(ds1$well), function(WELL){
            ds2 <- ds1 %>%
                filter(well == WELL) %>%
                arrange(rev(desc(realHour))) %>%
                filter(val.m-3*val.sd < val) %>%  # 3 standard deviation filter
                filter(val < val.m+3*val.sd) %>%
                mutate(val = pracma::hampel(val, k = 2, t0 = 3)$y)  # Hampel filter, 3 sigma
            mod <- Boltzmann(ds2$realHour, ds2$val, start = start)
            ds3 <- ds2 %>% mutate(
                y0 = coef(mod)['y0'],
                A = coef(mod)['A'],
                k = coef(mod)['k'],
                t2 = coef(mod)['t2'],
                val.predict = predict(mod, list(x = realHour)))
        }))
    }))
    df <- df %>%
        group_by(row) %>%
        mutate(
            y0.m = mean(y0),
            y0.sd = sd(y0),
            A.m = mean(A),
            A.sd = sd(A),
            k.m = mean(k),
            k.sd = sd(k),
            t2.m = mean(t2),
            t2.sd = sd(t2),
            val.pred.m = mean(val.predict),
            val.pred.sd = sd(val.predict)
        ) %>%
        ungroup()
}

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
