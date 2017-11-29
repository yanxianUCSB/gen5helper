#' Add Boltzmann fitting data to annotated data
#'
#' @param inputRDS annotated gen5 data frame
#'
#' @return fitted gen5 data frame
#' @export
#'
#' @examples
annotated2fitted <- function(inputRDS = 'annotated') {
    ds <- readRDS(inputRDS) %>%
        filter(readingType == unique(readingType)[1]) %>%
        filter(row %in% unique(row)) %>%
        filter(realHour < 60, realHour > 0)
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
            mod <- my_fit_model(ds2$realHour, ds2$val)
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
    # filter out deviated data points
    df <- df %>%
        filter(
            y0.sd < 0.5*y0.m,
            t2.sd < 0.5*t2.m,
            A.sd < 0.5*A.m,
            k.sd < 0.5*k.m
        )
    return(df)
}
