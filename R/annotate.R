#' annotate
#' annotate cleaned data.frame
#' @param ds
#'
#' @return
#' @export
#'
#' @examples
#' ds.cleaned   <- export2dataframe('data.txt')
#' ds.annotated <- annotate(ds.cleaned)
annotate <- function(ds) {
    ds <- ds %>%
        arrange(desc(realTime)) %>%
        mutate(
            realMinute = as.numeric(difftime(realTime, realTime[length(realTime)], units = 'mins')),
            realHour   = as.numeric(difftime(realTime, realTime[length(realTime)], units = 'hours'))
        ) %>%
        group_by(realTime, readingType, row) %>%
        mutate(
            val.m = mean(val),
            val.sd = sd(val)
        ) %>%
        ungroup() %>%
        mutate(
            treatment = factor(row),
            dose = factor(row)
        )
}
