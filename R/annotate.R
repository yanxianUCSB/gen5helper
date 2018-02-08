#' annotate cleaned dataset
#'
#' @param .data cleaned dataset to annotate
#'
#' @return annotated dataset
#' @export none
#'
#' @examples
#' export2dataframe('data.txt') %>% annotate()
annotate <- function(.data) {
    .data %>%
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
        ) %>%
        return()
}
