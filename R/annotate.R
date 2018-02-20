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
    .Deprecated('g5h.annotate')
    g5h.annotate(.data)
}

#' Add time interval, mean and standard deviation
#'
#' @param .data data.frame cleaned by g5h.clean()
#' @param by 'col' or 'row', default is 'col'
#'
#' @return data.frame
#' @export
#'
#' @examples
#' g5h.clean('data.txt') %>%
#'     g5h.annotate(by='col')
g5h.annotate <- function(.data, by='col'){
    if(by=='col'){
        .data %>%
            g5h.set_time() %>%
            g5h.gather_col() %>%
            return()
    }else if (by == 'row'){
        .data %>%
            g5h.set_time() %>%
            g5h.gather_row() %>%
            return()
    }
}

#' Add time intervals
#'
#' g5h.set_time() preserves existing variables and add new, realMinute and
#' realHour, which are time intervals in minutes and hours.
#'
#' @param .data data.frame cleaned by g5h.clean()
#'
#' @return input data.frame appended with realMinute and realHour
#' @export
#'
#' @examples
#' g5h.clean('data.txt') %>%
#'     g5h.set_time()
g5h.set_time <- function(.data){
    .data %>%
        arrange(desc(realTime)) %>%
        mutate(
            realMinute = as.numeric(difftime(realTime,
                                             realTime[length(realTime)],
                                             units = 'mins')),
            realHour   = as.numeric(difftime(realTime,
                                             realTime[length(realTime)],
                                             units = 'hours'))
        )
}

#' Add mean and standard deviation
#'
#' g5h.gather_col() preserve existing variables and add mean and standard
#' deviation, grouped by col.
#'
#' g5h.gather_row() preserve existing variables and add mean and standard
#' deviation, grouped by row.
#'
#' @param .data data.frame
#'
#' @return data.frame appended with val.m and val.sd
#' @export
#'
#' @examples
#' # group by col
#' g5h.clean('data.txt') %>%
#'     g5h.gather_col()
#' # group by row
#' g5h.clean('data.txt') %>%
#'     g5h.gather_row()
g5h.gather_col <- function(.data) {
    .data %>%
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

#' Add mean and standard deviation
#'
#' g5h.gather_col() preserve existing variables and add mean and standard
#' deviation, grouped by col.
#'
#' g5h.gather_row() preserve existing variables and add mean and standard
#' deviation, grouped by row.
#'
#' @param .data data.frame
#'
#' @return data.frame appended with val.m and val.sd
#' @export
#'
#' @examples
#' # group by col
#' g5h.clean('data.txt') %>%
#'     g5h.gather_col()
#' # group by row
#' g5h.clean('data.txt') %>%
#'     g5h.gather_row()
g5h.gather_row <- function(.data) {
    .data %>%
        group_by(realTime, readingType, col) %>%
        mutate(
            val.m = mean(val),
            val.sd = sd(val)
        ) %>%
        ungroup() %>%
        mutate(
            treatment = factor(col),
            dose = factor(col)
        ) %>%
        return()
}
