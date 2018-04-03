#' Add useful variables
#'
#' Add time interval, mean, standard deviation and initilized treatment and dose.
#'
#' @param .data data.frame cleaned by g5h.clean()
#'
#' @return data.frame appended with time intervals in minutes and hours, mean
#' and standard deviation, grouped by col
#' @export
#'
#' @examples
#' g5h.clean('data/demo.txt') %>%
#'     annotate()
annotate <- function(.data) {
    .Deprecated('g5h.annotate')
    g5h.annotate(.data, by='col')
}

#' Add useful variables
#'
#' Add time interval, mean, standard deviation and initilized treatment and dose.
#'
#' @param .data data.frame cleaned by g5h.clean()
#' @param by 'col' or 'row', default is 'col'. See ?g5h.gather_col for more info.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' ds <- g5h.clean('data/demo.txt')
#' ds %>% g5h.annotate()
#' ds %>% g5h.annotate(by='row')
#'
g5h.annotate <- function(.data, by='col'){
    .data <- .data %>% g5h.set_time()
    if (by == 'col') {
        .data <- .data %>% g5h.gather_col()
    } else if (by == 'row') {
        .data <- .data %>% g5h.gather_row()
    } else {
        stop('argument by should be either col or row')
    }
    .data %>%
        g5h.map_row('treatment', unique(.data$row)) %>%
        g5h.map_row('dose', unique(.data$row)) %>%
        return()
}

#' Add time intervals
#'
#' g5h.set_time() preserves existing variables and add new, realMinute and
#' realHour, which are time intervals in minutes and hours.
#'
#' @param .data data.frame cleaned by g5h.clean()
#'
#' @return input data.frame appended with realMinute and realHour
#'
#' @examples
#' g5h.clean('data/demo.txt') %>%
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
#'
#' @examples
#' # group by col
#' g5h.clean('data/demo.txt') %>%
#'     g5h.gather_col()
#' # group by row
#' g5h.clean('data/demo.txt') %>%
#'     g5h.gather_row()
g5h.gather_col <- function(.data) {
    .data %>%
        group_by(realTime, readingType, row) %>%
        mutate(
            val.m = mean(val),
            val.sd = sd(val)
        ) %>%
        ungroup() %>%
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
#'
#' @examples
#' # group by col
#' g5h.clean('data/demo.txt') %>%
#'     g5h.gather_col()
#' # group by row
#' g5h.clean('data/demo.txt') %>%
#'     g5h.gather_row()
g5h.gather_row <- function(.data) {
    .data %>%
        group_by(realTime, readingType, col) %>%
        mutate(
            val.m = mean(val),
            val.sd = sd(val)
        ) %>%
        ungroup() %>%
        return()
}

#' Map row or col to new values
#'
#' g5h.map_row() or g5h.map_col() add new variables by mapping row or col to
#' new values.
#'
#' @param .data data.frame cleaned by g5h.clean()
#' @param feature character, name of new variable to add.
#' @param factors vector with length equal to levels of row or col.
#'
#' @return data.frame appended with new variables
#'
#' @examples
#' g5h.clean('data/demo.txt') %>%
#'     g5h.map_row(feature = 'dose', factors = c(1, 2, 3)) %>%
#'     g5h.map_col(feature = 'treatment', factors = c('freeze', 'bake', 'grill'))
g5h.map_row <- function(.data, feature, factors){
    .data[[feature]] <- plyr::mapvalues(.data$row, unique(.data$row), factors)
    return(.data)
}

#' Map row or col to new values
#'
#' g5h.map_row() or g5h.map_col() add new variables by mapping row or col to
#' new values.
#'
#' @param .data data.frame cleaned by g5h.clean()
#' @param feature character, name of new variable to add.
#' @param factors vector with length equal to levels of row or col.
#'
#' @return data.frame appended with new variables
#'
#' @examples
#' g5h.clean('data/demo.txt') %>%
#'     g5h.map_row(feature = 'dose', factors = c(1, 2, 3)) %>%
#'     g5h.map_col(feature = 'treatment', factors = c('freeze', 'bake', 'grill'))
g5h.map_col <- function(.data, feature, factors){
    .data[[feature]] <- plyr::mapvalues(.data$col, unique(.data$col), factors)
    return(.data)
}
