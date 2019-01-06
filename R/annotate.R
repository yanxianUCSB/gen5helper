# annotating ------------------------------

#' g5h.annotate
#' Add time interval
#'
#' @param .data data.frame cleaned by g5h.clean()
#' @param by 'col' or 'row', default is 'col'. See ?g5h.gather_col for more info.
#'
#' @return data.frame
#' @export
#'
g5h.annotate <- function(.data, by='col'){
    .data %>%
        g5h.set_time2('hours')
}

#' Add time intervals
#'
#' g5h.set_time() preserves existing variables and add new variable,
#' time, which are time intervals in hours.
#'
#' @param .data data.frame cleaned by g5h.clean()
#' @param units hours or minutes
#'
#' @return input data.frame appended with time
#'
g5h.set_time2 <- function(.data, units='hours') {
    #NULLing
    realTime <- time <- NULL
    .data %>%
        arrange(desc(realTime)) %>%
        mutate(time = as.numeric(difftime(realTime,
                                          realTime[length(realTime)],
                                          units = units)))
}

#' Add mean and standard deviation
#'
#' gather_col() preserve existing variables and add mean and standard
#' deviation, grouped by col.
#' gather_row() preserve existing variables and add mean and standard
#' deviation, grouped by row.
#'
#' @param .data data.frame
#'
#' @return data.frame appended with val.m and val.sd
#'
gather_col <- function(.data) {
    #NULLing
    realTime <- readingType <- row <- val <- NULL
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
#' gather_col() preserve existing variables and add mean and standard
#' deviation, grouped by col.
#' gather_row() preserve existing variables and add mean and standard
#' deviation, grouped by row.
#'
#' @param .data data.frame
#'
#' @return data.frame appended with val.m and val.sd
#'
gather_row <- function(.data) {
    #NULLing
    realTime <- readingType <- row <- val <- NULL
    .data %>%
        group_by(realTime, readingType, col) %>%
        mutate(
            val.m = mean(val),
            val.sd = sd(val)
        ) %>%
        ungroup() %>%
        return()
}

# Deprecated methods ------------------------------

#' Add useful variables
#'
#' Add time interval, mean, standard deviation and initilized treatment and dose.
#'
#' @param .data data.frame cleaned by g5h.clean()
#'
#' @return data.frame appended with time intervals in minutes and hours, mean
#' and standard deviation, grouped by col
#'
annotate <- function(.data) {
    .Deprecated('g5h.annotate2')
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
#'
g5h.annotate.deprecated <- function(.data, by='col'){
    .Deprecated('g5h.annotate2')
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
g5h.set_time <- function(.data){
    .Deprecated('g5h.set_time2')
    #NULLing
    time <- realHour <- NULL
    .data %>%
        g5h.set_time2('hours') %>%
        rename(realHour = time) %>%
        mutate(realMinute = realHour * 60)
}

#' Add mean and standard deviation
#'
#' g5h.gather_col() preserve existing variables and add mean and standard
#' deviation, grouped by col.
#' g5h.gather_row() preserve existing variables and add mean and standard
#' deviation, grouped by row.
#'
#' @param .data data.frame
#'
#' @return data.frame appended with val.m and val.sd
g5h.gather_col <- function(.data) {
    .Deprecated('gather_col')
    .data %>% gather_col()
}

#' Add mean and standard deviation
#'
#' g5h.gather_col() preserve existing variables and add mean and standard
#' deviation, grouped by col.
#' g5h.gather_row() preserve existing variables and add mean and standard
#' deviation, grouped by row.
#'
#' @param .data data.frame
#'
#' @return data.frame appended with val.m and val.sd
g5h.gather_row <- function(.data) {
    .Deprecated('gather_row')
    .data %>% gather_row()
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
g5h.map_col <- function(.data, feature, factors){
    .data[[feature]] <- plyr::mapvalues(.data$col, unique(.data$col), factors)
    return(.data)
}
#' Map group into new variable
#'
#' @param .data data.frame with group info
#' @param newvar name of new variable to add.
#' @param facs vector with length equal to levels of group
#'
#' @return data.frame with added new variable
#' @importFrom plyr mapvalues
#' @importFrom naturalsort naturalfactor
#' @importFrom rlang :=
#'
map.group <- function(.data, newvar, facs){
    #NULLing
    group <- NULL
    if(is.numeric(facs)) {
        mutate(.data,
               !! quo_name(enquo(newvar)) :=
                   plyr::mapvalues(group, unique(group), facs) %>%
                   naturalfactor() %>%
                   as.is(facs)
        )
    } else {
        mutate(.data,
               !! quo_name(enquo(newvar)) :=
                   plyr::mapvalues(group, unique(group), facs) %>%
                   naturalfactor()
        )
    }
}
