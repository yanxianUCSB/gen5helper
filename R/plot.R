#' Plot multiple readingType in line+ribbon,
#' the linewidth of the first reading Type will be doubled
#'
#' @param ds a gen5 annotated data.frame
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
plotFlsAbs <- function(ds, Ctrl = list(flsReadingType = unique(ds$readingType)[1],
                                       plotN = c('all', 'major')[2])) {
    ds1 <- ds %>%
        filter(readingType == Ctrl$flsReadingType)
    g <- ggplot() +
        geom_line(
            data    = ds1,
            mapping = aes(
                x = realHour,
                y = val.m,
                col = readingType,
                group = readingType
            ),
            lwd = 1
        ) +
        geom_ribbon(
            data    = ds1,
            mapping = aes(
                x = realHour,
                ymax = val.m + val.sd,
                ymin = val.m - val.sd,
                fill = readingType
            ),
            alpha   = 0.2
        )
    Ntype <- length(unique(ds$readingType))
    if (Ctrl$plotN == 'all' && Ntype > 1) {
        dsN <- ds %>%
            filter(readingType != Ctrl$flsReadingType)
        SCALE <- max(ds1$val.m) / max(dsN$val.m)
        g <- g +
            geom_line(
                data = dsN,
                mapping = aes(
                    x = realHour,
                    y = val.m * SCALE,
                    col = readingType,
                    group = readingType
                ),
                lwd = 0.5
            ) +
            geom_ribbon(
                data = dsN,
                mapping = aes(
                    x = realHour,
                    ymax = (val.m + val.sd) * SCALE,
                    ymin = (val.m - val.sd) * SCALE,
                    fill = readingType
                ),
                alpha = 0.2
            )
        g <- g +
            scale_y_continuous(sec.axis = sec_axis(~./SCALE, name='Turbidity'),
                               limits = c(0, max(ds$val.m))) +
            scale_x_continuous()
    }
    g <- g +
        facet_grid(treatment~dose)
    g <- g +
        labs(x = 'Time [hr]',
             y = 'ThT Fluorescence',
             col = '',
             fill = '') +
        theme.background.1() +
        theme.title.text.1()
    return(g)
}
