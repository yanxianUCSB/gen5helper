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

#' plot fitted dataset
#'
#' @param df version XXX
#' @param tiff logical, whether to output tiff imgs
#' @param i suflix of images output filename
#'
#' @return df
#' @export
#'
#' @examples
#' annotated.dataset %>% fit.Boltzmann() %>% plot.fitted.dataset()
#'
plot.fitted.dataset <- function(df, tiff = F, i=NULL) {
    g.raw <- ggplot(df, aes(group = well)) +
        geom_line(aes(x = realHour, y=val.predict, col = 'fit'), alpha = 0.5, lwd = 1) +
        geom_point(aes(x = realHour, y=val, col = 'exp'), alpha = 0.5, size = 0.5) +
        facet_wrap(~dose) +
        theme.background.1() +
        theme.title.text.1() +
        labs(
            x = 'Time [hr]',
            y = 'ThT fluorescence',
            col = ''
        )
    df <- df %>% group_by(well) %>% filter(realHour == realHour[1])
    g.A <- ggplot(df, aes(x = dose, y = A.m)) +
        geom_point(size = 2) +
        geom_errorbar(aes(x = dose, ymax = A.m + A.sd, ymin = A.m - A.sd)) +
        theme.background.1() +
        theme.title.text.1() +
        labs(x = 'NaCl [mM]',
             y = 'Maximum ThT')
    g.t2 <- ggplot(df, aes(x = dose, y = t2.m)) +
        # g.t2 <- ggplot(df , aes(x = dose, y = t2.m)) +
        geom_point(size = 2) +
        geom_errorbar(aes(x = dose, ymax = t2.m + t2.sd, ymin = t2.m - t2.sd)) +
        theme.background.1() +
        theme.title.text.1() +
        labs(x = 'NaCl [mM]',
             y = 'Lag Time (t2)')
    g.k <- ggplot(df, aes(x = dose, y = k.m)) +
        geom_point(size = 2) +
        geom_errorbar(aes(x = dose, ymax = k.m + k.sd, ymin = k.m - k.sd)) +
        theme.background.1() +
        theme.title.text.1() +
        labs(x = 'NaCl [mM]',
             y = 'Rate Constant (k)')
    if (tiff){
        ggsave(paste0('plot.raw',i,'.tiff'), g.raw, width = 8, height = 8)
        ggsave(paste0('plot.k'  ,i,'.tiff'), g.k,   width = 6, height = 3)
        ggsave(paste0('plot.A'  ,i,'.tiff'), g.A,   width = 6, height = 3)
        ggsave(paste0('plot.t2' ,i,'.tiff'), g.t2,  width = 6, height = 3)
    }
    ggsave(paste0('plot.raw',i,'.png'), g.raw, width = 8, height = 8)
    ggsave(paste0('plot.k'  ,i,'.png'), g.k,   width = 6, height = 3)
    ggsave(paste0('plot.A'  ,i,'.png'), g.A,   width = 6, height = 3)
    ggsave(paste0('plot.t2' ,i,'.png'), g.t2,  width = 6, height = 3)
    return(df)
}

#' plot annotated dataset
#'
#' @param .data version XXX
#' @param primary primary readingType default 'tht'
#' @param secondary secondary readingType default 'abs'
#' @param i sufix of images output filename
#'
#' @return .data
#' @export
#'
#' @examples
#' export2data.frame('data.txt') %>% annotate() %>% plot.annotated.dataset()
plot.annotated.dataset <- function(.data, primary='tht', secondary='abs', i=NULL, facet.wrap = FALSE) {
    g.raw <- plotFlsAbs(.data, list(flsReadingType = primary, plotN = 'all')) +
        facet_wrap(~dose) +
        theme(axis.text.y = element_text(angle = 45)) +
        theme.background.1() +
        theme.title.text.1()
    g.tht <- plotFlsAbs(.data, list(flsReadingType = primary, plotN = 'major')) +
        facet_wrap(~dose) +
        theme(axis.text.y = element_text(angle = 45)) +
        theme.background.1() +
        theme.title.text.1()
    g.turb <- ggplot(.data %>%
                         filter(readingType == secondary) %>%
                         group_by(well) %>%
                         filter(realHour == realHour[1]) %>%
                         mutate(turb = max(val)) %>%
                         ungroup() %>%
                         group_by(treatment, dose) %>%
                         mutate(turb.m = mean(turb),
                                turb.sd = sd(turb)) %>%
                         ungroup()
                         ) +
        geom_point( aes(x = dose, y = val.m), size = 2) +
        geom_errorbar(aes(x = dose, ymax=val.m+val.sd, ymin=val.m-val.sd)) +
        labs(x = 'NaCl [mM]',
             y = 'Turbidity') +
        theme.background.1() +
        theme.title.text.1()
    ggsave(filename = paste0('plot.tht.', i, '.png'), g.tht, width = 8, height = 8)
    ggsave(filename = paste0('plot.raw.', i, '.png'), g.raw, width = 8, height = 8)
    ggsave(filename = paste0('plot.turb.', i, '.png'), g.turb, width = 6, height = 3)
    return(.data)
}
