#' find sigmoid curve peak
#'
#' @param vec
#' @param x
#'
#' @return
#' @export
#'
#' @examples
fp <- function(vec, x) {
    l <- mean(vec[(x-6):(x-1)])
    r <- mean(vec[(x+1):(x+6)])
    a <- vec[x]
    if(l <= a && a >= r) {
        return(x)
    }
    if(l <= a && a < r) {
        return(fp(vec, x+1))
    } else if (l > a && a >= r) {
        return(fp(vec, x-1))
    } else {
        warning('fp was trapped in valley')
        return(NA)
    }
}
