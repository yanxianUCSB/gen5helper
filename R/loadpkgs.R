#' Attach common packages
#'
#' @return NULL
#' @export
#'
#' @examples
#' loadpkgs()
loadpkgs <- function(){
    # set locale for string
    Sys.setlocale('LC_ALL','C')
    require(pracma)
    require(dplyr)
    require(tidyr)
    require(ggplot2)
    devtools::install_github('yanxianucsb/yxhelper')
    devtools::install_github('yanxianucsb/yxplot')
    require(yxhelper)
    require(yxplot)
}
