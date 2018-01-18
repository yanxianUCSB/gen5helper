#' loadpkgs for gen5 data analysis
#'
#' @return
#' @export
#'
#' @examples
loadpkgs <- function(){
    # set locale for string
    Sys.setlocale('LC_ALL','C')
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    devtools::install_github('yanxianucsb/yxhelper')
    devtools::install_github('yanxianucsb/yxplot')
    library(yxhelper)
    library(yxplot)
}
