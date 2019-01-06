#' this is a file for functions that are universally useful at common data manipulations
# functions ------------------------------

#' seq.LETTERS
#' generate a sequence of LETTERS from FROM to TO
#' @param from LETTER
#' @param to LETTER
#' @export
seq.LETTERS <- function(from, to){
    LETTERS[which(LETTERS == from):which(LETTERS == to)]
}
#' factor2num
#'Convert factor to numeric
#' @param x factor
#' @export
factor2num <- function(x){as.numeric(as.character(x))}
#' mapvalues_
#'
#' @param x factor or character
#' @param facs character. It maps unique(x) to facs
#' @param bNaturalSort binary
#' @return factor
#' @importFrom plyr mapvalues
#' @importFrom naturalsort naturalfactor
#' @export
#'
mapvalues_ <- function(x, facs, bNaturalSort = F) {
    #NULLing
    if (bNaturalSort) {
        naturalsort::naturalfactor(plyr::mapvalues(x, unique(x), facs))
    }else{
        factor(plyr::mapvalues(x, unique(x), facs), levels = unique(facs))
    }
}
#' as.is
#'
#' @param x object to transform
#' @param vec object to extract class
#'
#' @export
as.is <- function(x, vec) {
    if(class(vec) == 'numeric') {
        return(as.numeric(as.character(x)))
    } else if (class(vec) == 'character') {
        return(as.character(x))
    } else {
        return(x)
    }
}
# annotation
#' Smooth a vector using moving average
#'
#' @param vec numeric vector
#' @param naverage width of moving average
#'
#' @return smoothed
#' @export
#'
#' @examples
#' smooth.mean(1:10, 2)
#' smooth.mean(1:10, 3)
#' smooth.mean(1:10, 5)
smooth.mean <- function(vec, naverage){
    stopifnot(naverage <= as.integer(naverage), 0 < naverage)
    nvec <- c()
    for(i in 1:length(vec)) {
        if(i %% naverage == 0) {
            nvec[(i-naverage+1):i] <- mean(vec[(i-naverage+1):i])
        }
    }
    i <- length(vec)
    ni <- length(nvec)
    if(ni < i){
        nvec[(ni+1):i] <- mean(vec[(ni+1):i])
    }
    return(nvec)
}
#' Range of a vector
#'
#' This computes the range of a vector as a value; NA removed.
#' @param x numeric
#'
#' @return numeric value
#' @export
range_ <- function(x){
    max(x, na.rm = T)-min(x, na.rm = T)
}
#' Normalize a vector by min and max
#'
#' NA removed
#' @param x numeric
#'
#' @return a normalized vector
#' @export
normalize <- function(x){
    (x-min(x, na.rm = T))/range_(x)
}
#' Ungroup() and as.data.frame()
#'
#' @param .data grouped data.frame
#'
#' @return data.frame()
#' @importFrom dplyr ungroup
#' @export
ungroup_ <- function(.data){
    .data %>% ungroup() %>% as.data.frame()
}
#' write.csv and return .data
#'
#' @param x object
#' @param file filename for write.csv
#'
#' @return x
#' @importFrom utils write.csv
#' @export
write.csv_ <- function(x, file){
    write.csv(x, file, row.names = F)
    return(x)
}
#' saveRDS and return .data
#'
#' @param .data object to be saved
#' @param file filename to save
#' @param ... for saveRDS
#'
#' @return .data
#' @export
saveRDS_ <- function(.data, file = file, ...){
    saveRDS(object = .data, file = file, ...)
    return(.data)
}
#' Most frequent numbers
#'get the n most frequent elements in an array
#' @param x an array of elements
#' @param n integer, default is 1
#'
#' @return the most n elements
#' @export
#'
#' @examples
#' most.freq(c('a', 'a', 'b', 'b', 'b', 'c'), n = 2)
#' most.freq(c(1, 1, 2, 3, 3, 3, 4, 4), n = 2)
most.freq <- function(x, n = 1){
    if(length(x) == 1) return(x)
    type <- typeof(x)
    sort.freq <- sort(table(x),decreasing=TRUE)
    this.freq <- sort.freq[n]
    this.index <- which(sort.freq < this.freq) - 1
    if(length(this.index) == 0)
        this.index <- length(sort.freq)
    results <- names(sort.freq[1:this.index])
    class(results) <- typeof(x)
    return(results)
}
