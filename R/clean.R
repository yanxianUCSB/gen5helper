#' UI for g5h.clean
#'
#' ui.clean() is a command line user interface for g5h.clean. See ?g5h.clean for
#' more information.
#'
#' @param args command arguments
#'
#' @return data.frame
#'
#' @examples
#' ui.clean()
ui.clean <- function(args = commandArgs(trailingOnly = T)){
    if (length(args) < 1) {
        inputs <- select.list(
            title = 'Select Gen5 automatic exported .TXT data',
            choices = list.files(path = getwd(), pattern = '.txt$',
                                 all.files = F, full.names = F,
                                 include.dirs = F, ignore.case = T),
            multiple = T)
    } else if (args[1] == '-a') {
        inputs <- list.files(path = getwd(), pattern = '.txt$',
                             all.files = F, full.names = F,
                             include.dirs = F, ignore.case = T)
    } else {
        inputs <- args
    }
    inputs %>%
        bind_rows(function(input){
            g5h.clean(input) %>%
                mutate(src = input)
        }) %>%
        return()
}

#' Clean Gen5 exported data
#'
#' export2dataframe() returns technically correct data.frame from Gen5 2.06
#' exported tab-delim data. The exported data can be generated using default
#' export protocol in Gen5 2.06. See Gen5 User Guide for more information.
#'
#' @param filename the name of the file which the data are to be read from. If it
#' does not contain an absolute path, the file name is relative to the current
#' working directory, getwd().
#' @param Ctrl list of controls. NOT IMPLEMENTED
#'
#' @return technically correct data.frame.
#' @export
#'
#' @examples
#' export2dataframe(filename = 'data.txt')
export2dataframe <- function(filename, Ctrl = list(sample.by = 'row')) {
    .Deprecated('g5h.clean')
    g5h.clean(filename)
}

#' Clean Gen5 exported data
#'
#' g5h.clean() returns technically correct data.frame from Gen5 2.06 exported
#' tab-delim data. The exported data can be generated using default export
#' protocol in Gen5 2.06. See Gen5 User Guide for more information.
#'
#' @param file the name of the file which the data are to be read from. If it
#' does not contain an absolute path, the file name is relative to the current
#' working directory, getwd().
#'
#' @return technically correct data.frame.
#' @export
#'
#' @examples
#' g5h.clean(file = 'data.txt')
g5h.clean <- function(file) {
    .Deprecated('g5h.clean2')
    g5h.clean_(file)
}

#' Clean Gen5 exported data
#'
#' g5h.clean2() returns technically correct data.frame from Gen5 2.06 exported
#' tab-delim data. The exported data can be generated using default export
#' protocol in Gen5 2.06. See Gen5 User Guide for more information.
#'
#' @param files a vector of names of the file which the data are to be read from.
#' If it does not contain an absolute path, the file name is relative to the
#' current working directory, getwd().
#'
#' @return technically correct data.frame.
#' @export
#'
#' @examples
#' g5h.clean2(file = 'data.txt')
g5h.clean2 <- function(files) {
    bind_rows(lapply(files, function(file) g5h.clean_(file)))
}

#' Clean Gen5 exported data
#'
#' g5h.clean() returns technically correct data.frame from Gen5 2.06 exported
#' tab-delim data. The exported data can be generated using default export
#' protocol in Gen5 2.06. See Gen5 User Guide for more information.
#'
#' @param file the name of the file which the data are to be read from. If it
#' does not contain an absolute path, the file name is relative to the current
#' working directory, getwd().
#'
#' @return technically correct data.frame.
g5h.clean_ <- function(file) {
    read2ds <- function(file, start.row, end.row) {
        ds <- read.csv(
            file,
            header = T,
            nrows = end.row - start.row,
            skip = start.row - 1,
            sep = '\t',
            stringsAsFactors = F
        )
        ds2 <- gather(ds, 'well', 'val', 3:ncol(ds)) %>%
            filter(!is.na(val)) %>%
            separate(well, c('row', 'col'), sep = 1, remove = F)
        names(ds2)[2] <- 'temp'
        # filter out masked wells
        maskedRowCol <- ds2 %>%
            filter(grepl('\\*', val)) %>%
            select(row, col)
        ds3 <- ds2 %>%
            filter(!((row %in% unique(maskedRowCol$row)) &
                         (col %in% unique(maskedRowCol$col)))) %>%
            mutate(val = as.numeric(val)) %>%
            mutate(time.min = as.numeric(difftime(
                strptime(as.character(Time), format = '%H:%M:%S'),
                strptime(as.character(Time[1]), format = '%H:%M:%S'),
                units = 'min'
            )))
    }
    ds.raw <- readLines(file)
    line.date.times <- which(grepl('Date\t', ds.raw))
    line.procedures <- which(grepl('Procedure Details', ds.raw))
    line.reads <- line.procedures - 1 +
        grep('Start Kinetic',
             ds.raw[line.procedures[1]:(line.procedures[1] + 20)])
    if (length(line.reads) == 0) {
        warning('Input Format Not Defined')
        return(NULL)
    }
    out <- bind_rows(lapply(1:length(line.reads), function(read.i){
        line.read <- line.reads[read.i]
        line.date.time <- line.date.times[read.i]
        time.start.str <- strptime(
            paste(strsplit(ds.raw[line.date.time],'\t')[[1]][2],
                  strsplit(ds.raw[line.date.time+1], '\t')[[1]][2]),
            format = '%m/%d/%Y %I:%M:%S %p',
            tz = '')
        l <- strsplit(ds.raw[line.reads[read.i]], ' ')[[1]]
        cur.total.reads <- as.integer(l[which(l == 'Reads') - 1])
        cur.limt <- c(line.reads, length(ds.raw)+1)[c(read.i, read.i+1)]
        cur.range <- cur.limt[1]:(cur.limt[2]-1)
        cur.ds <- ds.raw[cur.range]
        readingTypes <- sapply(which(grepl('Time\tT', cur.ds)),
                               function(x){
                                   print(cur.ds[x-2]);
                                   strsplit(cur.ds[x-2], '\t')[[1]][1]
                                   })
        bind_rows(lapply(1:length(readingTypes), function(i){
            start.row <- cur.limt[1] +
                which(grepl('Time\tT', cur.ds))[i] - 1
            end.row   <- start.row + cur.total.reads
            read2ds(file, start.row, end.row) %>%
                mutate(readingType = readingTypes[i])
            })) %>%
            mutate(time.start = as.POSIXct(time.start.str)) %>%
            mutate(realTime = time.start + time.min * 60) %>%
            select(realTime, well, row, col, readingType, val, temp)
    }))
    out <- out %>% filter(!is.na(realTime))
    return(out)
}

#' filter matching wells
#'
#' @param .data data.frame cleaned by g5h.clean()
#' @param rows rows to be selected
#' @param cols columns to be selected
#'
#' @export
select.wells <- function(.data, rows, cols) {
    filter(.data, row %in% rows, col %in% cols)
}

#' filter matching reading type
#'
#' @param .data data.frame cleaned by g5h.clean()
#' @param rt readingType
#'
#' @export
select.reading <- function(.data, rt){
    filter(.data, grepl(rt, readingType))
}

