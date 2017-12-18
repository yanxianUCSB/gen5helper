#' UI Clean
#'User interface for cleaning Gen5 automatic exported .TXT data
#' @param args command arguments
#'
#' @return cleaned dataset
#' @export cleaned.RDS cleaned dataset
#'
#' @examples
ui.clean <- function(args = commandArgs(trailingOnly = T)){
    if (length(args) < 1) {
        inputs <- select.list(
            title = 'Select Gen5 automatic exported .TXT data',
            choices = list.files(path = getwd(), pattern = '.txt$',
                                 all.files = F, full.names = F, include.dirs = F, ignore.case = T),
            multiple = T)
    } else if (args[1] == '-a') {
        inputs <- list.files(path = getwd(), pattern = '.txt$',
                             all.files = F, full.names = F, include.dirs = F, ignore.case = T)
    } else {
        inputs <- args
    }
    ds <- bind_rows(lapply(inputs,function(input){export2dataframe(input) %>% mutate(src = input)}))
    saveRDS(ds, 'cleaned.RDS', ascii = T)
}

#' export2dataframe
#' return cleaned data.frame from Gene5 exported tab-delim data
#' @param filename
#' @param Ctrl
#'
#' @return cleaned dataset
#' @export
#'
#' @examples
#'
export2dataframe <- function(filename, Ctrl = list(sample.by = 'row')) {

    read2ds <- function(f, start.row, end.row) {
        ds <- read.csv(f, header = T, nrows = end.row-start.row, skip = start.row-1, sep = '\t', stringsAsFactors = F)
        ds2 <- gather(ds, 'well', 'val', 3:ncol(ds)) %>%
            filter(!is.na(val)) %>%
            separate(well, c('row', 'col'), sep = 1, remove = F)
        names(ds2)[2] <- 'temp'
        # filter out masked wells
        maskedRowCol <- ds2 %>%
            filter(grepl('\\*', val)) %>%
            select(row, col)
        ds3 <- ds2 %>%
            filter( !((row %in% unique(maskedRowCol$row)) & (col %in% unique(maskedRowCol$col))) ) %>%
            mutate( val = as.numeric(val)) %>%
            mutate(
                time.min = as.numeric(
                    difftime(strptime(as.character(Time), format = '%H:%M:%S'),
                             strptime(as.character(Time[1]), format = '%H:%M:%S'), units = 'min')))}

    print(filename)
    ds.raw <- readLines(filename)
    line.date.times <- which(grepl('Date\t', ds.raw))
    line.procedures <- which(grepl('Procedure Details', ds.raw))
    line.reads <- line.procedures - 1 + (grep('Start Kinetic', ds.raw[line.procedures[1]:(line.procedures[1]+20)]))

    if (length(line.reads) > 0) {

        out <- bind_rows(lapply(1:length(line.reads), function(read.i){

            line.read <- line.reads[read.i]
            line.date.time <- line.date.times[read.i]
            time.start.str <- strptime(paste(strsplit(ds.raw[line.date.time],'\t')[[1]][2],
                                         strsplit(ds.raw[line.date.time+1], '\t')[[1]][2]),
                                   format = '%m/%d/%Y %I:%M:%S %p',
                                   tz = '')
            l <- strsplit(ds.raw[line.reads[read.i]], ' ')[[1]]
            cur.total.reads <- as.integer(l[which(l == 'Reads') - 1])
            cur.limt <- c(line.reads, length(ds.raw)+1)[c(read.i, read.i+1)]
            cur.range <- cur.limt[1]:(cur.limt[2]-1)
            cur.ds <- ds.raw[cur.range]
            readingTypes <- sapply(which(grepl('Time\tT', cur.ds)),
                                   function(x){print(cur.ds[x-2]); strsplit(cur.ds[x-2], '\t')[[1]][1]})

            bind_rows(lapply(1:length(readingTypes), function(i){

                start.row <- cur.limt[1] + which(grepl('Time\tT', cur.ds))[i] - 1
                end.row   <- start.row + cur.total.reads
                read2ds(filename, start.row, end.row) %>%
                    mutate(readingType = readingTypes[i])

            })) %>%
                mutate(time.start = as.POSIXct(time.start.str)) %>%
                mutate(
                    realTime = time.start + time.min * 60
                ) %>%
                select(realTime, well, row, col, readingType, val, temp)

        }))

    } else {

        warning('Input Format Not Defined')
        out <- NULL

    }
    out <- out %>% filter(!is.na(realTime))
    return(out)
}
