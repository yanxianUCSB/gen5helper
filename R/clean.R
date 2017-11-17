#' export2dataframe
#' return cleaned data.frame from Gene5 exported tab-delim data
#' @param filename
#' @param Ctrl
#'
#' @return
#' @export
#'
#' @examples
#'
export2dataframe <- function(filename, Ctrl = list(sample.by = 'row')) {
    read2ds <- function(f, start.row, end.row) {
        ds <- read.csv(f, header = T, nrows = end.row-start.row, skip = start.row-1, sep = '\t')
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
    ds <- readLines(filename)
    line.DateTime <- which(grepl('Date\t', ds))
    start.Time <- strptime(paste(strsplit(ds[line.DateTime],'\t')[[1]][2],
                                 strsplit(ds[line.DateTime+1], '\t')[[1]][2]),
                           format = '%m/%d/%Y %I:%M:%S %p',
                           tz = '')
    line.procedure <- which(grepl('Procedure Details', ds))
    line.reads <- line.procedure - 1 + (grep('Start Kinetic', ds[line.procedure:(line.procedure+20)]))
    ifKinetics <- length(line.reads) == 1
    if (ifKinetics) {
        l <- strsplit(ds[line.reads], ' ')[[1]]
        total.reads <- as.integer(l[which(l == 'Reads') - 1])
        # readingTypes <- sapply(which(grepl('Read\t', ds)), function(x){strsplit(ds[x], '\t')[[1]][2]})
        readingTypes <- sapply(which(grepl('Time\tT', ds)), function(x){print(ds[x-2]); strsplit(ds[x-2], '\t')[[1]][1]})
        out <- bind_rows(lapply(1:length(readingTypes), function(i){
            start.row <- which(grepl('Time\tT', ds))[i]
            end.row   <- start.row + total.reads
            read2ds(filename, start.row, end.row) %>%
                mutate(readingType = readingTypes[i])
        })) %>%
            mutate(time.start = as.POSIXct(start.Time)) %>%
            mutate(
                row = as.factor(row),
                realTime = time.start + time.min * 60,
                readingType = factor(readingType)
            ) %>%
            select(realTime, well, row, col, readingType, val, temp)
    } else {
        warning('Input Format Not Defined')
        out <- NULL
    }
    out <- out %>% filter(!is.na(realTime))
    return(out)
}
