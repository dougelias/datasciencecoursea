corr <- function(directory, threshold = 0) {
   
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    curr_dir = getwd()
    
    if (!is.null(directory)) {
        result = tryCatch(setwd(directory))
        if ( getwd() != directory) {
            stop(cat("Could not setwd to ", directory,'(', result,')'));
        }
    }
    else {
        stop("Please provide a directory within which the CSV files can be found");
    }

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    record = list (
        sulfate = c(),
        nitrate = c()
        )
    
    files = list.files()
    for (file in files) {
        range = str_locate(file, '.csv')
        if (! anyNA(range)) {
            if (file.exists(file)) {
            
            
        ##
        ## open and slurp all the records
        ##

              raw_data = read.csv(file)
            
        ## determine the number of
        ## 'complete' cases --
        ## a 'complete case' is assumed to
        ## be one without any NAs
        ##
              for (datum in 1:length(raw_data[,1])) {
                  if (!anyNA(raw_data[datum,])) {
                      record$sulfate = c(record$sulfate, raw_data[datum,]$sulfate)
                      record$nitrate = c(record$nitrate, raw_data[datum,]$nitrate)
                  }
              }
          }
        }
    }
    if (length(record[,1]) >= threshold) {
        p_correlation = corr(as.matrix(record, type="pearson"))
        s_correlation = corr(as.matrix(record, type='spearman'))            
    }
    
    ## Return a numeric vector of correlations

    setwd(curr_dir)
    return(c(p_correlation,s_correlation))
}
