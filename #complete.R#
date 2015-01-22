require('stringr')    # for str_c

complete <- function(directory, id = 1:332) {
    
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

    curr_dir = getwd()
    if (!is.null(directory)) {
        tryCatch(setwd(directory))
        if (getwd() != directory) {
            stop(cat("Could not setwd to ",directory));
        }
    }
    else {
        stop("Please provide a directory within which the CSV files can be found");
    }


        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
            
    record = list (
        id = c(),
        nobs = c()
        )
    
    total_obs = 0
    
    for (idx in id) {
        ##
        ## Test for existence of datafile
        ##
        ## N.B.: Filenames have leading 0's,
        ## such that the numeric portion is
        ## exactly 3 numeric-characters long,
        ## so we need to preceed the index
        ## with the proper number of leading
        ## 0's
        ##
        
        lzeros = ""
        if(str_length(as.character(idx)) < 3) {
      
          for (zero in 1:(3 - str_length(as.character(idx))))
              lzeros = str_c(lzeros,"0")

        }
        datafile = str_c(lzeros,as.character(idx),".csv")

#        cat("Checking -- ",datafile, "\n")
        
        if (file.exists(datafile)) {
            
            
        ##
        ## open and slurp all the records
        ##
#            cat(" Reading data ", "\n")
            
            raw_data = read.csv(datafile)
            
        ## determine the number of
        ## 'complete' cases --
        ## a 'complete case' is assumed to
        ## be one without any NAs
        ##
            nobs = 0

            for (datum in 1:length(raw_data[,1])) {
                if (!anyNA(raw_data[datum,])) {
                    nobs = nobs+1
#                    cat("No NAs: ", as.character(raw_data[datum,]),"\n")
                }
#                else {
#                    cat("NAs: ", as.character(raw_data[datum,]),"\n")
#                }
            }

            record$id = c(record$id,idx)
            record$nobs = c(record$nobs,nobs)
            total_obs = total_obs + nobs

#            cat(idx, " -- ", as.character(nobs),"\n")
#            cat("Waiting ...")
#            scan()
        }
    }

    cat(as.character(total_obs), " complete observations\n")
    
    setwd(curr_dir)
    return(as.data.frame(record))
}

