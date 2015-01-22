require('stringr')

pollutantmean <- function(directory, pollutant, id = 1:332) {

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
    
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
    
    if (! pollutant %in% c('sulfate','nitrate')) {
        stop(cat("Pollutant must be either 'sulfate' or 'nitrate'"))
    }

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used    

    record = list (
        id = c(),
        sulfate = c(),
        nitrate = c()
        )
    
           
    for (idn in id) {

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
         
        datafile = str_c(as.character(idn),".csv");
        if (file.exists(datafile)) {
            
        ##
        ## open and slurp all the records
        ##
             
             data = read.csv(datafile)
             
         ## determine whether the selected pollutant value
         ## is not NA, add to correct vector
             
             for (datum in data) {
 
         ##
         ## pollutant == 'nitrate'
         ##
                 if (pollutant == 'nitrate') {
                     if (! is.na(datum$nitrate)) {
                         record$nitrate = c(record$nitrate,datum$nitrate)
                     }
                 }

        ##
        ## pollutant == 'sulfate'
        ##
                else if (!is.na(datum$sulfate)) { 
                    record$sulfate = c(record$sulfate,datum$sulfate)
                }
                    
            }
        }
    }

        ##
        ## calculate mean of proper pollutant
        ##

    if (pollutant == 'nitrate') {
        mean = mean(record$nitrate)
    }
    else {
        mean = mean(record$sulfate)
    }

    setwd(curr_dir)
    return(mean)

}
