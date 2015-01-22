## Common code to get into the correct data store

setdir <- function(directory) {
    
    if (!is.null(directory)) {
        tryCatch(setwd(directory))
        if (getwd() != directory) {
            stop(cat("Could not setwd to ",directory));
        }
    }
    else {
        stop("Please provide a directory within which the CSV files can be found");
    }
}   
    
    
    
    
    
    
    
    
    
