pmean <- function(directory, pollutant, id=1:332){
  common_mean  <- 0
  
  for(file_id in id){
    file_name <- paste(file_id)
    
    while(nchar(file_name) < 3){
      file_name <- paste(0, file_name, sep = "")
    }
    file_name <- paste(file_name, ".csv", sep = "")
    
    file_path <- file.path(directory, file_name, fsep = .Platform$file.sep)
    
    read_data <- read.csv(file_path)
    
    if(length(read_data[[pollutant]][!is.na(read_data[[pollutant]])]) == 0){
      current_mean <- 0
    }
    else{
      current_mean <-  mean(read_data[[pollutant]][!is.na(read_data[[pollutant]])])
    }
    
    common_mean <- common_mean + current_mean
  }
  
  return(common_mean / length(id))
}

test <- pmean("specdata", "t", 1)
print(test)

