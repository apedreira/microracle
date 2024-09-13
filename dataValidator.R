dataValidatorSingle <- function(data) {
  msg = NULL
  
  #Extract columns to compare from dataframe
  time_length = length(unlist(data[[1]])[!is.na(unlist(data[[1]]))])
  u = unlist(data[[2]][1])
  
  
  c_length = length(unlist(data[[3]])[!is.na(unlist(data[[3]]))])

  
  if(u == 0 || is.na(u)) {
    msg= showNotification(
      "Initial cell concentration must be provided. Check the data from the first row of column nº2.",
      type= "error")

  }
  
  else if(time_length != c_length){
    msg=showNotification(
      "Please, provide a value of drug concentration for each experimental time. Check the data from the column nº3",
      type= "error"
    )

  }
  
  
  else  {
    msg = "OK"
  }
  
  return(msg)
  
}

dataValidatorMultiple <- function(data) {
  
  msg = NULL
  subsetDf = data
  
  #Look for empty columns. Advert user that Time column (1) mustn't be 
  # removed if true. Else continue with the code.
  
  emptyCols = getEmptyColumns(subsetDf)
  
  if (1 %in%   emptyCols) {
    msg= showNotification(
      "Time must be provided. Check the data from the column n1º",
      type= "error")
  }
  
  else {
    
    #Remove empty (NA) columns from dataframe
    
    if(!is.null(emptyCols)) {
      
      subsetDf = subset(subsetDf, select = -emptyCols)
    }
    

    #Extract columns to compare from dataframe
    time_length = length(unlist(subsetDf[[1]])[!is.na(unlist(subsetDf[[1]]))])

    #Get the number of no-Time columns of dataframe
    #colNumber = ncol(subsetDf)/2-1
    colNumber = ncol(subsetDf)/2
    print (colNumber)
    #A bucle which iterates over dataframe 1:5 is for the number of columns (10), but must be
    for (i in 1:colNumber){
      print (i)
      #Extract columns and values to compare from dataframe
      initial_u = unlist(subsetDf[[(2*i)]][1])
      c_length = length(unlist(subsetDf[[(2*i+1)]])[!is.na(unlist(subsetDf[[(2*i+1)]]))])

      if(initial_u == 0 || is.na(initial_u)) {
        if (i==1){
        msg= showNotification(
          paste("Initial cell concentration must be provided. Check the data from the column",(i*2),"."),
          type= "error")
          break
        }
        
        else {
          msg= showNotification(
          paste("Initial cell concentration must be provided. Check the data from the column",(i*2),"."),
          type= "error")
        break
      }

      }
      else if(time_length != c_length){
        msg = showNotification(
          paste("Please, provide a value of drug concentration for each experimental time. Check the data from the column nº",(i*2+1),"."),
          type = "error"
        )
        break
      }

      else  {
        msg = "OK"
      }

    }
  }
  
  return(list(msg,subsetDf))
}


getEmptyColumns <- function(data) {

  # Check if any column contains just NA values
  emptyCols = c()
  for (col_index in seq_len(ncol(data))) {
    if (all(is.na(data[[col_index]]))) {
      emptyCols <- c(emptyCols, col_index)
      }
  }
  
    if(length(emptyCols) > 0) {
    
    # Remove odd index from emptyCols vector (cell concentrations has even index)
    # despite be odd, 1 is keep to assure that Time col exist
    # If the value is different from 1, the result of adding 1 to the current value
    # must be added to the vector. In this way, the CFU/mL columns and their respective
    # antimicrobial concentration columns will be eliminated from the dataframe.
    
    for (i in 1:length(emptyCols)) {
      
      col_index = emptyCols[i]
      
      if (col_index != 1 && col_index %% 2 == 0 ) {
        emptyCols = c(emptyCols, col_index + 1)
      }
       if (col_index >= 2 && col_index %% 2 != 0 ) {
        emptyCols = c(emptyCols, col_index - 1)
       }
    }
  }
  
  print(emptyCols)
  return(emptyCols)
  }

