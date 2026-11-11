# Validators of the sub-MIC data tables (models 1 and 2).
# They return list(ok, msg, ...) and the caller
# shows msg to the user when ok is FALSE.

validation_fail <- function(msg) list(ok = FALSE, msg = msg)

dataValidatorSingle <- function(data) {
  #Extract columns to compare from dataframe
  time_length = length(unlist(data[[1]])[!is.na(unlist(data[[1]]))])
  u = unlist(data[[2]][1])
  c_length = length(unlist(data[[3]])[!is.na(unlist(data[[3]]))])

  if (is.na(u) || u == 0) {
    return(validation_fail(
      "Initial cell concentration must be provided. Check the data from the first row of column nº2."))
  }
  if (time_length != c_length) {
    return(validation_fail(
      "Please, provide a value of drug concentration for each experimental time. Check the data from the column nº3"))
  }
  list(ok = TRUE, msg = NULL)
}


# Returns, when ok, the table without the empty experiments in 'data'
dataValidatorMultiple <- function(data) {
  subsetDf = data

  #Look for empty columns. Advert user that Time column (1) mustn't be
  # removed if true. Else continue with the code.
  emptyCols = getEmptyColumns(subsetDf)

  if (1 %in% emptyCols) {
    return(validation_fail("Time must be provided. Check the data from the column n1º"))
  }

  #Remove empty (NA) columns from dataframe
  if (!is.null(emptyCols)) {
    subsetDf = subset(subsetDf, select = -emptyCols)
  }

  #Extract columns to compare from dataframe
  time_length = length(unlist(subsetDf[[1]])[!is.na(unlist(subsetDf[[1]]))])

  #Get the number of no-Time columns of dataframe
  colNumber = (ncol(subsetDf) - 1) %/% 2
  if (colNumber == 0) {
    return(validation_fail("Provide the data of at least one experiment."))
  }

  for (i in seq_len(colNumber)) {
    #Extract columns and values to compare from dataframe
    initial_u = unlist(subsetDf[[(2*i)]][1])
    c_length = length(unlist(subsetDf[[(2*i+1)]])
                      [!is.na(unlist(subsetDf[[(2*i+1)]]))])

    if (is.na(initial_u) || initial_u == 0) {
      return(validation_fail(
        paste("Initial cell concentration must be provided. Check the data from the column", (i*2), ".")))
    }
    if (time_length != c_length) {
      return(validation_fail(
        paste("Please, provide a value of drug concentration for each experimental time. Check the data from the column nº",
              (i*2+1), ".")))
    }
  }
  list(ok = TRUE, msg = NULL, data = subsetDf)
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

    # Index 1 is keep to assure that Time col exist
    # If the value is different from 1, the result of adding 1 to the current
    #  value must be added to the vector. In this way, the CFU/mL columns and
    # their respective antimicrobial concentration columns will be eliminated
    # from the dataframe.

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


  return(unique(emptyCols))
  }
