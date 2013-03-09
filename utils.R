
# selects only specified subjects and removes "action" column
getSubjects <- function(data, subs)
{
  data[data$subject %in% subs, -c(562)]
}

# funcion for renaming invalid columns 
cleanDataStructure <- function(raw)
{
  colnames(raw) <- gsub("\\(\\)", "_", colnames(raw))
  colnames(raw) <- gsub("\\(", "_", colnames(raw))
  colnames(raw) <- gsub("\\)", "_", colnames(raw))
  colnames(raw) <- gsub("\\,", "_", colnames(raw))
  colnames(raw) <- gsub("\\.", "_", colnames(raw))
  colnames(raw) <- gsub("-", "_", colnames(raw))
  raw$activity <- as.factor(raw$activity)  
  
  exists <- c()
  ret <- data.frame()
  for (col in colnames(raw))
  {
    if (!any(exists == col))
    {
      exists[length(exists) + 1] <- col
    }
  }

  raw[, exists]
}