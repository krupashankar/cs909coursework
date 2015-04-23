measures<- function(data_matrix)
{
  num_col<- ncol(data_matrix)
  num_row<- nrow(data_matrix)
  if(num_col!=num_row) { cat("Incorrect table "); return(-1)}
  
  measures<- NULL
  
  for(i in 1:num_col)
  {  for(j in 1:num_row)
     {
        if(i==j){
        # TRUE POSITIVES
        TP =  data_matrix[i,j]
        
        # sum of all the values in the current row beside the current cell - FALSE NEGATIVES
        FN = sum(data_matrix[j,])- data_matrix[i,j]
        
        # sum of all the values in the current column except the current cell - FALSE POSITIVES
        FP = sum(data_matrix[,i])- data_matrix[i,j]
        
        # Sum of the remaining matrix except - TRUE NEGATIVES
        TN = sum(data_matrix) - (TP + FN + FP)
  
        measures<- rbind(measures,c(TP,FN,FP,TN))
        }
      }
  }
  
    colnames(measures)<- c("True Positive","False Negative","False Positive","True Negative")
  rownames(measures)<- colnames(data_matrix)
  measures[is.nan(measures)]<-0
  
  recall<- round((measures[,"True Positive"] / (measures[,"True Positive"] + measures[,"False Negative"])),2)
  precision<-round((measures[,"True Positive"] / (measures[,"True Positive"] + measures[,"False Positive"])),2)
  f_measure<- round(((2*precision * recall) / (precision + recall)),2)
  
  measures<- cbind(measures,recall)
  measures<- cbind(measures,precision)
  measures<- cbind(measures,f_measure)
  measures[is.nan(measures)]<-0
  colnames(measures)<- c(colnames(measures)[1:4],"recall","precision","f_measure")
  return(measures)
}




average_measures <- function(data_matrix)
{
  num_col<- ncol(data_matrix)
  num_row<- nrow(data_matrix)
  if(num_col!=num_row) { cat("Incorrect table "); return(-1)}
  
  measures<- NULL
  TP_sum    <- 0
  TP_FP_sum <- 0
  TP_FN_sum <- 0
  
  for(i in 1:num_col)
  {  for(j in 1:num_row)
  {
    if(i==j){
      # TRUE POSITIVES
      TP =  data_matrix[i,j]
      
      # sum of all the values in the current row beside the current cell - FALSE NEGATIVES
      FN = sum(data_matrix[j,])- data_matrix[i,j]
      
      # sum of all the values in the current column except the current cell - FALSE POSITIVES
      FP = sum(data_matrix[,i])- data_matrix[i,j]
      
      # Sum of the remaining matrix except - TRUE NEGATIVES
      TN = sum(data_matrix) - (TP + FN + FP)
      
      measures<- rbind(measures,c(TP,FN,FP,TN))
      
      TP_sum = TP_sum + TP
      TP_FP_sum <- TP_FP_sum + TP + FP
      TP_FN_sum <- TP_FN_sum + TP + FN
      
      
    }
  }
  }
  colnames(measures)<- c("True Positive","False Negative","False Positive","True Negative")
  rownames(measures)<- colnames(data_matrix)
  measures[is.nan(measures)]<-0
  micro_recall<- round((TP_sum / TP_FN_sum),2)
  micro_precision <- round((TP_sum / TP_FP_sum),2)
  macro_recall<-round((mean(measures[,"True Positive"] / (measures[,"True Positive"] + measures[,"False Negative"]))),2)
  macro_precision<-round((mean(measures[,"True Positive"] / (measures[,"True Positive"] + measures[,"False Positive"]))),2)
  accuracy<-round((sum(diag(data_matrix))/sum(data_matrix)),2);

  average_measures1 <- c(micro_recall,micro_precision,macro_recall,macro_precision,accuracy)
  #colnames(average_measures1)<- c("micro_recall","micro_precision","macro_recall","macro_precision","accuracy")
  average_measures1[is.nan(average_measures1)]<-0
  return(average_measures1)
}



is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
