get_confusion_matrix <- function(umbral, predicciones, test_data) {
  
  predictions_test <- if_else(predicciones >= umbral, 1, 0) 
  df <- data.frame(real = test_set$survived, pred = predictions_test)
  confusion <- table(df$real, df$pred)
  
  
  # Si solo hay predicciones positivas
  if (sum(df$pred == 1 ) == nrow(df) ) {
    `0` = c(0, 0)
    return (cbind(`0`, confusion)) 
    # Si solo hay predicciones negativas
  } else if (sum(df$pred == 0 ) == nrow(df)) {
    `1` = c(0, 0)
    return(cbind(confusion,   `1`))
  } else {
    return(confusion)
  }
  
  
}


get_tpr <- function(confusion) {
  tp = confusion[2, 2]  
  fn = confusion[2, 1]
  tp / (tp + fn)
}

get_fpr <- function(confusion) {
  fp = confusion[1, 2]
  tn = confusion[1 ,1]
  fp / (fp + tn)
  
  
}

