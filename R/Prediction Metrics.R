prediction_metrics <- function (predictions, test.set){
  # INPUT: 1 Vector of Predictions, 1 Vector of the actual values from the test set
  
  # OUTPUT: Print Confusion Matrix
  #         Print Accuracy, Specificity, Sensitivity, Recall, Precision
  
  # Compute Confusion Matrix
  Prediction.Metrics.Table = table(test.set, predictions > 0.5)
  
  # Print Confusion Matrix and corresponding symbols
  cat("
    Confusion Matrix
        TN    FP  
        FN    TP  
      ")
  print(Prediction.Metrics.Table)
  
  # Calculate 4 Different Values of the Matrix
  True.Positive =  Prediction.Metrics.Table[2,2]
  True.Negative =  Prediction.Metrics.Table[1,1]
  False.Positive = Prediction.Metrics.Table[1,2]
  False.Negative = Prediction.Metrics.Table[2,1]
  Table.Sum = sum(Prediction.Metrics.Table)
  
  # Compute All the Statistics
  Accuracy = (True.Positive + True.Negative)/(Table.Sum)
  Specificty = (True.Negative)/(True.Negative + False.Positive)
  Recall =  (True.Positive)/(True.Positive + False.Negative) # Or Sensitivity
  Precision = (True.Positive)/(True.Positive + False.Positive)
  Negative.Predictive.Value = (True.Negative)/(True.Negative + False.Negative) 
  Fall.out = (False.Positive)/(False.Positive + True.Negative)
  False.Discovery.Rate = (False.Positive)/(False.Positive + True.Positive)
  F1.Score = (2*True.Positive)/((2*True.Positive)+False.Positive + False.Negative)
  
  # Print the values
  cat ("\n")
  cat("Accuracy:   "); cat(Accuracy); cat("    Formula: (TP + TN)/(TP+TN+FP+FN)")
  cat("\nSpecificty: "); cat(Specificty); cat("    Formula: (TN)/(TN+FP)")
  cat("\nRecall:     "); cat(Recall); cat("    Formula: (TP)/(TP+FN)")
  cat("\nF1 Score:   "); cat(F1.Score); cat("    Formula: (2*TP)/((2*TP)+FP+FN)")
  
}



