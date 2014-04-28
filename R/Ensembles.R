optimal_ensemble_ROC <- function(prediction1, prediction2, test.set.outcome, step = 0.001){
  # INPUT: 3 Vectors of numbers (0<x<1) with the 2 different predictions and the actual
  #       values from the test set (to compute ROC curve)
  
  # OUTPUT: Plot ROC on y-axis and % weight on x-axis
  #         Print a table with:
  #         Model1 ROC      Model2 ROC   Ensemble: Optimal ROC (Optimal Step Value)
  
  # Variables for tracking optimal Step Value and plot
  library(ROCR)
  max_ROC = 0
  index = 0
  plot.index = c()
  plot.ROC = c()
  iteration = 1
  
  # Loop to find step that optimizes ROC wile combining both models
  for(i in seq(0, 1, step)){
    predictions = as.numeric((prediction1*i + prediction2*(1-i)))
    Prediction.ROCR = prediction(predictions, test.set.outcome)
    ROC = as.numeric(performance(Prediction.ROCR, "auc")@y.values)
    plot.index[iteration] = i
    plot.ROC[iteration] = ROC
    iteration = iteration +1
    if(ROC > max_ROC){
      max_ROC = ROC
      index = i
    } 
  }
  # Plotting Scatterplot
  plot(plot.ROC~plot.index, pch= 10, type = "l")
  
  # ROC Values for 2 original models
  Prediction.ROCR1 = prediction(prediction1, test.set.outcome)
  ROC1 = as.numeric(performance(Prediction.ROCR1, "auc")@y.values)
  Prediction.ROCR2 = prediction(prediction2, test.set.outcome)
  ROC2 = as.numeric(performance(Prediction.ROCR2, "auc")@y.values)
  
  # Plotting Table
  cat("Model 1 ROC: ") ; cat(ROC1); cat ("     Model 2 ROC: "); cat(ROC2); cat("     Ensemble ROC: "); cat(max_ROC); cat(" (Step: ");cat(index); cat(")")
  
}

