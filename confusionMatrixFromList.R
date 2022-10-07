confusionMatrixFromList <- function(modelList){

  confusionMatrixList <- list()
  #Calculate confusionmatrix for each model
  for (i in 1:length(modelList)){
    table <- data.frame(caret::confusionMatrix(
      modelList[[i]]$.pred_class, 
      modelList[[i]]$class)$table
    ) %>% 
      mutate(model = i)
    
    confusionMatrixList[[i]] <- table
  }
  
  #unnest the confusionamtrices
  confusionMatrix_binded <- confusionMatrixList %>% 
    bind_rows() %>% 
    distinct()
  
  #Calculate the mean probability of all combination across all models
  plotTable <- confusionMatrix_binded %>%
    mutate(Predicted = ifelse(confusionMatrix_binded$Prediction == confusionMatrix_binded$Reference, "True", "False")) %>%
    group_by(Reference, model) %>%
    mutate(Probability = Freq/sum(Freq)) %>% 
    ungroup() %>% 
    group_by(Prediction, Reference) %>% 
    summarise(meanProbability = round(mean(Probability),2),
              sdProbability = round(sd(Probability),2)) %>% 
    ungroup() %>% 
    mutate(results = paste(meanProbability, sdProbability, sep = "±"))
  
  #plot
  plotTable %>% 
    ggplot(mapping = aes(x = Reference, y = Prediction, alpha = meanProbability)) +
    geom_tile() +
    geom_text(aes(label = results), size = 16, fontface  = "bold", alpha = 1) +
    theme_minimal() + 
    xlim(rev(levels(table$Reference))) + #reverse x axis order
    labs(x = "Actual") + 
    theme(legend.title = element_text(size = 35, face = "bold"),
          legend.key.size = unit(3, 'cm'),
          axis.title = element_text(size = 40, face = "bold"),
          text = element_text(size = 35)) +
    scale_alpha_continuous("Probability", range = c(0.0, 1.0), labels = c("0.00", "0.25", "0.5", "0.75", "1.00"))
  
}
