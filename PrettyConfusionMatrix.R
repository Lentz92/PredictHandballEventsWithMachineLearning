PrettyConfusionMatrix <- function(actuel, predicted){
  table <- data.frame(caret::confusionMatrix(predicted, actuel)$table)
  
  plotTable <- table %>%
    mutate(Predicted = ifelse(table$Prediction == table$Reference, "True", "False")) %>%
    group_by(Reference) %>%
    mutate(Probability = Freq/sum(Freq))
  
  # fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups 
  ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, alpha = Probability)) +
    geom_tile() +
    geom_text(aes(label = Freq), size = 25, fontface  = "bold", alpha = 1) +
    theme_minimal() + 
    xlim(rev(levels(table$Reference))) + #reverse x axis order
    labs(x = "Actual") + 
    theme(legend.title = element_text(size = 35, face = "bold"),
          legend.key.size = unit(3, 'cm'),
          axis.title = element_text(size = 40, face = "bold"),
          text = element_text(size = 35)) +
    scale_alpha_continuous(range = c(0.0, 1.0), labels = c("0.00", "0.25", "0.5", "0.75", "1.00"))
}
