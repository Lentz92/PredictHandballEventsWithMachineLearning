# PredictHandballEventsWithMachineLearning
Code used for predicting throw and locomotion events in a handball match.

**Lentz-Nielsen, Nicki**., Hart, Brian., Samani, Afshin (2022), Prediction of movement in handball with the use of Intertial Measurement Units and Machine Learning. *Under peer-review*

The code consist of 3 main .qmd files and 2 .R files
 * **00-DataPreparation.qmd:** imports, cleans and feature engineers all the data
 * **01-FeatureSelection.qmd:** Conducts two feature selection algorithms (VSURF and RFE) to select best performing features
 * **02-XgBoostModel.qmd:** The final modeling procedure
 * **import_all.R:** Function to import and join IMU data and labels from different csv files
 * **PrettyConfusionMatrix.R:** Creates a confusionmatrix with a probability hue. 
