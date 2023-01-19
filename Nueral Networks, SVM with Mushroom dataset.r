library(e1071) #Naive Bayes
library(kernlab) #SVM CLassifier

## Scott Schirkofsky
## June 7, 2020

rm(list=ls()) #remove base objects to clear workspace

##Collecting data/load CSV
shrooms <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), header = FALSE, sep = ",")

##Data Exploration
str(shrooms) #
#lapply(shrooms, summary) #apply summary function on shrooms dataset

##Data Transformation / Preparation
colnames(shrooms) <- c("edibility", "cap_shape", "cap_surface", "cap_color", "bruises", "odor", "grill_attachment", "grill_spacing", "grill_size","grill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")
sum(is.na(shrooms)) #check for NAs, none found
#shrooms <- subset(x = shrooms, select = -veil_type) #remove veil_type
shrooms$veil_type <- NULL # remove veil_type column
str(shrooms) #
#table(shrooms$stalk.root) # Inspect

shrooms$cap_shape <- as.numeric(shrooms$cap_shape)
shrooms$cap_surface <- as.numeric(shrooms$cap_surface)
shrooms$cap_color <- as.numeric(shrooms$cap_color)
shrooms$bruises <- as.numeric(shrooms$bruises)
shrooms$odor <- as.numeric(shrooms$odor)
shrooms$grill_attachment <- as.numeric(shrooms$grill_attachment)
shrooms$grill_spacing <- as.numeric(shrooms$grill_spacing)
shrooms$grill_size <- as.numeric(shrooms$grill_size)
shrooms$grill_color <- as.numeric(shrooms$grill_color)
shrooms$stalk_shape <- as.numeric(shrooms$stalk_shape)
shrooms$stalk_root <- as.numeric(shrooms$stalk_root)
shrooms$stalk_surface_above_ring <- as.numeric(shrooms$cap_shape)
shrooms$stalk_surface_below_ring <- as.numeric(shrooms$cap_shape)
shrooms$stalk_color_above_ring <- as.numeric(shrooms$stalk_color_above_ring)
shrooms$stalk_color_below_ring <- as.numeric(shrooms$stalk_color_below_ring)
shrooms$veil_color <- as.numeric(shrooms$veil_color)
shrooms$ring_number <- as.numeric(shrooms$ring_number)
shrooms$ring_type <- as.numeric(shrooms$ring_type)
shrooms$spore_print_color <- as.numeric(shrooms$spore_print_color)
shrooms$population <- as.numeric(shrooms$population)
shrooms$habitat <- as.numeric(shrooms$habitat)

colnames(shrooms) <- c("edibility", "cap_shape", "cap_surface", "cap_color", "bruises", "odor", "grill_attachment", "grill_spacing", "grill_size","grill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")
sum(is.na(shrooms)) #check for NAs, none found
shrooms <- subset(x = shrooms, select = -veil_type) #remove veil_type
levels(shrooms$'stalk.root')[levels(shrooms$'stalk.root')== "?"] <- "u" # 
table(shrooms$stalk.root) # Inspect Again

levels = c("e","p")
labels=c("edible","poisonous")

shrooms <- as.data.frame(lapply(shrooms, as.numeric)) # Change all rows to numeric
shrooms$class <- factor(shrooms$class)                # Convert class back to factor
str(shrooms)

set.seed(77)                                          # Get the same data each time
idx <- sample(nrow(shrooms), round(nrow(shrooms)*0.7))  # Create 2 samples with ratio 70:30
shrooms_train <- shroom[idx, ]                         # Split 70%
shrooms_test <- shroom[-idx, ]                         # Split 30%

##Train the model
shrooms_model <- svm(formula = class ~ ., data = shrooms_train, kernel = "linear")
summary(shrooms_model)

##Test the Model
shrooms_pred <- predict(shrooms_model, shrooms_test)
summary(shrooms_pred)

##Evaluating Accuracy
# Returns the percentage of correct predictions
get.accuracy <- function(prediction, real) {
  accuracy <- prediction == real
  return (length(accuracy[accuracy == TRUE])/length(accuracy))
}
get.accuracy(shrooms_pred, shrooms_test$class)


