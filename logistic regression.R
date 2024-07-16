install.packages("tidyverse")
library(tidyverse)
library(clipr)
library(dplyr)
library(stringi)
library(caret)
library(pROC)
library(gtsummary)
data <- read.csv('dataset v3.csv', encoding = "UTF-8")
# Check data type
str(data)
# display the first few rows
head(data)

empty_cols <- colSums(is.na(data))==nrow(data)
data <- data[,!empty_cols]
head(data)
# Display summary stats
summary(data)
# Create a new factor variable "Outcome" based on "改良RANKIN"
data$Outcome <- factor(ifelse(data$改良RANKIN <= 1, "Good", "Poor"), levels = c("Good", "Poor"))
# Check the levels of the new "Outcome" variable
levels(data$Outcome)
# Combine 斑块大小mm and 斑块位置 into Plaque_Location
data <- data %>% 
  unite(Plaque_Location, 斑块大小mm, 斑块位置, sep = "_", remove = FALSE, na.rm = FALSE) %>%
  mutate(Plaque_Location = factor(Plaque_Location))

# Convert the factor column back to character
data$Plaque_Location <- as.character(data$Plaque_Location)


# Create a new column `Plaque_Location_Group` based on the mapping
#Combined Anterior Circulation: We now group ICA and MCA under "Anterior Circulation (ICA/MCA)."
#Combined Posterior Circulation: We group BA, 双侧M1, and 双侧C7 under "Posterior Circulation (BA/M1/C7)."
#Other Categories: ACA, VA, and Other remain as separate categories.
#Updated Categorization Logic: The categorize_plaque_location function is modified to reflect these new groupings.

data$Plaque_Location_Group = ifelse(grepl("LC|RC", data$Plaque_Location), "ICA",
                                     ifelse(grepl("ACA|LA|RA|LM|RM", data$Plaque_Location), "MCA",
                                            ifelse(grepl("VA|RV|LV", data$Plaque_Location), "VA",
                                                   ifelse(grepl("双侧|BA|环", data$Plaque_Location), "PC)", 
                                                          "Other"))))

# Convert the new column to a factor variable
data$Plaque_Location_Group = factor(data$Plaque_Location_Group, levels = c("ICA", "MCA", "VA", "PC", "Other"))


# Find rows where Plaque_Location_Group is unassigned (i.e., has the level "O")
unassigned_rows <- which(is.na(data$Plaque_Location_Group))
# Display the unmatched values
unassigned_rows$Plaque_Location

# Assign "PC" to rows where Plaque_Location_Group is NA (unassigned)
data$Plaque_Location_Group[unassigned_rows] <- "PC"

# Update the factor levels to include "PC" if it wasn't already there
data$Plaque_Location_Group <- factor(data$Plaque_Location_Group, levels = unique(c(levels(data$Plaque_Location_Group), "PC"))) 

# Verify that all values have been assigned
table(data$Plaque_Location_Group)

# Fill in missing values with "Missing/Unknown"
data$斑块厚度mm[is.na(data$斑块厚度mm)] <- "Missing/Unknown"

# Create a new categorical variable for 斑块厚度mm
data$Plaque_Thickness_Group <- ifelse(grepl("[^0-9.]", data$斑块厚度mm), data$斑块厚度mm, "Numeric")  # Categorize based on numeric vs. non-numeric

# Convert to a factor with the appropriate levels
data$Plaque_Thickness_Group <- factor(data$Plaque_Thickness_Group, 
                                      levels = c("不均匀增厚", "Missing/Unknown", "Numeric"))  

# Check unique values and their frequencies
table(data$Plaque_Thickness_Group)

# Find rows where Plaque_Location_Group is unassigned (i.e., has the level "O")
unassigned_rows2 <- which(is.na(data$Plaque_Thickness_Group))
# Display the unmatched values
data$Plaque_Thickness_Group[13] <- "Numeric"

# Ensure stringi is loaded
library(stringi)

# Update the existing Plaque_Location_Group column
data$Plaque_Location_Group <- ifelse(stri_detect_fixed(data$Plaque_Location, "LC") | stri_detect_fixed(data$Plaque_Location, "RC"), "ICA",
                                     ifelse(stri_detect_fixed(data$Plaque_Location, "ACA") | stri_detect_fixed(data$Plaque_Location, "LA") | stri_detect_fixed(data$Plaque_Location, "RA") | stri_detect_fixed(data$Plaque_Location, "LM") | stri_detect_fixed(data$Plaque_Location, "RM"), "MCA",
                                            ifelse(stri_detect_fixed(data$Plaque_Location, "VA") | stri_detect_fixed(data$Plaque_Location, "RV") | stri_detect_fixed(data$Plaque_Location, "LV"), "VA",
                                                   ifelse(stri_detect_fixed(data$Plaque_Location, "双侧") | stri_detect_fixed(data$Plaque_Location, "BA") | stri_detect_fixed(data$Plaque_Location, "环"), "PC",
                                                          "Other"))))

# Convert to factor with correct levels
data$Plaque_Location_Group <- factor(data$Plaque_Location_Group, levels = c("ICA", "MCA", "VA", "PC", "Other"))

# Extract numeric plaque thickness values
data$Plaque_Thickness_Numeric <- as.numeric(ifelse(data$Plaque_Thickness_Group == "Numeric", as.character(data$斑块厚度mm), NA))

# Impute missing values in the new numeric variable
data$Plaque_Thickness_Numeric[is.na(data$Plaque_Thickness_Numeric)]<-mean(data$Plaque_Thickness_Numeric,na.rm = TRUE)

# Update the 斑块T1信号 column
data$Plaque_T1_Signal_Group <- ifelse(stri_detect_fixed(data$斑块T1信号, "高信号"), "High",
                                     ifelse(stri_detect_fixed(data$斑块T1信号, "稍低信号"), "Low",
                                            ifelse(stri_detect_fixed(data$斑块T1信号, "等信号"), "Mid",
                                            "Unknown")))
data$Plaque_T1_Signal_Group <- factor(data$Plaque_T1_Signal_Group, levels = c("High", "Low","Mid","Unknown"))

# Update the 斑块强化程度 column
data$Plaque_Enhancement_Group <- # Ensure stringi is loaded
  library(stringi)

# Create 斑块强化程度 (binary)
data$Plaque_Enhancement_Group <- ifelse(data$斑块强化程度 == "强化", 1, 0)

data$Plaque_Enhancement_Group <- factor(data$Plaque_Enhancement_Group, levels = c(1,0))

# Create 偏心斑块 (binary)
data$Plaque_Eccentric_Group <- ifelse(data$偏心斑块 == "是", 1, 0)

data$Plaque_Eccentric_Group <- factor(data$Plaque_Eccentric_Group, levels = c(1,0))

# Create 斑块易损 (binary)
data$Plaque_Vul_Group <- ifelse(data$斑块易损 == "是", 1, 0)

data$Plaque_Vul_Group <- factor(data$Plaque_Vul_Group, levels = c(1,0))

# Update the PWI column
data$Plaque_PWI_Group <- ifelse(stri_detect_fixed(data$PWI, "左"), "L",
                                      ifelse(stri_detect_fixed(data$PWI, "右"), "R",
                                             ifelse(stri_detect_fixed(data$斑块T1信号, "双"), "Both",
                                                    "Other")))
data$Plaque_PWI_Group <- factor(data$Plaque_PWI_Group, levels = c("L", "R","Both","Other"))

# Update the TTP column
data$Plaque_TTP_Group <- ifelse(stri_detect_fixed(data$TTP, "左"), "L",
                                      ifelse(stri_detect_fixed(data$TTP, "右"), "R",
                                                    "Other"))
data$Plaque_TTP_Group <- factor(data$Plaque_TTP_Group, levels = c("L", "R","Other"))

# Update the MTT column
data$Plaque_MTT_Group <- ifelse(stri_detect_fixed(data$MTT, "左"), "L",
                                ifelse(stri_detect_fixed(data$MTT, "右"), "R",
                                       "Other"))
data$Plaque_MTT_Group <- factor(data$Plaque_MTT_Group, levels = c("L", "R","Other"))

# Update the CBV column
data$Plaque_CBV_Group <- ifelse(stri_detect_fixed(data$CBV, "左"), "L",
                                ifelse(stri_detect_fixed(data$CBV, "右"), "R",
                                       "Other"))
data$Plaque_CBV_Group <- factor(data$Plaque_CBV_Group, levels = c("L", "R","Other"))

# Update the CBF column
data$Plaque_CBF_Group <- ifelse(stri_detect_fixed(data$CBF, "左"), "L",
                                ifelse(stri_detect_fixed(data$CBF, "右"), "R",
                                       "Other"))
data$Plaque_CBF_Group <- factor(data$Plaque_CBF_Group, levels = c("L", "R","Other"))

# Update the Tmax column
data$Plaque_Tmax_Group <- ifelse(stri_detect_fixed(data$Tmax, "左"), "L",
                                ifelse(stri_detect_fixed(data$Tmax, "右"), "R",
                                       "Other"))
data$Plaque_Tmax_Group <- factor(data$Plaque_Tmax_Group, levels = c("L", "R","Other"))

##########################################################################################
##########################################################################################

# Select only factor columns and the outcome variable
data_factors <- data %>% 
  select(where(is.factor), Outcome)  

set.seed(123)  # Set random seed for reproducibility
trainIndex <- createDataPartition(data_factors$Outcome, p = .8, list = FALSE)
train_data <- data_factors[trainIndex, ]
test_data  <- data_factors[-trainIndex, ]

# Fit the logistic regression model
model <- glm(Outcome ~ ., data = train_data, family = binomial)

############################### Print the model summary
summary(model)

# Predict probabilities on the test set
probs <- predict(model, newdata = test_data, type = "response")

# Convert probabilities to class predictions (0 or 1)
pred_class <- ifelse(probs > 0.5, "Poor", "Good")

###########################################################################################
# Generate confusion matrix
cm <- confusionMatrix(factor(pred_class, levels = levels(test_data$Outcome)), 
                      test_data$Outcome)

# Calculate additional performance metrics
accuracy <- cm$overall["Accuracy"]
sensitivity <- cm$byClass["Sensitivity"]
specificity <- cm$byClass["Specificity"]

# Extract F1 score from confusion matrix
f1_score <- cm$byClass["F1"] 


# Print the confusion matrix and performance metrics
print(cm)
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("f-measure:", f1_score, "\n")


############################################################################################

#####################            ROC                      #################################

# Calculate predicted probabilities on the test set
test_prob <- predict(model, newdata = test_data, type = "response")

# Create ROC object and calculate AUC (Area Under the Curve)
roc_obj <- roc(test_data$Outcome, test_prob)

# Plot ROC curve
plot(roc_obj, print.auc = TRUE, main = "ROC Curve", print.auc.y = 0.4)

###################################################################################
#####################          gtsummary                  ##########################
# Create a gtsummary table of the logistic regression results
table1 <- tbl_regression(model, exponentiate = TRUE)

# Print the table to the console
table1

# Save the table as a Word document (requires flextable package)
install.packages("flextable")
library(flextable)
tbl_regression(model) %>%
  as_flex_table() %>%
  save_as_docx(path = "model_results.docx")
