## ----setup, include=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------
# load libraries
if(!require(gtools)) install.packages("gtools", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

# The library "stringr" for string manipulation is loaded via dependencies due to the library "tidyverse"

options(digits = 5) # Number of significant digits in numerical results

knitr::opts_chunk$set(echo = TRUE)

# prepare actual date to print (European Format)
AktDatum <- ymd(today())
AktDatumS <- sprintf("%d.%d.%d", day(AktDatum), month(AktDatum), year(AktDatum))

# read the data file
htdis_data <- read.csv("heart_statlog_cleveland_hungary_final.csv", header = TRUE, sep = ",")

# display header data
head(htdis_data)

# display additional information about data file content
str(htdis_data)

#check for missing values (N/A) in the columns and display as a table

knitr::kable(colSums(is.na(htdis_data)),col.names = c("Variable","N"), caption = "Missing values")

# create a new data frame and change variables from numerical to factorial values (i.e. continuous vs. levels), where necessary

htdis_datax <- htdis_data
htdis_datax$sex <- if_else(htdis_data$sex == 0, "female", "male")
htdis_datax$sex <- as.factor(htdis_datax$sex)
htdis_datax$chest.pain.type <- as.factor(htdis_data$chest.pain.type)
htdis_datax$fasting.blood.sugar <- as.factor(htdis_data$fasting.blood.sugar)
htdis_datax$resting.ecg <- as.factor(htdis_data$resting.ecg)
htdis_datax$exercise.angina <- as.factor(htdis_data$exercise.angina)
htdis_datax$ST.slope <- as.factor(htdis_data$ST.slope)

# keep "target" as numerical and add a new column "case" with class "factor"
htdis_datax <- htdis_datax %>%
  mutate( case = as.factor(target))


# Function to plot all distributions per column
plot_distributions <- function(data) {
  
  # Initialize an empty list to store plots
  plot_list <- list()
  
  # Loop through all columns in the dataset
  for (col_name in names(data)) {
    # Select the column
    column <- data[[col_name]]
    
    # Check the column type
    if (is.numeric(column)) {
      # Numeric: Create a histogram
      plot <- ggplot(data, aes_string(x = col_name)) +
        geom_histogram(bins = 30, fill = "blue", color = "black") +
        theme_minimal() +
        ylim(0,250) +
        labs(title = paste("Distribution of", col_name), x = col_name, y = "Frequency") +
        theme(plot.title = element_text(size = 8))
    } else if (is.factor(column) || is.character(column)) {
      # Categorical: Create a bar chart
      plot <- ggplot(data, aes_string(x = col_name)) +
        geom_bar(fill = "green", color = "black") +
        theme_minimal() +
        ylim(0,1000) +
        labs(title = paste("Distribution of", col_name), x = col_name, y = "Count") +
        theme(axis.text.x = element_text(hjust = 1)) +
        theme(plot.title = element_text(size = 8))
    } else {
      # Skip unsupported types
      message(paste("Skipping column:", col_name, "- unsupported type"))
      next
    }
    
    # Add the plot to the list
    plot_list[[col_name]] <- plot
  }
  # Arrange the plots in a grid and display the results
  do.call(grid.arrange, c(plot_list, ncol = 3))

}

# Call the function on the dataset
plot_distributions(htdis_datax %>% select(-target))

# Removing row with wrong entry in ST.slope
htdis_datax <- htdis_datax %>%
  filter(ST.slope != "0")

# compute average, min, max, standard deviation and median for all numerical variables
# reduce the data set to the numerical data only
htdis_num <- htdis_datax %>%
  select(age, resting.bp.s,cholesterol, max.heart.rate, oldpeak)

cn <- colnames(htdis_num)

# calculate the different values for all columns
ht_avgs <- apply(htdis_num, 2, mean)
ht_max <- apply(htdis_num, 2, max)
ht_min <- apply(htdis_num, 2, min)
ht_median <- apply(htdis_num, 2, median)
ht_stdev <- apply(htdis_num, 2, sd)

#create a tibble to display the table efficiently
ht_info <- tibble(cn, ht_min, ht_max, ht_avgs, ht_stdev, ht_median)

# print the different values as a formatted table
knitr::kable(ht_info, col.names = c("Variable", "min", "max", "average", "sd", "median"), digits = 2, caption = "Continuous Variables: spread, average, standard deviation and median")

# count the zero values in the cholesterol variable
zero_chol <- htdis_datax %>%
  filter(cholesterol == 0) %>%
  count() %>%
  as.numeric()

# count zero values in cholesterol variable
chol_zeros <- htdis_datax %>%
  filter(cholesterol == 0) %>%
  summarize(n()) %>%
  as.numeric()

# calculate the average cholesterol value without zero values
avg_chol <- htdis_datax %>%
  filter(cholesterol > 0) %>%
  summarize(mean(cholesterol))

# calculate the median cholesterol value without zero values
med_chol <- htdis_datax %>%
  filter(cholesterol > 0) %>%
  summarize(median(cholesterol))

# convert to a numerical value
avg_chol <- as.numeric(avg_chol)
med_chol <- as.numeric(med_chol)

# put the data into a matrix that can be displayed using kable
# create matrix
xy <- matrix(0,2,3)
xy[1,1] <- "Zeros included"
xy[1,2] <- round(ht_avgs[3],1)
xy[1,3] <- round(ht_median[3],1)
xy[2,1] <- "Zeros removed"
xy[2,2] <- round(avg_chol,1)
xy[2,3] <- round(med_chol,1)

# print the table in an acceptable format
knitr::kable(xy, col.names = c("Data","Average mg/dl","Median mg/dl"), digits = 2, caption = "Cholesterol average and median")

# calculate the total number of cholesterol values above 450 mg/dL to evaluate a possible influence on the model
chol_top <- htdis_datax %>%
  filter(cholesterol > 450) %>%
  summarize(n())

# print the value without any formatting
chol_top <- as.numeric(chol_top)


# replacing zeros of cholesterol values with average as calculated above and store in a different data frame

htdis <- htdis_datax %>%
   mutate(cholesterol = ifelse(cholesterol == 0, med_chol, cholesterol))

# plot the new data distribution of cholesterol values after correction of zeros
htdis %>%
  ggplot(aes(cholesterol)) +
  geom_histogram(fill = "white", color = "blue") +
  ggtitle("Cholesterol value distribution after correction") +
  labs(x = "Cholesterol [mg/dl]", y = "Number of occurences")


# count zero values in resting.bp.s variable
rbps_zeros <- htdis_datax %>%
  filter(resting.bp.s == 0) %>%
  summarize(n()) %>%
  as.numeric()

# replacing the zero of resting.bp.s values with median as calculated above and store in a different data frame

htdis <- htdis %>%
   mutate(resting.bp.s = ifelse(resting.bp.s == 0, as.numeric(ht_median[2]), resting.bp.s))

# plot the new data distribution of resting.bp.s values after correction of zeros
htdis %>%
  ggplot(aes(resting.bp.s)) +
  geom_histogram(fill = "white", color = "blue") +
  ggtitle("resting.bp.s value distribution after correction") +
  labs(x = "resting.bp.s [mm Hg]", y = "Number of occurences")

#calculate the number of the ten top occurrences for blood pressure (variable resting.bp.s)
top_frq <- htdis %>%
  group_by(resting.bp.s) %>%
  summarize(frq = n()) %>%
  slice_max(order_by = frq, n = 10)

# print the table in an acceptable format
knitr::kable(top_frq, col.names = c("BP in mm Hg","Frequency"), digits = 0, caption = "Top ten blood pressure values")

# select all columns with numerical values and calculate their correlation
xx <- htdis %>%
  select(where(is.numeric)) %>%
  cor()

# plot the correlation matrix
ggcorr(xx, label = TRUE, hjust = 0.9, layout.exp = 1) +
  theme(text = element_text(size = 12), legend.title=element_text(size=10)) +
  ggtitle("Correlation between variables")

# calculate the number of males and females in the data set 
gdn <- htdis %>%
  group_by(sex) %>%
  count()

# store the total number of males and females in numerical variables for later use
Nf <- as.numeric(gdn[1,2]) 
Nm <- as.numeric(gdn[2,2])

# print the table in an acceptable format
knitr::kable(gdn, col.names = c("Gender","N"), digits = 2, caption = "Number of datsets for males/females")

# generate histogram with age distribution for males and females
# This plot shows the age distributions separately for a direct comparison
htdis %>%
  ggplot() +
  geom_histogram(aes(age), color = "red", fill = "white", filter(htdis, sex == "male")) +
  geom_histogram(aes(age), color = "blue", fill = "lightblue", filter(htdis, sex == "female")) +
  ggtitle("Age distribution of males (red) and females (blue)")

# generate a violin plot to judge differences in age distribution between males and females
ggplot(htdis, aes(x=sex, y=age, fill=sex)) + geom_violin(alpha = 0.5) +
  ggtitle("Age distribution of males and females (Violin Plot")


# calculate the proportional frequency distributions of males and females vs. age
# by use of a stratification of the age into groups of 5 years

htdis <- htdis %>%
  mutate(age_strat = factor(round(age/5)*5))

#Generate the data table with information about number an proportion of males/females
age_prop <- htdis %>%
  select(sex, age_strat) %>%
  group_by(age_strat, sex) %>%
  count() %>%
  pivot_wider(names_from = sex, values_from = n) %>%
  mutate(pM = round(100*male/Nm,1), pF = round(100*female/Nf,1))

# generate a graphical representation of the proportion of males and females of the age classes
age_prop %>%
  ggplot() +
  geom_col(aes(age_strat, pM), color = "red", fill = "white", alpha = 0.4) +
  geom_col(aes(age_strat, pF), color = "blue", fill = "white", alpha = 0.4) +
  ggtitle("Proportion of males and females vs. age") +
  labs(x = "Age (stratified)", y = "Proportion in %")

# print the table in an acceptable format
knitr::kable(age_prop, col.names = c("Age (class)","females", "males", "males %", "females %"), digits = 2, caption = "Age distribution of males/females")


# Generate a Histogram of age-related number of cases based on the stratified age classes
htdis %>%
  group_by(age_strat, case) %>%
  summarize(n()) %>%
  ggplot() +
  geom_col(aes(age_strat, `n()`, color = case, fill = case), alpha = 0.5) +
  ggtitle("Age distribution of heart disease")


# The table with stratified age classes is used 
# Count total number of cases 0 and 1 to calculate proportions
n_dis <- htdis %>%
  group_by(case) %>%
  count()
  
# Generate the data table with information about number an proportion of heart disease in total for each group (case)
dis_prop <- htdis %>%
  select(case, age_strat) %>%
  group_by(age_strat, case) %>%
  count() %>%
  pivot_wider(names_from = case, values_from = n) %>%
  mutate(pM = round(100*`0`/(`0`+`1`),1), pF = round(100*`1`/(`0`+`1`),1))
  #mutate(pM = round(100*`0`/as.numeric(n_dis[1,2]),1), pF = round(100*`1`/as.numeric(n_dis[2,2]),1))
  
cn <- c("age_strat" , "Disease", "Normal", "pDisease", "pNormal")
names(dis_prop) <- cn

# generate a graphical representation of the proportion of peple with disease/normal of the age classes
dis_prop %>%
  ggplot() +
  geom_col(aes(age_strat, pDisease), color = "red", fill = "white", alpha = 0.4) +
  geom_col(aes(age_strat, pNormal), color = "blue", fill = "white", alpha = 0.4) +
  ggtitle("Proportion with disease (red) and normal (blue) in each age class") +
  labs(x = "Age (stratified)", y = "Proportion in %")

# print the table in an acceptable format
knitr::kable(dis_prop, col.names = c("Age (class)","Disease", "Normal", "Disease %", "Normal %"), digits = 2, caption = "Age distribution of Disease")

# Generate plot to show the correlation of cholesterol vs. age, split by chest.pain.type
htdis %>%
  ggplot(aes(age, cholesterol, color = chest.pain.type)) +
  geom_point() +
  facet_wrap(~sex)

# Generate plot to show the correlation of cholesterol vs. age, split by sex and chest.pain.type
htdis %>%
  ggplot(aes(age, cholesterol, color = sex)) +
  geom_point() +
  facet_wrap(~chest.pain.type)

# Generate plot to show the correlation of max.heart.rate vs. age, split by sex and chest.pain.type
htdis %>%
  ggplot(aes(age, max.heart.rate, color = chest.pain.type)) +
  geom_point() +
  facet_wrap(~sex)


# Generate plot to show the correlation of max.heart.rate vs. age, split by sex and chest.pain.type
htdis %>%
  ggplot(aes(age, max.heart.rate, color = ST.slope)) +
  geom_point() +
  facet_wrap(~sex)


# Generate plot to show the correlation of max.heart.rate vs. age, split by sex and exercise.angina
htdis %>%
  ggplot(aes(age, max.heart.rate, color = exercise.angina)) +
  geom_point() +
  facet_wrap(~sex)


# Generate plot to show the correlation of max.heart.rate vs. age, split by sex and case
htdis %>%
  ggplot(aes(age, max.heart.rate, color = case)) +
  geom_point() +
  facet_wrap(~sex) +
  ggtitle("Max. heart rate vs. disease grouped by sex")


htdis %>%
  ggplot(aes(age, cholesterol, color = case)) +
  geom_point() +
  facet_wrap(~sex) +
  ggtitle("Cholesterol vs. disease for males & females")

# Generate a data frame to display the relative number of men and women  for exercise.angina and chest.pain.type
group1 <- htdis %>%
  group_by(sex, exercise.angina, chest.pain.type) %>%
  summarize(Number = n()) %>%
  mutate(Percent = ifelse(sex == "female", round(100*Number/Nf,1), round(100*Number/Nm,1)))

# check if the Sum equals 100 % (only used during model development)
# group1 %>%
# group_by(sex) %>%
#   summarize(sum(Percent))

# plot a graphic with percentage of men and Women for *exercise.angina and chest.pain.type*
# the size of the circles represents the percentage of occurrence in that group
group1 %>%
  ggplot(aes(exercise.angina, chest.pain.type, color = sex, size = Percent)) +
  geom_point(shape = 1) +
  ggtitle("Chest pain and exercise - Relative number of occurences") +
  labs(x = "Exercise induced angina", y = "Chest pain type")

# plot a similar graphic with the absolute number of persons
group1 %>%
  ggplot(aes(exercise.angina, chest.pain.type, color = sex, size = Number)) +
  geom_point(shape = 1) +
  ggtitle("Chest pain and exercise - Absolute number of occurences") +
  labs(x = "Exercise induced angina", y = "Chest pain type")

# set the start (seed) value for the generation of random samples to get reproducible and comparable results

set.seed(1643) # Birth year of Isaac Newton :-)

# remove the column "target" from the data set because the goal is to predict case
# target is numerical, but case a factor variable created from target
# the variable age is also removed, while age_strat is used instead
htdis <- htdis %>%
  select(-target, -age)

# splitting up the data into training and test sets using the caret library
test_idx <- createDataPartition(htdis$case, times = 1, p = 0.2, list = FALSE)
ht_test <- htdis[test_idx,]
ht_train <- htdis[-test_idx,]

# generate samples based on a 50% chance for "0" and "1" with appropriate length of the data sets
p1_case <- as.factor(sample(c("0","1"), size = dim(ht_train)[1], replace = TRUE))
p2_case <- as.factor(sample(c("0","1"), size = dim(ht_test)[1], replace = TRUE))

# Calculate the confusion matrices for both sets based on the predictions
result1 <- confusionMatrix(p1_case, ht_train$case)
result2 <- confusionMatrix(p2_case, ht_test$case)


# generate a results table as tibble to display accurately
all_acc <- tibble(Method = "Guessing", Accuracy = result2$overall[["Accuracy"]])

# display table with results from guessing
knitr::kable(all_acc, col.names = c("Method","Accuracy"), digits = 3, caption = "Accuracy of the model")


train_glm <- train(case ~ ., method = "glm", data = ht_train)
p_glm <- predict(train_glm, ht_test, type = "raw")

# Calculate the accuracy of the model with respect to the test set
acc_glm <- confusionMatrix(p_glm,ht_test$case)$overall[["Accuracy"]]

all_acc <- bind_rows(all_acc, tibble(Method="Linear Combination (glm)", Accuracy = acc_glm))

# display table with results from guessing
knitr::kable(tibble(Method="Linear Combination (glm)", Accuracy = acc_glm),
      col.names = c("Method","Accuracy"), digits = 3, caption = "Accuracy of the model")


# train a k-Nearest Neighbor model based on the caret library

train_knn <- train(case ~ ., method = "knn", data = ht_train, tuneGrid = data.frame(k = seq(3, 150, 2)))
p_knn <- predict(train_knn, ht_test, type = "raw")

# Calculate the accuracy of the model with respect to the test set
acc_knn <- confusionMatrix(p_knn,ht_test$case)$overall[["Accuracy"]]

# convert factor variables to numeric values for RMSE calculation
RMSE_knn <- RMSE(as.numeric(p_knn), as.numeric(ht_test$case))

ggplot(train_knn, highlight = TRUE) +
  labs(x = "Tuning parameter k = # of Neighbors", "Accuracy (with bootstrapping)")

all_acc <- bind_rows(all_acc, tibble(Method="k-Nearest neighbors", Accuracy = acc_knn ))

# display table with results from k-Nearest Neighbours
knitr::kable(tibble(Method="Linear Combination", Accuracy = acc_knn),
      col.names = c("Method","Accuracy"), digits = 3, caption = "Accuracy of the model")


# train a GAM Loess model based on the caret library

train_loess <- train(case ~ ., method = "gamLoess", data = ht_train)
p_loess <- predict(train_loess, ht_test, type = "raw")

# Calculate the accuracy of the model with respect to the test set
acc_loess <- confusionMatrix(p_loess,ht_test$case)$overall[["Accuracy"]]

# add entry to the complete table of results
all_acc <- bind_rows(all_acc, tibble(Method="GAM Loess", Accuracy = acc_loess))

# display table with results from GAM Loess
knitr::kable(tibble(Method="GAM Loess", Accuracy = acc_loess),
      col.names = c("Method","Accuracy"), digits = 3, caption = "Accuracy of the model")


# train a Random Forest model based on the caret library with cforest

train_rf <- train(case ~ ., method = "cforest", data = ht_train)
p_rf <- predict(train_rf, ht_test, type = "raw")

# Calculate the accuracy of the model with respect to the test set
acc_rf <- confusionMatrix(p_rf,ht_test$case)$overall[["Accuracy"]]

# Generate entry in the results table
all_acc <- bind_rows(all_acc, tibble(Method="Random Forest (cforest)", Accuracy = acc_rf))

# display table with results from Random Forest model
knitr::kable(tibble(Method="Random Forest (cforest)", Accuracy = acc_rf),
      col.names = c("Method","Accuracy"), digits = 3, caption = "Accuracy of the model")


# setting up the parameters to train the Random Forest model with optimized parameters and rf
x <- ht_train %>%
  select(-case)
y <- ht_train %>%
  select(case) %>%
  as.matrix()

# set tuning parameters
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1, 3, 5, 7, 10, 25, 50, 100))

train_rf2 <- train(x, y,
  method = "rf",
  ntree = 185,
  trControl = control,
  tuneGrid = grid,
  nSamp = 5000)

# plot the accurcy vs. tuning parameter mtry
ggplot(train_rf2, highlight = TRUE) +
  ggtitle("Optimal Tuning Parameter mtry for Random Forest")

# train_rf2$bestTune

yh_rf <- predict(train_rf2, ht_test, type = "raw")
acc_rf2 <- confusionMatrix(yh_rf, ht_test$case)$overall["Accuracy"]

all_acc <- bind_rows(all_acc, tibble(Method="Random Forest Optimized", Accuracy = acc_rf2))

# display table with results from Random Forest Optimized
knitr::kable(tibble(Method="Random Forest Optimized", Accuracy = acc_rf2),
      col.names = c("Method","Accuracy"), digits = 3, caption = "Accuracy of the model")


# sort the table with descending RMSE
all_acc <- all_acc %>% arrange(Accuracy)

# print the table of the accuracy of the models in an acceptable format
knitr::kable(all_acc, col.names = c("Method","Accuracy"), digits = 3, caption = "Comparison of the models")


