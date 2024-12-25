# heartdisease

## Background Information
In the United States, more than 800,000 people die from cardiovascular disease each year, which is about 1 in every 3 deaths. The dataset we intend to analyze is the Heart Failure Prediction Dataset from Kaggle. It contains 918 observations gathered from various locations with 12 variables about the patient’s information and risk factors associated with cardiovascular disease. The 12 variables are as listed: Age, Sex, ChestPainType, RestingBP, Cholesterol, FastingBS, RestingECG, MaxHR, ExerciseAngina, Oldpeak, ST_Slope, HeartDisease. 

Age is an integer data type while Sex is a character data type indicated by either “M” or “F” for Male or Female respectively. ChestPainType is a character data type that tells us the types of chest pain experienced and this is categorized by the abbreviations and name listed: “TA” (Typical Angina), “ATA” (Atypical Angina), “NAP” (Non-Anginal Pain), “ASY” (Asymptomatic). RestingBP is an integer data type that expresses the resting blood pressure of a patient in mm Hg. Cholesterol is an integer data type that shows the patient’s serum cholesterol in mm/dl. FastingBS, the fasting blood sugar, is an integer data type expressed by either 1 or 0 where 1 is FastingBS > 120 mg/dl, otherwise 0. RestingECG, the resting electrocardiogram results, is a character data type indicated by “Normal”, “ST” (Having ST-T wave abnormality), and “LVH” (Showing probable or definite left ventricular hypertrophy by Estes' criteria). MaxHR, the maximum heart rate, is an integer datatype with a numeric value between 60 and 202. ExerciseAngina, exercise-induced angina, is a logical data type that shows if the patient has exercise-induced angina or not through the use of Y(Yes) or N (No). Oldpeak, ST depression caused by activity in comparison to rest, is a numeric data type that is measured in depression while ST_Slope, the slope of the peak exercise ST segment, is a character data type expressed with “Up” (Upsloping), “Flat” (Flat), and “Down” (Downsloping) in relations to our findings in Oldpeak. HeartDisease, which tells us whether the recorded patient had heart disease, is a logical data type where 1 is they have heart disease and 0 is they don’t. 

Heart Disease is the target variable as this predicts whether a patient will have heart disease or not. Based on factors associated with cardiovascular diseases, we have a prediction model that predicts who would have heart disease. We want to see what groups of people need to be paid close attention to so we can spot signs of cardiovascular risk and manage their levels while they still can. This will allow us to answer what groups would be more susceptible to heart attacks/diseases. Other questions addressed in the project will be what factors are most significant in causing heart disease and how accurate our model is. Depending on our findings, we would want those in the group susceptible to heart attacks to be our target audience. Anyone else who thinks they may have cardiovascular issues may also be our target. Healthcare physicians can use this to vet patients who are likely to develop heart disease before more serious symptoms occur.

## Dataset
https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction

## Exploratory Data Analysis
To explore our data, we graphed all the variables related to our target variable, HeartDisease. We also created a pairwise scatterplot, and correlation matrix heat map, and calculated the variance inflation factor for each variable to determine whether there is collinearity between the variables and to visualize the relationships better. 

An initial look at boxplots and bar graphs provided us insight into interesting relationships between each variable and heart disease. For age, the median and upper 75% of data are within a higher age range for those with heart disease. For resting blood pressure the interquartile range and overall range are larger but around a similar median for those with heart disease. Serum cholesterol was surprising because the lower two quartiles range from 0 mm/dl to 250 mm/dl for patients with heart disease whereas the upper two quartiles range from 250 to 600. For patients without heart disease, serum cholesterol ranges from 100 to 400 with the middle 50 percent ranging from 190 to 260 mm/dl. Healthy patients have much more consistent cholesterol levels compared to those with heart disease. In terms of maximum heart rate, the interquartile ranges of both heart disease and no heart disease are the same. However, the values for no heart disease are consistently higher as the graph for no heart disease has a clear upward shift of about 20 bpm. Oldpeak refers to a depression in the ECG plot in response to exercise and is represented as a slope value. The old peak for heart disease patients has a much larger range, from -2.2 to 5, and a median of around 1.1. For healthy patients, it ranges from -1.8 to 1.5 and the median is around 0. Just by looking at boxplots, we can see that old peaks, serum cholesterol levels, and maximum heart rates all display differences between healthy and heart disease patients. For all variables on box plots, heart disease patients show higher ranges and higher or lower values depending on their impact on heart disease. 

Bar graphs also yield interesting insights. Based on the sex of the patient, it can be seen that male patients tend to be more prone to heart failure, especially when heart failure is predicted. Chest Pain Types are a key indicator that the heart is having some sort of issue since your chest is experiencing pain in some way. When the patient isn’t predicted to have heart failure, 3 of the chest pain types are in the 100 range with only TA being in the 20 to 30 range. When it is predicted that heart failure is shown, ASY is over 300 while the other 3 dropped significantly with ATA and TA being around 10 to 30 and NAP being in the 40 to 50 range. This change can indicate this variable's significance as an abnormal change occurred. Moving onto the Resting ECG graph, Normal ECG seems to be high for both having and not having heart failure which is normal since having this level of ECG is considered healthy. The other 2 indicators, LVH and ST show a slight increase in frequency as it goes from the mid-50 range when it is predicted not to have heart failure to the low 100 range when it is predicted heart failure is prevalent.  In the Exercise Angina graph we see that when it is predicted that the patient doesn’t have heart failure, it is more likely that they don’t have exercise-induced angina. When it is predicted that they have heart failure, they are more likely to have exercise-induced angina but, likely, someone who doesn’t have it can also have heart failure. In fasting blood sugar (FBS) levels (>120 mg/dl), we see an increase in people who show signs of having  FBS of  >120 mg/dl when heart disease is predicted as opposed to not. In the ST Slope, when heart disease isn’t predicted we see an exponential curve from down, flat, to up. When it is predicted, however, flat increases a lot while down drops by about half of what we see when heart disease isn’t predicted.

We also determined that none of the variables show collinearity. Based on our findings and better understanding of variables from the graphs, we better understand how our variables interact. 

## Regression Analysis
To further explore the variables that have the highest impact when predicting heart failure and find the best predictive model, we used logistic regression, random forest, and KNN. Using 0.001 as the significant code for logistic regression, we can see that the variables Sex (Males), all Chest Pain Types (ATA, NAP, TA), Cholesterol, Fasting Blood Sugar, ExerciseAngina (Yes), ST Slope (Flat) are significant when it comes to determining heart disease. Males have a higher chance of experiencing heart failure by a factor of 4.3339419. Based on the confusion matrix and the mean of the accuracy, logistic regression has a high mean accuracy of  0.8612946 which tells us that our initial dataset did a pretty good job predicting heart disease. We then created a random forest model using an 80-20 holdout to train and test the data. Using the MSE, we found that the testing error is 0.1358695 meaning that the accuracy is 0.8641305. This is lower than logistic regression as random forests are often more complex and decrease classification accuracy when there are redundant variables, which we can see in our dataset with various factors within many of our variables. For KNN, we discovered that 5NN is the best as the highest mean accuracy obtained was 0.7027301. This tells us that out of the 3 models, KNN has the worst accuracy. This makes sense since it is a lazy algorithm that classifies based on Euclidean distance. 

## Conclusion
Based on our findings, random forest had the best model so we would use random forest to model heart failure. While random forest is the best model, when it comes to predicting what variables had the most impact on heart disease, logistic regression was better at doing so. We decided to use this as logistic regression as random forests had extremely similar accuracies. An example of this is when we change all 3 model’s seeds from 1 to 2, logistic becomes the best model by approximately 1%. To answer our question on which variable is the most significant when it comes to determining heart failure we look back on what variables we found to be significant and its associated odds ratio. Based on this we found that the Chest Pain Types decreased the odds of having a heart failure which is interesting as chest pain and heart failure aren’t that different. What might’ve created this result was the fact that there were variables that were more relevant in increasing heart disease. For example, being a male increased heart failure by approximately 4 times. Cholesterol had a factor of 0.99 which means that heart failure won’t be impacted by cholesterol that much. Fasting Blood Sugar had a factor of 3.1157887, Exercise Angina (Yes) had a factor of 2.4603214, and  ST Slope (Flat) had a factor of 4.2797800. These 3 variables increased the chance of heart failure to a large degree. Excluding Sex (Male) as it is a variable a person can’t change,  ST Slope (Flat) has the highest odds ratio that increases the chance of heart failure. ST Slope (Flat) is the factor that people more susceptible to heart attacks/diseases, especially males, should look into changing over time to become ST Slope (Up) as this decreases the chance of heart failure based on the odds ratio of 0.37. 

