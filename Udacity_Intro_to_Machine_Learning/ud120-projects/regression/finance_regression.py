#!/usr/bin/python

"""
    Starter code for the regression mini-project.
    
    Loads up/formats a modified version of the dataset
    (why modified?  we've removed some trouble points
    that you'll find yourself in the outliers mini-project).

    Draws a little scatterplot of the training/testing data

    You fill in the regression code where indicated:
"""    

### prepare the data using helper functions from other py files in ud120-projects
import sys
import pickle
sys.path.append("./tools/")
from feature_format import featureFormat, targetFeatureSplit
dictionary = pickle.load( open("./final_project/final_project_dataset_modified.pkl", "r") )

### list the features you want to look at--first item in the 
### list will be the "target" feature
features_list = ["bonus", "salary"]
data = featureFormat( dictionary, features_list, remove_any_zeroes=True)
target, features = targetFeatureSplit( data )

### training-testing split needed in regression, just like classification
from sklearn.cross_validation import train_test_split
feature_train, feature_test, target_train, target_test = train_test_split(features, target, test_size=0.5, random_state=42)
train_color = "b"
test_color = "r"

##############################################################################

""" Import LinearRegression from sklearn, and create/fit your regression. 
    Name it reg so that the plotting code will show it overlaid on the 
    scatterplot. Does it fall approximately where you expected it?

    Extract the slope (stored in the reg.coef_ attribute) and the intercept. 
    What are the slope and intercept?
"""
### Please name it reg, so that the plotting code below picks it up and 
### plots it correctly. Don't forget to change the test_color above from "b" to
### "r" to differentiate training points from test points.
from sklearn.linear_model import LinearRegression
reg = LinearRegression()
reg.fit(feature_train, target_train)

# Evaluate
print "slope:", reg.coef_               # slope = 5.45
print "intercept:", reg.intercept_      # intercept = -102360.543

##############################################################################

""" Imagine you were a less savvy machine learner, and didn’t know to test on
    a holdout test set. Instead, you tested on the same data that you used to 
    train, by comparing the regression predictions to the target values (i.e. 
    bonuses) in the training data. What score do you find? You may not have an
    intuition yet for what a “good” score is; this score isn’t very good (but
    it could be a lot worse).
"""

# r-squared of training data
reg.score(feature_train, target_train)  # = 0.046 (correct answer and they say it can be lower!)

##############################################################################

""" Now compute the score for your regression on the test data, like you know 
    you should. What’s that score on the testing data? If you made the mistake 
    of only assessing on the training data, would you overestimate or 
    underestimate the performance of your regression?
"""
# r-squared of test data
reg.score(feature_test, target_test)  # -1.48

##############################################################################

""" Perform the regression of bonus against long term incentive--what’s the
    score on the test data?
"""
### list the features you want to look at--first item in the 
### list will be the "target" feature

dictionary.values()    # view list of features in the dataset

# extract just the bonus and long term incentive
features_list = ["bonus", "long_term_incentive"]
data = featureFormat( dictionary, features_list, remove_any_zeroes=True)
target, features = targetFeatureSplit( data )

# split the data into training and test sets
from sklearn.cross_validation import train_test_split
feature_train, feature_test, target_train, target_test = train_test_split(features, target, test_size=0.5, random_state=42)
# set colours for plotting
train_color = "b"
test_color = "r"

# regression
reg = LinearRegression()
reg.fit(feature_train, target_train)
reg.score(feature_test, target_test) # = -0.593  (correct: long_term_incentive was better than salary at predicting bonuses)

##############################################################################

""" Go back to a setup where you are using the salary to predict the bonus, 
    and rerun the code to remind yourself of what the data look like.
"""
# prepare the data
features_list = ["bonus", "salary"]
data = featureFormat( dictionary, features_list, remove_any_zeroes=True)
target, features = targetFeatureSplit( data )

# split into training & testing sets
from sklearn.cross_validation import train_test_split
feature_train, feature_test, target_train, target_test = train_test_split(features, target, test_size=0.5, random_state=42)
train_color = "b"
test_color = "r"

# run the regression
from sklearn.linear_model import LinearRegression
reg = LinearRegression()
reg.fit(feature_train, target_train)
reg.score(feature_test, target_test)    # -1.485

##############################################################################

### PLOTTING
# select all code down to plt.show() & run it all at once.

# draw the scatterplot, with color-coded training and testing points
import matplotlib.pyplot as plt
for feature, target in zip(feature_test, target_test):
    plt.scatter( feature, target, color=test_color ) 
for feature, target in zip(feature_train, target_train):
    plt.scatter( feature, target, color=train_color ) 

# labels for the legend
plt.scatter(feature_test[0], target_test[0], color=test_color, label="test")
plt.scatter(feature_test[0], target_test[0], color=train_color, label="train")

# draw the regression line, fit to the test data (which includes an 
# outlier with a large salary & small bonus)
try:
    plt.plot(feature_test, reg.predict(feature_test))  
except NameError:
    pass
plt.xlabel(features_list[1])
plt.ylabel(features_list[0])
plt.legend()
plt.show()

""""""

# Plot again but with 2 regression lines, one for the test set and one for
# the training set (without the high salary/low bonus outlier).

# draw the scatterplot, with color-coded training and testing points
import matplotlib.pyplot as plt
for feature, target in zip(feature_test, target_test):
    plt.scatter( feature, target, color=test_color ) 
for feature, target in zip(feature_train, target_train):
    plt.scatter( feature, target, color=train_color ) 

# labels for the legend
plt.scatter(feature_test[0], target_test[0], color=test_color, label="test")
plt.scatter(feature_test[0], target_test[0], color=train_color, label="train")


# draw the regression line, once it's coded
try:
    plt.plot( feature_test, reg.predict(feature_test) )    # fits regression line to test data (which includes an outlier with large salary & small bonus)
except NameError:
    pass
# the 2 lines of code below draw the 2nd regression line:
reg.fit(feature_test, target_test)
plt.plot(feature_train, reg.predict(feature_train), color="k") # k=black
#
plt.xlabel(features_list[1])
plt.ylabel(features_list[0])
plt.legend()
plt.show()

# slope of the new reg line = 
reg.coef_   # new slope without the outlier = 2.274 
            # the original slope was 5.4
            # removing the outlier made a big difference!
            

##############################################################################