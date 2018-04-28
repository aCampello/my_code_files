#!/usr/bin/python

""" Udacity intro to machine learning
    Lesson 8 quiz 10: outlier removal 
"""

##############################################################################

### Prepare the workspace

import random
import numpy
import matplotlib.pyplot as plt
import pickle

cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\outliers
from outlier_cleaner import outlierCleaner

##############################################################################

### load up some practice data with outliers in it
ages = pickle.load( open("practice_outliers_ages.pkl", "r") )
net_worths = pickle.load( open("practice_outliers_net_worths.pkl", "r") )

# Prepare the data

### ages and net_worths are lists so need to be reshaped into 2D numpy arrays
### second argument of reshape command is a tuple of integers: (n_rows, n_columns)
### by convention, n_rows is the number of data points
### and n_columns is the number of features
ages       = numpy.reshape( numpy.array(ages), (len(ages), 1))
net_worths = numpy.reshape( numpy.array(net_worths), (len(net_worths), 1))

# Split the data into training and testing sets
from sklearn.cross_validation import train_test_split  # depreciation warning for "sklearn.cross_validation"
from sklearn.model_selection import train_test_split  # sklearn.model_selection has replaced sklearn.cross_validation

ages_train, ages_test, net_worths_train, net_worths_test = train_test_split(ages, net_worths, test_size=0.1, random_state=42)

##############################################################################

### fill in a regression here!  Name the regression object reg so that
### the plotting code below works, and you can see what your regression looks like

from sklearn.linear_model import LinearRegression

reg = LinearRegression()
reg.fit(ages_train, net_worths_train)

##############################################################################

""" Plot the regression """

try:
    plt.plot(ages, reg.predict(ages), color="blue")
except NameError:
    pass
plt.scatter(ages, net_worths)
plt.show()

""" There are some outliers below the regression line. """

##############################################################################

""" What is the slope?"""
reg.coef_  # slope =  5.077 (should be 6.25, which Udacity used to generate the data)

##############################################################################

""" What is the score (r-sq) you get when using your regression to make 
    predictions with the test data?"""
    
print "TEST SET r-sq:", reg.score(ages_test, net_worths_test)   # r-sq = 0.878

##############################################################################

""" Write a function outlierCleaner in file outlier_cleaner.py to remove the
    10% of datapoints with the highest residual error.  
    
    Then deploy a linear regression where net worth is the target and the feature 
    being used to predict it is a person’s age (remember to train on the 
    training data!)."""

# fit the regression (I did it already above so this is just a reminder)
from sklearn.linear_model import LinearRegression
reg = LinearRegression()
reg.fit(ages_train, net_worths_train)


# import the new function    
cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\outliers
from outlier_cleaner import outlierCleaner
""" NOTE might need to restart the kernel (and reload in all the data/modules) 
# for changes to outlier_cleaner to be recognised."""


# use my new outlierCleaner function to identify and remove the most outlier-y
# points from the training data
cleaned_data = []
try:
    predictions = reg.predict(ages_train)
    cleaned_data = outlierCleaner( predictions, ages_train, net_worths_train )
except NameError:
    print "your regression object doesn't exist, or isn't name reg"
    print "can't make predictions to use in identifying outliers"


### Udacity code to re-train the regression using the cleaned data

# Udacity note: only run this code if cleaned_data is returning data
if len(cleaned_data) > 0:
    # convert the list of tuples in cleaned_data back into usable numpy arrays
    ages, net_worths, errors = zip(*cleaned_data)
    ages       = numpy.reshape( numpy.array(ages), (len(ages), 1))
    net_worths = numpy.reshape( numpy.array(net_worths), (len(net_worths), 1))
    # refit the cleaned data:
    try:
        reg.fit(ages, net_worths)
        plt.plot(ages, reg.predict(ages), color="blue")
    except NameError:
        print "you don't seem to have regression imported/created,"
        print "   or else your regression object isn't named reg"
        print "   either way, only draw the scatter plot of the cleaned data"
    # plot
    plt.scatter(ages, net_worths)
    plt.xlabel("ages")
    plt.ylabel("net worths")
    plt.show()
else:
    print "outlierCleaner() is returning an empty list, no refitting to be done"


# Get the new slope and r-squared
reg.coef_                               # = 6.369 (much closer to the true slope of 6.25)
reg.score(ages_test, net_worths_test)   # r-sq = 0.983 (almost perfect!)
