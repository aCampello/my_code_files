#!/usr/bin/python

""" 
    This is the code to accompany the Lesson 3 (decision tree) mini-project.

    Use a Decision Tree to identify emails from the Enron corpus by author:    
    Sara has label 0
    Chris has label 1
"""

###############################################################################

# Setting up
    
import sys
from time import time
cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\tools
from email_preprocess import preprocess

### features_train and features_test are the features for the training
### and testing datasets, respectively
### labels_train and labels_test are the corresponding item labels
features_train, features_test, labels_train, labels_test = preprocess()

###############################################################################

""" Part 1: Get the Decision Tree Running
    Get the decision tree up and running as a classifier, setting 
    min_samples_split=40.  It will probably take a while to train.  
    What’s the accuracy?
"""

# create/train
from sklearn import tree
clf = tree.DecisionTreeClassifier(min_samples_split=40)
clf.fit(features_train, labels_train)

# preds
pred = clf.predict(features_test)

# accuracy
from sklearn.metrics import accuracy_score 
accuracy_score(pred, labels_test)  # = 0.977


##############################################################################

""" 
    Part 2: Speed It Up
    You found in the SVM mini-project that the parameter tune can significantly
    speed up the training time of a machine learning algorithm.  A general rule
    is that the parameters can tune the complexity of the algorithm, with more 
    complex algorithms generally running more slowly. 
    
    Another way to control the complexity of an algorithm is via the number of 
    features that you use in training/testing.  The more features the algorithm
    has available, the more potential there is for a complex fit.  We will 
    explore this in detail in the “Feature Selection” lesson, but you’ll get a 
    sneak preview now.

    Find the number of features in your data.  The data is organized into a 
    numpy array where the number of rows is the number of data points and the 
    number of columns is the number of features; so to extract this number, use
    a line of code like len(features_train[0])
    
    Numpy arrays = multidimensional, so 2D matrices (or stacks)
        (standard Python arrays = one-dimensional, i.e. lists)
"""

# number of features in data (=no. columns)
features_train.shape    # 15820 rows, 3785 columns.
len(features_train[0])  # length of the first row = 3785 columns


###############################################################################

"""
    In tools/email_preprocess.py, find the line of code that looks like this:
        selector = SelectPercentile(f_classif, percentile=1)  
    Change percentile from 10 to 1. How many features are there now?
    NOTE: may need to restart the console to overwrite the previous preprocess 
    function saved in the memory.
"""

cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\tools
from email_preprocess import preprocess
features_train, features_test, labels_train, labels_test = preprocess()
len(features_train[0])    # = 379, so reducing the percentile reduced the 
                          # number of features available by 10%.
                         
###############################################################################

""" 
    What do you think SelectPercentile is doing?  
    Would a large value for percentile lead to a more complex or less complex
    decision tree, all other things being equal?
"""
# It is selecting a given % of available features, so percentile=1 selects 1% 
# of all available features.
# Higher percentiles mean the training/test datasets contain more features
# and will produce more complex decision trees.

###############################################################################

"""
What's the accuracy of your decision tree when you use only 1% of your 
available features (i.e. percentile=1)?
"""

# create/train
from sklearn import tree
clf = tree.DecisionTreeClassifier(min_samples_split=40)
clf.fit(features_train, labels_train)

# preds
pred = clf.predict(features_test)

# accuracy
from sklearn.metrics import accuracy_score 
accuracy_score(pred, labels_test)  # = 0.966

###############################################################################