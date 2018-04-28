# -*- coding: utf-8 -*-
"""
Created on Tue Jul 25 17:14:26 2017

Udacity Intro to Machine Learning: Lesson 4 Decision Trees

User guide: http://scikit-learn.org/stable/modules/tree.html

For end-of-unit quizzes see file dt_author_id.py in:
C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\decision_tree

"""

##### Get data
import sys
from time import time
cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\tools
from email_preprocess import preprocess

features_train, features_test, labels_train, labels_test = preprocess()


##### Standard code for Decision Tree Classifiers

# create/train
from sklearn import tree
clf = tree.DecisionTreeClassifier()
clf.fit(features_train, labels_train)

# preds
pred = clf.predict(features_test)

# accuracy
from sklearn.metrics import accuracy_score 
accuracy_score(pred, labels_test)  


##############################################################################

##### Decision Tree Parameters
# classifiers can be optimised by tuning the parameters

# list of parameters available when creating the DecisionTreeClassifier
# http://scikit-learn.org/stable/modules/generated/sklearn.tree.DecisionTreeClassifier.html#sklearn.tree.DecisionTreeClassifier

# Testing the min_samples_split parameter. Default=2, so groups can continue 
# to be split until they contain just 1 sample.

""" Create 2 decision tree classifiers, one with min_samples_split=2 and 
    one with min_samples_split=50. Compute the accuracies on the testing 
    data and store the accuracy numbers to acc_min_samples_split_2 and
    acc_min_samples_split_50 """

# function to create and train
def classify(features, labels, samples):
    from sklearn import tree
    clf = tree.DecisionTreeClassifier(min_samples_split = samples)
    clf = clf.fit(features, labels)
    return clf

# create/train with min_samples_split=2
clf2 = classify(features_train, labels_train, 2)
pred2 = clf2.predict(features_test)
from sklearn.metrics import accuracy_score 
acc_min_samples_split_2 = accuracy_score(pred2, labels_test)  
print acc_min_samples_split_2 # 0.991

# create/train with min_samples_split=50
clf50 = classify(features_train, labels_train, 50)
pred50 = clf50.predict(features_test)
acc_min_samples_split_50 = accuracy_score(pred50, labels_test)  
print acc_min_samples_split_50 # 0.971


##############################################################################

##### """ Splitting criterion """

# This is the function to measure the quality of a split.
# For decision trees in scikit-learn, the default splitting criterion is "gini"
# for Gini impurity. This will probably work fine in most cases, but is
# slightly different from entropy (short note in Udacity lesson 4 video 32). 
# The splitting criterion is tunable and can be changed to "entropy".



##### Entropy (ranges from 0 to 1.0)

# 0 = 0% impure, so all examples are in the same class
# 1 = 100% impure, so examples are evenly split between all the available classes.

# Entropy formula: SUM[-Pi * log2(Pi)]
# Pi = fraction of examples in class i 

# Say a parent node contains 4 examples, labelled: [slow, slow, fast, fast].
# The proportion of examples in the slow class (Pslow) = 2/4 = 0.5
# The proportion of examples in the fast class (Pfast) = 2/4 = 0.5. 

# To calculate entropy:
import math
-0.5*math.log(0.5, 2) + -0.5*math.log(0.5, 2)   # log(0.5, 2) means calc log of 0.5 to base 2
# output = 1.0 (the entropy for this parent node)



##### Information Gain (ranges from 0 to 1.0)

# 0 = zero info gain
# 1 = maximum info gain

# Decision trees split data using variables/values that maximise the info gain.

# Information gain = entropy(parent) - [weighted average]entropy(children)

# If we split the 4-example parent node above into two child nodes, 
# the examples in these child nodes are the 'children'.
# There are several ways to split the parent node [slow, slow, fast, fast]:
# based on bumpiness (bumpy/smooth), gradient (flat/steep) or speed limit (yes/no).


# Calculating info gain for different splits:

""" EXAMPLE 1: splitting on bumpiness (udacity lesson 4 video 30)"""
# Child node 1: contains 2 of the original 4 and Pslow=1/2=0.5; Pfast=1/2=0.5
# entropy = 1.0    (-0.5*math.log(0.5, 2) + -0.5*math.log(0.5, 2))

# Chid node 2: contains 2 of the original 4 and Pslow=1/2=0.5; Pfast=1/2=0.5
# entropy = 1.0    (-0.5*math.log(0.5, 2) + -0.5*math.log(0.5, 2))

# Weighted average of entropy(children) = 1/2(1) + 1/2(1)
(0.5*1) + (0.5*1) # = 1.0

# parent entropy = 1.0    # (calculated this in the ### Entropy section above)

# Info gain = 1-1 = 0. 
##   We don't learn anything from splitting in this way.


""" EXAMPLE 2: splitting on gradient (udacity lesson 4 videos 22-29) """
# Child node 1: contains 3 (out of the original 4) examples and 
# Pslow = 2/3 = 0.666; Pfast = 1/3 = 0.333
# entropy=
-0.666*math.log(0.666, 2) + -0.333*math.log(0.333, 2)    # = 0.919

# Child node 2: contains 1 (out of 4) examples and this example is fast, so
# Pfast = 1/1 = 1  (there is no Pslow)
# entropy=
-1*math.log(1, 2)   # = 0.0

# Weighted average of entropy(children) = 3/4(0.919) + 1/4(0.0)
(0.75*0.919) + (0.25*0)    # = 0.689

# parent entropy = 1.0    # (calculated this in the ### Entropy section above)

# Info gain = 1-0.689 = 0.311. 
##   We learn more by splitting this way.


""" EXAMPLE 3: splitting on speed limit (udacity lesson 4 video 31) """
# Child node 1: contains 2 (out of the original 4) examples and 
# Pslow = 2/2 = 1.0
# entropy=
-1*math.log(1, 2)   # = 0.0

# Child node 2: contains 2 (out of the original 4) examples and 
# Pfast = 2/2 = 1.0
# entropy=
-1*math.log(1, 2)   # = 0.0

# Weighted average of entropy(children) = 3/4(0.919) + 1/4(0.0)
(0.5*0.0) + (0.5*0.0)    # = 0.0

# parent entropy = 1.0    # (calculated this in the ### Entropy section above)

# Info gain = 1.0-0.0 = 1.0. 
##   This is the highest possible info gain so a good place to make split.

##############################################################################









