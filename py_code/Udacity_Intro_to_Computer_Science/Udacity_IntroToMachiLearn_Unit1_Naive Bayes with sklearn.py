# -*- coding: utf-8 -*-
"""
Created on Fri Jul 14 10:11:07 2017

Udacity Intro to Machine Learning: Unit 1
"""
### Module sklearn
# Python library for machine learning
# Creates decision surfaces based on supervised classification:
# When you feed in several datapoints that are known examples and the computer uses
#  these to form a decision surface (boundary between two extremes e.g. fast and slow)
#  which is used to predict the classification of each new datapoint based on its value
#  (visualised as its position on a scatter plot).

# sklearn has different sub-modules to run different types of algorithms.

# 1) Import the Naive Bayes module (this creates a decision surface)
from sklearn.naive_bayes import GaussianNB

# 2) Create the classifier, here named 'clf'
clf = GaussianNB()

# 3) Train the classifier by feeding in some known examples (this command is always run for supervised classification)
clf.fit(X, Y)
# This command calls the fit function on the classifier clf, where X = the features
# and Y = the labels/classes (e.g. features when measuring terrain might be bumpiness
# and slope, and the labels/classes would be whether to drive 'fast' or 'slow'.)
# This is when the classifier learns the patterns.

# 4) Then use the classifier to predict the label/class of a new point
# print a single prediction
print(clf.predict([[-0.8, -1]]))    # The features (X) are -0.8 and -1.
                                    # This will return the predicted label/classification
                                    # of the point - whether it is 'fast' or 'slow' based on the training.
# print / save a vector of predictions
pred = clf.predict(features_test)   # where features_test is a vector of features to classify.


### Create a function 'classify' to do the training
def classify(features_train, labels_train):
    ### import the sklearn module for GaussianNB
    from sklearn.naive_bayes import GaussianNB
    ### create classifier
    clf = GaussianNB()
    ### fit the classifier on the training features and labels)
    clf.fit(features_train, labels_train)
    ### return the fit classifier
    return clf

# Then use the classify function to create a trained classifier in one line.
clf = classify(features_train, labels_train)

# make predictions
pred = clf.predict(features_test)

# measure the accuracy of those predictions
# accuracy is defined as the number of test points that are classified correctly divided by the total number of test points.
from sklearn.metrics import accuracy_score
accuracy = accuracy_score(pred, labels_test) # pred = vector of preds from clf.predict(); labels_test = vector of true/known labels

# alternative way to find the accuracy:
accuracy = clf.score(features_test, labels_test)