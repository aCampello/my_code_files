#!/usr/bin/python

""" 
    This is the code to accompany the Lesson 2 (SVM) mini-project.

    Use a SVM to identify emails from the Enron corpus by their authors:    
    Sara has label 0
    Chris has label 1
"""
    
import sys
from time import time
cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\tools
from email_preprocess import preprocess


### features_train and features_test are the features for the training
### and testing datasets, respectively
### labels_train and labels_test are the corresponding item labels
features_train, features_test, labels_train, labels_test = preprocess()

##############################################################################

""" TASK: Import, create, train and make predictions with the sklearn SVC 
    classifier. When creating the classifier, use a linear kernel.
    What is the accuracy? """

from sklearn.svm import SVC    # Import SVM function

# Create and train/fit an linear SVM classifier
### can condense this stage from 2 lines of code into one:
# two lines:
linear_clf = SVC(kernel = "linear")
linear_clf.fit(features_train, labels_train)
# = one line:
linear_clf = SVC(kernel="linear").fit(features_train, labels_train)

# Make predictions (you pass in the test features and predict the labels)
pred = linear_clf.predict(features_test)

# Calculate the accuracy of predictions (Naive Bayes was 88.4%)
from sklearn.metrics import accuracy_score    # import the accuracy function
acc = accuracy_score(pred, labels_test)
print acc    # output = 0.984, so SVM is more accurate than Naive Bayes.

##############################################################################

 """ Place timing code around the fit and predict functions, like you did in
 the Naive Bayes mini-project. How do the training and prediction times 
 compare to Naive Bayes? """

# Create a new linear classifier
linear_clf = SVC(kernel = "linear")
 
### Measure the time taken to train it
t0 = time()
linear_clf.fit(features_train, labels_train)
print "training time:", round(time()-t0, 3), "s"   # answer = 201.835 seconds

### Measure the time taken to make predictions
t0 = time()
pred = linear_clf.predict(features_test)
print "prediction time:", round(time()-t0, 3), "s"   # answer = 21.355 seconds

# SVM is MUCH slower for training and predicting compared to Naive Bayes. 

##############################################################################

""" One way to speed up an algorithm is to train it on a smaller training 
    dataset. The tradeoff is that the accuracy almost always goes down when 
    you do this. Let’s explore this more concretely: add in the following 
    two lines immediately before training your classifier. 
    These lines effectively slice the training dataset down to 1% of its 
    original size, tossing out 99% of the training data. You can leave all 
    other code unchanged. What’s the accuracy now? """

# Take a subset of 1% of the training dataset
features_train = features_train[:len(features_train)/100] 
labels_train = labels_train[:len(labels_train)/100] 

# Run SVM on reduced dataset for speed, and then check the accuracy.
linear_clf = SVC(kernel="linear").fit(features_train, labels_train)
pred = linear_clf.predict(features_test)
from sklearn.metrics import accuracy_score
acc = accuracy_score(pred, labels_test)
print acc    # output = 0.885: v.good for such a reduction in the data!

##############################################################################

""" Keep the training set slice code from the last quiz, so that you are still
 training on only 1% of the full training set. Change the kernel of your SVM 
 to “rbf”. What’s the accuracy now, with this MORE COMPLEX kernel? """

rbf_clf = SVC(kernel="rbf").fit(features_train, labels_train)
pred = rbf_clf.predict(features_test)
from sklearn.metrics import accuracy_score
acc = accuracy_score(pred, labels_test)
print acc    # output = 0.616 = NOT GREAT.

##############################################################################

""" Keep the training set size and rbf kernel from the last quiz, but try 
several values of C (say, 10.0, 100., 1000., and 10000.). Which one gives the 
best accuracy? """

# Default value of C = 1.0
# Compare rbf kernels with different C (smoothness vs misclassification)
#   by fitting SVM algorithms to the reduced (1%) dataset:

# C = 10
rbf_clf_C10 = SVC(kernel="rbf", C=10.0).fit(features_train, labels_train)
pred = rbf_clf_C10.predict(features_test)
acc = accuracy_score(pred, labels_test)
print acc    # output = 0.616

# C = 100
rbf_clf_C100 = SVC(kernel="rbf", C=100.0).fit(features_train, labels_train)
pred = rbf_clf_C100.predict(features_test)
acc = accuracy_score(pred, labels_test)
print acc    # output = 0.616

# C = 1000
rbf_clf_C1000 = SVC(kernel="rbf", C=1000.0).fit(features_train, labels_train)
pred = rbf_clf_C1000.predict(features_test)
acc = accuracy_score(pred, labels_test)
print acc    # output = 0.821

# C = 10000
rbf_clf_C10000 = SVC(kernel="rbf", C=10000.0).fit(features_train, labels_train)
pred = rbf_clf_C10000.predict(features_test)
acc = accuracy_score(pred, labels_test)
print acc    # output = 0.892

# C = 10,000 gives the best accuracy, but also gives the most complex boundary.
# As more points are classified correctly, traded off against line smoothness.

##############################################################################

""" Now that you’ve optimized C for the RBF kernel, go back to using the full 
    training set. In general, having a larger training set will improve the 
    performance of your algorithm, so (by tuning C and training on a large 
    dataset) we should get a fairly optimized result. What is the accuracy of
    the optimized SVM? """
    
# full dataset:    
features_train, features_test, labels_train, labels_test = preprocess()

# Fit rbf boundary to full dataset with C=10,000    (takes a while)
rbf_clf_C10000_full = SVC(kernel="rbf", C=10000.0).fit(features_train, labels_train)
pred = rbf_clf_C10000.predict(features_test)
acc = accuracy_score(pred, labels_test)
print acc    # output = 0.991 = pretty good!

##############################################################################

""" What class does your SVM (0 or 1, corresponding to Sara and Chris 
    respectively) predict for element 10 of the test set? The 26th? The 50th? 
    (Use the RBF kernel, C=10000, and 1% of the training set. Normally you'd 
    get the best results using the full training set, but we found that using 
    1% sped up the computation considerably and did not change our results).
    
    The data point numbers that we give here (10, 26, 50) assume a zero-indexed
    list. So the correct answer for element #100 would be found using something
    like answer=predictions[100] """
    
# Take a subset of 1% of the training dataset
features_train = features_train[:len(features_train)/100] 
labels_train = labels_train[:len(labels_train)/100] 

# Fit the optimised SVM algorithm (rbf kernel with C=10,000) to the reduced dataset
rbf_clf_C10000_red = SVC(kernel="rbf", C=10000.0).fit(features_train, labels_train)

# Make predictions and find specific results
pred = rbf_clf_C10000.predict(features_test)
pred[10]    # output = 1
pred[26]    # output = 0
pred[50]    # output = 1

##############################################################################

""" There are over 1700 test events - how many are predicted to be in the
    “Chris” (1) class? (Use the RBF kernel, C=10000., and the full training 
    set.) """
    
 # full dataset:    
features_train, features_test, labels_train, labels_test = preprocess()

# Fit rbf boundary to full dataset with C=10,000    (takes a while)
rbf_clf_C10000_full = SVC(kernel="rbf", C=10000.0).fit(features_train, labels_train)
pred = rbf_clf_C10000_full.predict(features_test)

sum(pred)    # as results are 1 or 0, sum will give the number of 1 scores.
             # output = 877
           
##############################################################################

# Compare linear kernels with different gammas (sensitivity to outliers)
linear_g1_C1 = SVC(kernel="linear", gamma=1.0, C=1.0).fit(features_train, labels_train)
linear_g1000_C1 = SVC(kernel="linear", gamma=1000, C=1.0).fit(features_train, labels_train)
