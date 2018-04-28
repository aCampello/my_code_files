#!/usr/bin/python

"""
    This is the code to accompany the Lesson 1 (Naive Bayes) mini-project.

    Project spec = "Create and train a Naive Bayes classifier in naive_bayes/nb_author_id.py. 
    Use it to make predictions for the test set. What is the accuracy?"

    Use a Naive Bayes Classifier to identify emails by their authors

    authors and labels:
    Sara has label 0
    Chris has label 1
"""
### Set the working directory to where the course files are saved
cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\
# check it worked
pwd

### Set up the code
import sys
from time import time
sys.path.append("../tools/") # change the wd to /tools
from email_preprocess import preprocess # imports the function preprocess from the python script file email_preprocess.py

# Load the data (and process it using the preprocess function)
features_train, features_test, labels_train, labels_test = preprocess()
# features_train & features_test are the features for the training & testing datasets.
# labels_train & labels_test are the corresponding item labels.


#########################################################

### your code goes here ###

### scikit-learn is the package used for machine learning
# (installation instructions: http://scikit-learn.org/stable/install.html)

#########


### Create, train and make predictions from a new classifier

from sklearn.naive_bayes import GaussianNB    # import the Naive Bayes module

clf = GaussianNB()    # create a classifier named 'clf'

clf.fit(features_train, labels_train)   # train the classifier using training data (features (X) and labels (y))

pred = clf.predict(features_test)   # make predictions

#########

### Measure the accuracy of predictions:
from sklearn.metrics import accuracy_score      # import the accuracy_score function
accuracy = accuracy_score(pred, labels_test)    # calculate the accuracy score.
print accuracy    # answer = 0.973

#########

### Measure the time taken to train the classifier
t0 = time()
clf.fit(features_train, labels_train)
print "training time:", round(time()-t0, 3), "s"   # answer = 2.158 seconds

### Measure the time taken to make predictions with the classifier
t0 = time()
pred = clf.predict(features_test)
print "prediction time:", round(time()-t0, 3), "s"   # answer = 0.385 seconds

## Which is faster? - predicting is faster than training. 