# -*- coding: utf-8 -*-
"""
Created on Mon Jul 24 14:13:15 2017

Udacity Intro to Machine Learning: Lesson 2 Naive Bayes Quiz

http://scikit-learn.org/stable/modules/generated/sklearn.naive_bayes.GaussianNB.html

"""
# See file nb_author_id.py
# in C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\naive_bayes


# Example training data
X = [[0, 0], [0, 1]]    # Training features = an array X of size [n_samples, n_features].
y = [0, 1]              # Training labels = an array y of class labels (strings or integers) 
                        #  and size [n_samples].
                        
                        
### Template Code to create, train and predict from a naive Bayes classifier:
from sklearn.naive_bayes import GaussianNB    # import the Naive Bayes module
clf = GaussianNB()    # create a classifier named 'clf'
clf.fit(features_train, labels_train)   # train the classifier using training data
pred = clf.predict(features_test)   # make predictions (you pass in the test features and predict the labels)


### To measure the accuracy of predictions:
from sklearn.metrics import accuracy_score      # import the accuracy_score function
accuracy = accuracy_score(pred, labels_test)    # calculate the accuracy score by comparing pred with the known (test)labels.
print accuracy    # answer = 0.973