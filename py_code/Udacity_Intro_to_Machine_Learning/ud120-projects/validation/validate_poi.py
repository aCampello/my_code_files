#!/usr/bin/python


"""
    Starter code for the lesson 14 validation mini-project.
    The first step toward building your POI identifier!

    Start by loading/formatting the data

    After that, it's not our code anymore--it's yours!
"""

##############################################################################
### Prepare the data
""" Read in the data and format it into lists of labels and features """

import pickle
import sys
cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects
sys.path.append("/tools")
from feature_format import featureFormat, targetFeatureSplit

data_dict = pickle.load(open("./final_project/final_project_dataset.pkl", "r") )

### The first element is our labels, any added elements are predictor features. 
""" Keep this the same for the mini-project, but you'll have a different feature 
    list when you do the final project. """
features_list = ["poi", "salary"]

data = featureFormat(data_dict, features_list)
labels, features = targetFeatureSplit(data)

##############################################################################
### Create an overfit decision tree classifier without using separate 
#   training and testing sets

from sklearn import tree
clf = tree.DecisionTreeClassifier()
clf.fit(features, labels)

# accuracy:
print clf.score(features, labels)  # = 0.989
# alternative accuracy score method:
from sklearn.metrics import accuracy_score
pred = clf.predict(features)
accuracy_score(pred, labels)  # = 0.989
""" Testing on the training data makes you think you're doing amazingly well,
    but this classifier is overfit. """
    
##############################################################################
### Add in training and testing to get a trustworthy accuracy score

from sklearn.model_selection import train_test_split

# hold back 30% data for testing:
features_train, features_test, labels_train, labels_test = train_test_split(features, labels, test_size=0.3, random_state=42)  

# new decision tree
clf = tree.DecisionTreeClassifier()
clf.fit(features_train, labels_train)

# new accuracy:
print clf.score(features_test, labels_test)   # = 0.724

# and accuracy using the alternative method:
from sklearn.metrics import accuracy_score
pred = clf.predict(features_test)
accuracy_score(pred, labels_test)  # = 0.724
""" This accuracy is more realistic """

##############################################################################
