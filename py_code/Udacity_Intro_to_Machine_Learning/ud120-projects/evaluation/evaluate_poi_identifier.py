#!/usr/bin/python


"""
    Starter code for the evaluation mini-project.
    Start by copying your trained/tested POI identifier from
    that which you built in the validation mini-project.

    This is the second step toward building your POI identifier!

    Start by loading/formatting the data...
"""

### Get and prepare data

import pickle
import sys
sys.path.append("../tools/")
from feature_format import featureFormat, targetFeatureSplit

data_dict = pickle.load(open("./final_project/final_project_dataset.pkl", "r") )

### add more features to features_list!
features_list = ["poi", "salary"]

data = featureFormat(data_dict, features_list)
labels, features = targetFeatureSplit(data)

### your code goes here

### Split into training and testing data
from sklearn.model_selection import train_test_split
# hold back 30% data for testing:
features_train, features_test, labels_train, labels_test = train_test_split(features, labels, test_size=0.3, random_state=42)  


###############################################################################

### Make decision tree
from sklearn import tree
clf = tree.DecisionTreeClassifier()
clf.fit(features_train, labels_train)

###############################################################################

""" Lesson 15 Quiz 28: How many POIs are predicted for the test set for your 
    POI identifier? """
pred = clf.predict(features_test)
sum(pred)  # = 4 (calculated this way because POIs are marked as 1, non-POIs as 0)


""" Lesson 15 Quiz 29: How many people total are in your test set? """
len(features_test)  # 29 in test set (as there is one row/datapoint per person)
len(features_train)  # 66 in training set


""" Lesson 15 Quiz 30: If your identifier predicted 0. (not POI) for everyone 
    in the test set, what would its accuracy be? """
# true/actual N POIs in test set:
sum(labels_test)  # = 4

# There are 29 people in the test set, 4 of which are POIs (so 25 = non-POIs). 
# Accuracy = [N items in class that are labelled correctly / N items in class]
# If identifier predicted 0 for everyone in test set, 25 would be labelled correctly
# So accuracy = 25.0/29.0 = 0.862
    
    
""" Lesson 15 Quiz 31: Look at the predictions of your model and compare them
    to the true test labels. Do you get any true positives? 
    (In this case, we define a true positive as a case where both the actual
    label and the predicted label are 1). """    

# list comprehension to compare the pred list with the labels_test list
counter = 0
for i, j in zip(pred, labels_test):
    if i == 1.0:   # only look at the positives in pred
        if i == j:
            counter += 1
print counter  # = 0
# equivalent to this list comprehension:
len([1 for i, j in zip(labels_test, pred) if i == 1.0 if i == j])  # = 0      

""" There were no true positives: having imbalanced classes like we have in 
    the Enron dataset (many more non-POIs than POIs) introduces some special 
    challenges, namely that you can just guess the more common class label 
    for every point (not a very insightful strategy) and still get pretty 
    good accuracy! """

###############################################################################

""" Lesson 15 Quiz 32: Precision and recall can help illuminate your 
    performance better. 
    Use the precision_score and recall_score available in sklearn.metrics 
    to compute those quantities. What's the precision? """     

from sklearn.metrics import precision_score
from sklearn.metrics import recall_score
   
# Precision = tp / (tp + fp) where tp=true positives and fp=false positives.
precision = precision_score(y_true, y_pred)
precision = precision_score(labels_test, pred)  # = 0.0

# Recall = tp / (tp + fn) where tp=true positives and fn=false negatives.  
recall = recall_score(y_true, y_pred)
recall = recall_score(labels_test, pred) # = 0.0

""" Although the accuracy was high, the low precision and recall indicates
    that the algorithm requires further optimisation. Could try a different
    algorithm such as an SVM, use K-Fold Cross Validation and/or tune the 
    parameters. """
###############################################################################



    