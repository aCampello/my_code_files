#!/usr/bin/python
import matplotlib.pyplot as plt
cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\choose_your_own
from prep_terrain_data import makeTerrainData
from class_vis import prettyPicture

features_train, labels_train, features_test, labels_test = makeTerrainData()


### the training data (features_train, labels_train) have both "fast" and "slow"
### points mixed together--separate them so we can give them different colors
### in the scatterplot and identify them visually
grade_fast = [features_train[ii][0] for ii in range(0, len(features_train)) if labels_train[ii]==0]
bumpy_fast = [features_train[ii][1] for ii in range(0, len(features_train)) if labels_train[ii]==0]
grade_slow = [features_train[ii][0] for ii in range(0, len(features_train)) if labels_train[ii]==1]
bumpy_slow = [features_train[ii][1] for ii in range(0, len(features_train)) if labels_train[ii]==1]


#### initial visualization
plt.xlim(0.0, 1.0)
plt.ylim(0.0, 1.0)
plt.scatter(bumpy_fast, grade_fast, color = "b", label="fast")
plt.scatter(grade_slow, bumpy_slow, color = "r", label="slow")
plt.legend()
plt.xlabel("bumpiness")
plt.ylabel("grade")
plt.show()

###############################################################################

### K Nearest Neighbour Classifier

# http://scikit-learn.org/stable/modules/generated/sklearn.neighbors.KNeighborsClassifier.html#sklearn.neighbors.KNeighborsClassifier

###############################################################################
### your code here!  name your classifier object clf if you want the 
### visualization code (prettyPicture) to show you the decision boundary
###############################################################################

### Make a classifier function
def classify(features_train, labels_train, n_neighbors, weights):
    from sklearn.neighbors import KNeighborsClassifier
    clf = KNeighborsClassifier(n_neighbors, weights)    
    clf.fit(features_train, labels_train)
    return clf

### Test the algorithm using different k and weights parameters

# using default k=5 + uniform weighting
clf = classify(features_train, labels_train, n_neighbors=5, weights="uniform")
pred = clf.predict(features_test)
from sklearn.metrics import accuracy_score 
accuracy_score(pred, labels_test)    # = 0.920

# using default k=5 + distance weighting
clf = classify(features_train, labels_train, n_neighbors=5, weights="distance")
pred = clf.predict(features_test)
accuracy_score(pred, labels_test)    # = 0.932 # distance weights increase accuracy.

#######

# using k=3 + uniform weighting
 clf = classify(features_train, labels_train, n_neighbors=3, weights="uniform")
pred = clf.predict(features_test)
accuracy_score(pred, labels_test)    # = 0.936 = the same as distance

# using k=3 + distance weighting
clf = classify(features_train, labels_train, n_neighbors=3, weights="distance")
pred = clf.predict(features_test)
accuracy_score(pred, labels_test)    # = 0.936 = the same as distance

#######

# using k=1 + uniform weighting
clf = classify(features_train, labels_train, n_neighbors=1, weights="uniform")
pred = clf.predict(features_test)
accuracy_score(pred, labels_test)    # = 0.939 # k=1 is most accurate (though probably overfit!)

# using k=1 + distance weighting - no point as only considering one neighbour.

#######

# using k=50 + uniform weighting
clf = classify(features_train, labels_train, n_neighbors=50, weights="uniform")
pred = clf.predict(features_test)
accuracy_score(pred, labels_test)    # = 0.928 and comparable boundary when plotted.

# using k=50 + distance weighting
clf = classify(features_train, labels_train, n_neighbors=50, weights="distance")
pred = clf.predict(features_test)
accuracy_score(pred, labels_test)    # = 0.928 and comparable boundary when plotted.

#######

# using k=100 + uniform weighting
clf = classify(features_train, labels_train, n_neighbors=100, weights="uniform")
pred = clf.predict(features_test)
accuracy_score(pred, labels_test)    # = 0.928 and more wiggly boundary

# using k=100 + distance weighting
clf = classify(features_train, labels_train, n_neighbors=100, weights="distance")
pred = clf.predict(features_test)
accuracy_score(pred, labels_test)    # = 0.932 and straighter boundary


# Increasing k smoothes the boundary and reduces the accuracy. 
# Distance weights increase the accuracy.

###############################################################################

# To plot the decision boundary
# (the prettyPicture function is from Udacity file class_vis.py)

try:
    prettyPicture(clf, features_test, labels_test)
except NameError:  # if the classifier is named something other than clf it returns an error.
    pass

###############################################################################