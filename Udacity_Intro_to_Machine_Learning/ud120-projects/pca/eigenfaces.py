"""

Lesson 13. PCA
===================================================
Faces recognition example using eigenfaces and SVMs
===================================================

The dataset used in this example is a preprocessed excerpt of the
"Labeled Faces in the Wild", aka LFW_:

  http://vis-www.cs.umass.edu/lfw/lfw-funneled.tgz (233MB)

  .. _LFW: http://vis-www.cs.umass.edu/lfw/

  original source: http://scikit-learn.org/stable/auto_examples/applications/face_recognition.html

"""

from time import time
import logging
import pylab as pl
import numpy as np

from sklearn.model_selection import train_test_split
from sklearn.datasets import fetch_lfw_people
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.decomposition import PCA
from sklearn.svm import SVC

# Display progress logs on stdout
logging.basicConfig(level=logging.INFO, format='%(asctime)s %(message)s')


###############################################################################
### Import and examine the data
# info on: http://scikit-learn.org/stable/modules/generated/sklearn.datasets.fetch_lfw_people.html

# Download the data and store it in default folder ('C:\Users\User\scikit_learn_data'. 
# Then if you run this line again sklearn gets the data from that folder 
# automatically (I can't seem to specify the path!). Loads the data as numpy arrays.
lfw_people = fetch_lfw_people(min_faces_per_person=70, resize=0.4)  # only loads data from people (labels) with min 70 photos.

# introspect the images arrays to find the shapes (for plotting)
n_samples, h, w = lfw_people.images.shape
np.random.seed(42)

# for machine learning we use the data directly (as relative pixel
# position info is ignored by this model)
X = lfw_people.data   # lfw_people = the dataset, so now X = features
n_datapoints = X.shape[0]  # = 1288
n_features = X.shape[1]  # = 1850

# the label to predict is the id of the person
y = lfw_people.target  # lfw_people = the dataset, so now y=labels
target_names = lfw_people.target_names   # get the names of the targets (labels) in the lfw_people dataset
n_classes = target_names.shape[0]   # = dataset contains 7 different labels/people/classes

print "Total dataset size:"
print "n_samples: %d" % n_samples
print "n_features: %d" % n_features
print "n_classes: %d" % n_classes


###############################################################################
### Split data into a training and testing set
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=42)

###############################################################################
### Compute a PCA (eigenfaces) on the face dataset (treated as unlabeled
# dataset): unsupervised feature extraction / dimensionality reduction
n_components = 250

print "Extracting the top %d eigenfaces from %d faces" % (n_components, X_train.shape[0])
t0 = time()
pca = PCA(n_components=n_components, svd_solver='randomized', whiten=True).fit(X_train)
print "done in %0.3fs" % (time() - t0)

eigenfaces = pca.components_.reshape((n_components, h, w))

print "Projecting the input data on the eigenfaces orthonormal basis"
t0 = time()
X_train_pca = pca.transform(X_train)
X_test_pca = pca.transform(X_test)
print "done in %0.3fs" % (time() - t0)

##########
# Quiz 34: find the variance explained by PC1 and PC2
""" We mentioned that PCA will order the principal components, with the first 
    PC giving the direction of maximal variance, second PC has second-largest 
    variance, and so on. How much of the variance is explained by the first
    principal component? The second? """
    
print pca.explained_variance_ratio_   # first=0.19346527, second=0.15116846


###############################################################################
### Train a SVM classification model

print "Fitting the classifier to the training set"
t0 = time()

# make dictionary of different parameter values (here C and gamma) to test using GridSearchCV
param_grid = {'C': [1e3, 5e3, 1e4, 5e4, 1e5],
              'gamma': [0.0001, 0.0005, 0.001, 0.005, 0.01, 0.1], }

# for sklearn version 0.16 or prior, the class_weight parameter value is 'auto'
# for sklearn version 0.17 onwards, the class_weight parameter value is 'balanced'
clf = GridSearchCV(SVC(kernel='rbf', class_weight='balanced'), param_grid)

clf = clf.fit(X_train_pca, y_train)

print "done in %0.3fs" % (time() - t0)
print "Best estimator found by grid search:"
    
print clf.best_estimator_       # show the fitted classifier with the best parameter combination.


###############################################################################
### Quantitative evaluation of the model quality on the test set

# make predictions    
print "Predicting the people names on the testing set"
t0 = time()
y_pred = clf.predict(X_test_pca)
print "done in %0.3fs" % (time() - t0)

# print report showing precision, recall, F1-score snd support for each classification
# (select and print these 2 lines at once).
print classification_report(y_test, y_pred, target_names=target_names)
print confusion_matrix(y_test, y_pred, labels=range(n_classes))


""" Quiz 35: As you add more principal components as features for training 
    your classifier, do you expect it to get better or worse performance? """
# avg f-score for 50 components = 0.81
# avg f-score for 150 components = 0.84
# avg f-score for 200 components = 0.85
# adding more PCs increases the f-score, so improves the classifier's performance.

""" Change n_components to the following values: [10, 15, 25, 50, 100, 250]. 
    For each number of principal components, note the F1 score for Ariel Sharon. 
    (For 10 PCs, the plotting functions in the code will break, but you should 
    be able to see the F1 scores.) 
    If you see a higher F1 score, does it mean the classifier is doing better, 
    or worse?   -- Better! """

# f-scores for Ariel Sharon:
    # 10 PCs = 0.12    (SVC took a while to fit)
    # 50 PCs = 0.67    (SVC was a bit faster to fit)
    # 100 PCs = 0.64     (SVC was about the same to fit)
    # 250 PCs = 0.55    (SVC took a while to fit)

""" Performance (F1 score) starts to drop as you add too many PCs """    

    
###############################################################################
### Qualitative evaluation of the predictions using matplotlib

def plot_gallery(images, titles, h, w, n_row=3, n_col=4):
    """Helper function to plot a gallery of portraits"""
    pl.figure(figsize=(1.8 * n_col, 2.4 * n_row))
    pl.subplots_adjust(bottom=0, left=.01, right=.99, top=.90, hspace=.35)
    for i in range(n_row * n_col):
        pl.subplot(n_row, n_col, i + 1)
        pl.imshow(images[i].reshape((h, w)), cmap=pl.cm.gray)
        pl.title(titles[i], size=12)
        pl.xticks(())
        pl.yticks(())

# plot the result of the prediction on a portion of the test set
def title(y_pred, y_test, target_names, i):
    pred_name = target_names[y_pred[i]].rsplit(' ', 1)[-1]
    true_name = target_names[y_test[i]].rsplit(' ', 1)[-1]
    return 'predicted: %s\ntrue:      %s' % (pred_name, true_name)

prediction_titles = [title(y_pred, y_test, target_names, i)
                         for i in range(y_pred.shape[0])]

plot_gallery(X_test, prediction_titles, h, w)


# plot the gallery of the most significative eigenfaces
eigenface_titles = ["eigenface %d" % i for i in range(eigenfaces.shape[0])]
plot_gallery(eigenfaces, eigenface_titles, h, w)

pl.show()

###############################################################################