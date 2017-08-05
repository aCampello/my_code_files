#!/usr/bin/python

""" Udacity Intro to machine learning
    Code for quizzes at end of Lesson 12: Feature Selection:
    
    The aim of this was to use a tiny training set to create an overfitted
    decision tree, and then work out why we might not be able to detect this
    blatant overfitting due to certain key features in the data. 
    
    In this case, these features were words that were exceptionally common
    in the dataset, but not very meaningful (email signatures by Chris and Sara).
    
    Here we detect the outliers and re-fit the decision tree without them, 
    which allowed the overfitting to become more clear (indicated by a lower
    accuracy of the test set than the training set). 
"""

import pickle
import numpy
numpy.random.seed(42)


### The words (features) and authors (labels), already largely processed.
### These files should have been created from the previous mini-project 
### (Lesson 10 text learning).
cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\text_learning
word_data = pickle.load(open("your_word_data.pkl", "rb"))
author_data = pickle.load(open("your_email_authors.pkl", "rb"))


### test_size is the percentage of events assigned to the test set (the
### remainder go into training)
### feature matrices changed to dense representations for compatibility with
### classifier functions in versions 0.15.2 and earlier
from sklearn import model_selection
features_train, features_test, labels_train, labels_test = model_selection.train_test_split(word_data, author_data, test_size=0.1, random_state=42)

from sklearn.feature_extraction.text import TfidfVectorizer
vectorizer = TfidfVectorizer(sublinear_tf=True, max_df=0.5, stop_words='english')
features_train = vectorizer.fit_transform(features_train)
features_test  = vectorizer.transform(features_test).toarray()


### a classic way to overfit is to use a small number
### of data points and a large number of features;
### train on only 150 events to put ourselves in this regime
features_train = features_train[:150].toarray()
labels_train   = labels_train[:150]



### your code goes here
""" Get a decision tree up and training on the training data, and print out 
    the accuracy. """

# tree    
from sklearn import tree 
clf = tree.DecisionTreeClassifier() 
clf.fit(features_train, labels_train)


# accuracy
from sklearn.metrics import accuracy_score

# for the training set (as a comparison)
pred = clf.predict(features_train)
accuracy_score(pred, labels_train)   # =  1.0

# for the testing set
pred = clf.predict(features_test)
accuracy_score(pred, labels_test)   # = 0.959

""" An accuracy score as high as 0.959 is really high for a test set when
    the training set was only 150 samples and most probably overfit:
    You'd expect the test accuracy to be way lower than the training accuracy
    as a result of this overfitting, but it isn't.
    
    This suggests there might be outliers (exceptionally common words) 
    in the dataset that are masking the effects of overfitting. """

##############################################################################

""" Take your (overfit) decision tree and use the feature_importances_ 
    attribute to get a list of the relative importance of all the features 
    being used. 
    
    We suggest iterating through this list (it’s long, since this 
    is text data) and only printing out the feature importance if it’s above 
    some threshold (say, 0.2--remember, if all words were equally important, 
    each one would give an importance of far less than 0.01).
    
    What’s the importance of the most important feature? What is the number of this 
    feature? """

## Check for outliers / features with unusually high importance:
    
#   Loop through the features and print only the features with importance > 0.2.
#   Use a counter to get the feature number.
counter = -1
for importance in clf.feature_importances_:
    counter += 1
    if importance > 0.2:
        print "Feature number:", counter, "Importance:", importance
        
# Feature number: 33614 Importance: 0.7645

##############################################################################

""" In order to figure out what words are causing the problem, you need to go
    back to the TfIdf and use the feature numbers that you obtained in the 
    previous part of the mini-project to get the associated words. 
    You can return a list of all the words in the TfIdf by calling 
    get_feature_names() on it; pull out the word that’s causing most of the 
    discrimination of the decision tree. What is it? Does it make sense as a 
    word that’s uniquely tied to either Chris Germany or Sara Shackleton, a 
    signature of sorts? """

# What is word number 33614 in the vectorizer?
vectorizer.get_feature_names()[33614]    # = 'sshacklensf'
'stephaniethank'

##############################################################################

""" This word seems like an outlier in a certain sense, so let’s remove it 
    and refit. Go back to text_learning/vectorize_text.py, and remove this 
    word from the emails using the same method you used to remove “sara”, 
    “chris”, etc. Rerun vectorize_text.py, and once that finishes, rerun 
    find_signature.py. 
    Any other outliers pop up? What word is it? 
    Seem like a signature-type word? 
    (Define an outlier as a feature with importance >0.2, as before). """

# Re-ran for importance in clf.feature_importances_ and got this output:
# Feature number: 14343 Importance: 0.667
vectorizer.get_feature_names()[14343] # = 'cgermannsf'  # could be Chris's email signature

##############################################################################

""" Update vectorize_test.py one more time, and rerun. Then run 
    find_signature.py again. Any other important features (importance>0.2) 
    arise? How many? Do any of them look like “signature words”, or are they 
    more “email content” words, that look like they legitimately come from the
    text of the messages? """
    
# Re-ran for importance in clf.feature_importances_ and got this output:
# Feature number: 21323 Importance: 0.364
vectorizer.get_feature_names()[21323] # = 'houectect'   # doesn't look like a signature word


##############################################################################

""" What’s the accuracy of the decision tree now? We've removed two "signature
    words", so it will be more difficult for the algorithm to fit to our 
    limited training set without overfitting. 
    Remember, the whole point was to see if we could get the algorithm to 
    overfit--a sensible result is one where the accuracy isn't that great, as
    it will always be much higher on the training than the testing set. A 
    lowish accuracy on the test set indicates no/less overfitting """

# new accuracy
from sklearn.metrics import accuracy_score 

pred = clf.predict(features_train)
accuracy_score(pred, labels_train)   # =  1.0

pred = clf.predict(features_test)
accuracy_score(pred, labels_test)   # = 0.819
""" Now we have removed the signature words 'sshacklensf' and 'cgermannsf',
    the accuracy is lower on the test set than the training set and starting 
    to indicate that we have overfit to our limited training set (of 150
    examples). """
    
# Conclusion: when analysing text, check for exceptionally common words that
# could be outliers and masking any overfitting that's going on.
# Could do this by limiting the training set to a size that you'd expect to
# cause overfitting, fitting a decision tree (as they are prone to overfitting)
# and comparing the accuracy of the training and testing sets. They should be 
# very different to indicate overfitting, and if not then there could be some
# outliers/unwanted features/extra stopwords in the data that need removing.                                                         

##############################################################################








