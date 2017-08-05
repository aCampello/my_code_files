# -*- coding: utf-8 -*-
"""
Created on Thu Jul 27 11:12:29 2017

Udacity Intro to Machine Learning: Lesson 7 Regressions

http://scikit-learn.org/stable/modules/linear_model.html

For more coding and quizzes see file finance_regression.py in:
C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\regression

"""

### Linear regression in sklearn

from sklearn.linear_model import LinearRegression
reg = LinearRegression()
reg.fit(features_train, labels_train)

# or as a function:
def regfun(features_train, labels_train):
    from sklearn.linear_model import LinearRegression
    reg = LinearRegression()
    reg.fit(features_train, labels_train)
    return reg
reg = regfun(features_train, labels_train) 

# predictions
preds = reg.predict(features_test)  # where features_test is an array of shape = (n_samples, n_features)

# e.g. to predict the y-value for a new point [2, 4] (i.e. value for feature_1 = 2 and value for feature_2 = 4)
reg.predict([[2,4]])         # list of lists (one per point, despite there being only one point in this example)
# e.g. to predict the y-value for two new points
reg.predict([[2,4], [2,7]])  


# access the coefficients and intercept
print reg.coef_
print reg.intercept_

###############################################################################
