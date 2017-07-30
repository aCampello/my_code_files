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
reg.fit(ages_train, net_worths_train)

# or as a function:
def studentReg(ages_train, net_worths_train):
    from sklearn.linear_model import LinearRegression
    reg = LinearRegression()
    reg.fit(ages_train, net_worths_train)
    return reg
reg = studentReg(ages_train, net_worths_train)

""""""
