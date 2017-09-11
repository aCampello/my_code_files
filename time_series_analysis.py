# -*- coding: utf-8 -*-
"""
Created on Sat Aug 05 17:03:26 2017

    Notes on Time series analysis (TSA) in Python

"""

# From: http://earthpy.org/pandas-basics.html
# see jupyter notebook saved in my_cod_files / time_series folder

import numpy as no
import pandas as pd

# example data
array([[  1.95000000e+03,   1.00000000e+00,  -6.03100000e-02],
                [  1.95000000e+03,   2.00000000e+00,   6.26810000e-01]])


# convert it to time series

# first create the range of dates for our time series (based on first/last values in the raw data)
dates = pd.date_range('1950-01', '2013-03', freq='M')  # freq='M' = data frequency is once per month

# then create the time series
# the index = the dates from the (first 2 columns of) the 'dates' variable; value = the values in 'mydata'.
data_ts = Series(mydata[:,2], index=dates)

# plot the time series
data_ts.plot()


