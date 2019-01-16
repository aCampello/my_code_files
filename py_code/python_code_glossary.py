# -*- coding: utf-8 -*-

                    ## -------------------------------- ##
                    """ Python - useful bits of code """
                    ## -------------------------------- ##

# CONTENTS
# basic mathematical functions
# big data
# bigquery
# classes
# code formatting
# conditional statements: if/else, booleans, decision maps
# data cleaning
# dates and times
# dictionaries
# display
# distributions (numpy)
# email from Python
# environments
# errors and bugs
# feature scaling
# file input/output
# for loops
# functions
# get name of python objects
# HTML & BeautifulSoup
# Inspecting objects defined in the console
# Installing / loading packages libraries
# jupyter notebook
# linear regression
# lists
# machine learning
# missing values
# numpy
# outliers
# package management
# pandas
## querying, indexing
# plotting
# python 2 vs python 3
# randomising or finding all combinations of things
# raw input
# regex
# rounding / reducing d.p. without rounding
# s3
# saving files as csv etc
# saving python objects
# scipy (for hypothesis testing in python)
# set working directory
# shortcuts for running code
# SQLite DATABASES
# strings
# subsetting dfs
# tensorflow
# text processing
# timing code
# tuples
# variables
# while loops


# Make copy of new df
new_dataframe = dataframe.copy()

# General things
help(function_name) # get help on a python function

# =============================================================================
###  """ BASIC MATHEMATICAL OPERATIONS """
# =============================================================================

# the modulo operation
% # tests whether a number is divisible by another number and outputs the remainder
# e.g....
7 % 2   # = 1, as 2 fits 3 times into 7 with remainder 1.
8 % 2   # = 0, as 2 fits 4 times into 8 with no remainders.
14 % 12 # = 2, as 12 fits once into 14 with remainder 2.

# exponents
**
# e.g....
x ** 2  # x squared (x times x), e.g. 2 ** 2 = 4
x ** 3  # x cubed (x times x times x), e.g. 2 ** 3 = 8
2 ** 4  # 2 X 2 X 2 X 2 = 16

# mean
np.mean([0.70233333,  0.72133333,  0.72266667])


#==========================
### """ BIG DATA """
#==========================
# Out of memory computation (link sent via R4DS)
# read posts by Tom Augspurger, especially the *Modern Pandas* and the *Scalable Machine Learning* series:
# https://tomaugspurger.github.io



# =============================================================================
###  """  Google BigQuery """
# =============================================================================

# TABLE_DATE_RANGE

# Error message: "Error: Can't parse table: DestinationTables"
# - This means you have entered an incorrect table prefix - it should be the table name
# up to the start of the timestamp, e.g. the prefix for "times_data_20170101" is "times_data_"

query = """
    SELECT
        activity_date
        views
    FROM
        TABLE_DATE_RANGE([newsuk-datatech-s2ds-2017-2:project_data.times_data_],
                         TIMESTAMP('{}'),
                         TIMESTAMP('{}')),
    GROUP BY
        activity_date """


## Format function
# syntax: "TIMESTAMP('{}')".format()
# NOTE queries are entered as strings but .format() is not part of the string.
# e.g. syntax to select rows between a specified start_date and end_date
query = """
    SELECT * FROM [table_name] ......
        TIMESTAMP('{}'),
        TIMESTAMP('{}'))
        GROUP BY activity_date """.format(start_date, end_date) # and see S2DS codes

# alternatively, name the vars:
my_string = '{state}_{table_name}_log.csv'.format(state=state, table_name=table_name)




# Can't group by timestamp() functions so give them an alias:
SELECT HOUR(datetime) AS hour
FROM tablename
GROUP BY hour;

# =============================================================================
###  """ CLASSES """
# =============================================================================

#  A class is a way to take a grouping of functions and data and place them
#  inside a container so you can access them with the . (dot) operator.

#  Classes are like blueprints for creating objects of a predefined structure,
#  where the created objects are like mini modules.

### Create a new Parent Class:

#  The class name always starts with a capital letter
class Animal(object):
    """Makes cute animals."""  # documentation string for the help page.
    # 'member variables' apply to all instances of the object should go above the _init_:
    is_alive = True  # is_alive is a member variable.
    health = "good"  # health is another member variable.
    # Initialize your newly created empty object using def __init__():
    def __init__(self, name, age):
        # self is first parameter after def __init__(), which tells Python to refer to the object being created - gives it its identity.
        self.name = name  # specifies that 'name' refers to the created object's name.
        self.age = age
    # Add your method here!    # a 'class method' is a function within a class definition.
    def description(self):
        print(self.name)    # to print the name of instance of the object
        print(self.age)


#  Members of a class, e.g. an Animal class, are called instances,
#  e.g. a hippo is an instance of the Animal class.
#  Instances are variables: you instantiate (create) a new object from the
#  class and assign it to a variable (instance), e.g. 'hippo', to work with it.

# Instantiate a hippo instance:
hippo = Animal(name="Harriet", age=7)
hippo = Animal("Harriet", 7)  # can also exclude the labels.

#  Get stuff from classes in the same way as from modules:
import math  # import math module
math.sqrt  # print details of square root function in math module
math.sqrt(5)  # run the square root function from math module

# show hippo's name (assuming there's only one hippo) from Animal class:
hippo.name
hippo.is_alive

# Print hippo's description -this runs the Animal class's description() method.
hippo.description()



###  Create a child class based on a parent class

# The child class inherits variables and methods from the parent class.
#  Create by replacing (object) with (ParentClass) in the class creation code:
#  (Object is the most basic type of class & all Parent Classes inherit from Object; Child Classes will inherit from Object and their Parent Class).

class ChildClass(ParentClass):
    # new variables and functions go here


### The Representation Method - when you're initiating classes.
# use def __repr__() to tell Python how to present the class object.

class Point3D(object):
    def __init__(self, x, y, z):    #  standard class initiation function
        self.x = x
        self.y = y
        self.z = z
    def __repr__(self):             #  representation function
        return "(%d, %d, %d)" % (self.x, self.y, self.z)

my_point = Point3D(x=1, y=2, z=3) #  will represent the object in format (x,y,z).



# =============================================================================
###  """ CODE FORMATTING """
# =============================================================================

# I've set Preferences to check for IPython Styling so my code is easy for
# others to understand and debug.
# Start comments with a triple hashtag to make them appear in the Outline pane.

Ctrl + # increases font size in the editor

#  Indentation is important: a tab is equivalent to 4 spaces.


### Pandas idioms

## Pandas method chaining - like %>% in R
# start line with parentheses to run multiple lines in one call,

# e.g. drop NaNs where species = fox, set the index and rename a column...
(df.where(df['species'] == 'fox')
    .dropna()
    .set_index(['species', 'animalid'])
    .rename(columns={'individuals': 'n_foxes'}))

# e.g. drop entries where quantity = 0 and rename the weight column
(df.drop(df[df['Quantity'] == 0].index).rename(columns={'Weight': 'Weight (oz.)'}))


## Pandas apply function
# applies a function across rows of a df
# typically used with lambdas, but can also be run with long functions as below

# 1. First create a function that returns the value to use as a replacement
def min_max(row):
    data = row[['spring', 'summer', 'autumn', 'winter']]
    return pd.Series({'min': np.min(data), 'max': np.max(data)})

# 2. Then apply the function across columns using axis=1
df_new = df.apply(min_max, axis=1)


# Alternatively, have the function add new columns to existing df (useful when merging dfs)
def min_max(row):
    data = row[['spring', 'summer', 'autumn', 'winter']]
    row['max'] = np.max(data)
    row['min'] = np.min(data)
    return row
df = df.apply(min_max, axis=1)

# as a lambda function: much cleaner!
rows = ['spring', 'summer', 'autumn', 'winter']
df.apply(lambda x: np.max(x[rows]), axis=1)

# strip parentheses from country names in each row of df (e.g. 'China (Republic of)' -> 'China')
# apply to run func on each row, [0] to return words on LHS of the opening parenthesis only.
energy['Country'] = energy['Country'].apply(lambda x: x.split('(')[0])

# lambda syntax
df.apply(function, input1, input2, input3...)




# =============================================================================
###  """ CONDITIONAL STATEMENTS """
# =============================================================================

### IF / ELSE ifelse

# syntax
if <test expression>:   # if the test expression is true, the block (procedure) below will execute.
                        # if it's false, the block won't execute.
    <block>             # the procedure to run if test expression is true (indented).

# example test expressions using if:
if x > y:           # if x is greater than y
if not x > y:       # if x is not greater than y (equivalent to if x <= y)
if not list_1:      # if list_1 is empty

# if-else statement: will do something if the statement is true, or something else if it is false.
# e.g. here, if x is divisible by 5 it will return True, otherwise return False
# (without the else statement it'd return None instead of False):
if x % 5 == 0:
    return True
else:
    return False

# elif statements are to create further if statements, e.g.
if AgeClass == 1:
    return "Adult"
elif AgeClass == 2:
    return "Subadult"
elif AgeClass == 3:
    return "Cub"
else:
    return "Unknown status"


### """ Boolean expressions """
### The 'and' expression
# Returns True only if both expressions are True:
True and True = True
False and False = False
True and False = False

### The 'or' expression
# Evaluates only what it needs to, e.g. for the expression:
<expression_1> or <expression_2>    # if exp_1 is True, exp_2 is ignored and True is returned
                                    # if exp_1 is False, the value of exp_2 is returned. So:
True or True = True
True or False = True
False or True = True
False or False = False


### Decision map dictionaries (the 'dispatch' method)
# http://scottlobdell.me/2014/05/time-efficiency-statements-vs-python-dictionaries/
# https://softwareengineering.stackexchange.com/questions/182093/why-store-a-function-inside-a-python-dictionary
# much cleaner and faster than having many if statements inside a function.

decision_map = {
    '1': do_something,
    '2': do_something_else,
    '3': do_another_thing
}
def decision_func(input_var):
    func = decision_map[input_var]
    func()

# same as...
decision_map[input_var]() # Gets the correct function from response_dict and calls it as a function using ()

# safer than eval()
# Using a dispatch method is safer than other techniques, such as eval(), as it limits the commands allowable to
# what you defined beforehand, e.g. hackers can't sneak a <DROP TABLE Students;>  injection past a dispatch table.



# =============================================================================
### """ DATA CLEANING """
# =============================================================================

# To extract certain columns from a dataframe
wanted_columns = data[['Sex', 'Age', 'Fare', 'Survived']]


# create a copy of the data
cleaned_data = cleaned_wanted_columns.copy()

# change a 2-level category to boolean
is_female = (cleaned_data['Sex'] == 'female')
cleaned_data['Sex'] = is_female



# Mapping - to rename entries in a pandas df column (Series)
# faster than looping through each entry
# https://pandas.pydata.org/pandas-docs/stable/generated/pandas.Series.map.html

# e.g. like Antonio wrote to clean sections in cuzco:

dataframe_clean = dataframe.copy()
section_list = ['news', 'living', 'money', 'motors', 'sport', 'tv & showbiz', 'tech', 'travel', 'all']
to_replace = {}  # make empty dict

# A hash to map rename the section in time O(n)(instead of 2n):
for section in dataframe_clean['section'].unique():
	if section in section_list:
		to_replace[section] = section
	else:
		to_replace[section] = 'other'

dataframe_clean['section'] = dataframe_clean['section'].map(to_replace)

# Old code to replace by looping - slower - takes 2N
# dataframe_clean = dataframe.copy()
#
# to_replace = [section for section in dataframe_clean['section'] if section not in section_list]
# value = 'other'
# dataframe_clean['section'].replace(to_replace, value, inplace=True)

# without using a function to make the dictionary...
# rename column of financial year ranges, e.g. 2000-01, to the first year only.
mapping = {'2000-01': 2000, '2001-02': 2001, '2002-03': 2002, '2003-04': 2003}
veh_data['year_code'] = veh_data['FinancialYear'].map(mapping)


# Create training and testing sets
X = cleaned_data.drop('Survived', axis=1)  # drops the labels from the dataframe
y = cleaned_data['Survived']  # saves only the column of labels




# =============================================================================
### """ DATES & TIMES """
# =============================================================================

import datetime as dt
import time as tm

#time returns the current time in seconds since the Epoch. (January 1st, 1970)
tm.time()
#date.today returns the current local date.
today = dt.date.today()

#Convert the time timestamp to datetime.
dtnow = dt.datetime.fromtimestamp(tm.time())
# dtnow is: dtnow.year, dtnow.month, dtnow.day, dtnow.hour, dtnow.minute, dtnow.second # get year, month, day, etc.from a datetime

#timedelta is a duration expressing the difference between two dates:
# to create sliding window, moving window, time window
delta = dt.timedelta(days = 100) # create a timedelta of 100 days
today - delta # subtract delta from today: the date 100 days ago
today > today-delta # compare dates (returns True / False)

# change float epoch time (seconds since 1970-01-01) to timestamp
import time
time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(1482493110.0))


### Date Functionality in pandas

# Pandas has 4 main time related classes:
# Timestamp, DatetimeIndex, Period and PeriodIndex.

pd.Timestamp('2016-03-01 10:05:00')

# periods are ranges / timespans e.g. week or month
pd.Period('2016-03', 'M') # month period
pd.Period('2016-03-01', 'D') # day period

# the index of a timestamp is DatetimeIndex
# the index of a period is PeriodIndex

# time differences
# get timestamp 5 days and 3 hours earlier
pd.Timestamp('2016-03-01 10:05AM') - pd.Timedelta('5D 3H')

# get series of dates
# 10 measurements taken every two weeks on a sunday, starting on 2016-03-01
dates = pd.date_range('2016-03-01', periods = 10, freq = '2W-SUN')

# set date as index in df
df = pd.DataFrame({'Count1': np.random.randint(0,50,10),
                   'Count2': np.random.randint(0,50,10)}, index=dates)

# get info about dates
df.index.weekday_name    # monday, tuesday etc

# get last friday
from datetime import datetime
from dateutil.relativedelta import relativedelta, FR
last_friday = datetime.now() + relativedelta(weekday=FR(-1))
last_friday = last_friday.strftime('%Y%m%d') # string date

# get last/next monday (datetime assumes week starts on monday so less explicit coding is required)
import datetime
today = datetime.date.today()
# next monday
today + datetime.timedelta(days= -today.weekday(), weeks=1)
# last monday
today - datetime.timedelta(days=today.weekday())

# diff between count 1 and 2 for each date (as there are only 2 cols)
df.diff()

# mean count1 and count2 per month
df.resample('M').mean()    # index is first date from each month that appears in the dataset

# filter by date
df['2017']      # only data from 2017
df['2017-03']   # only data from march 2017
df['2017-03':]  # data from march 2017 onwards

# change frequency of date index from biweekly ('2W') to weekly ('W')
df.asfreq('W', method=ffill())   # ffill = forward-fill to fill missing values with values from the previous row


#--- different example df used from here down

# Convert a string date to a date in a pandas dataframe
df["activity_date"] = pd.to_datetime(df["activity_date"])


# Can now sort by date
df.sort_values(by="activity_date", inplace=True)

# Antonio's function to combine date, hour and minute columns into a timestamp (rounded to the nearest minute)
def extract_date_time(d0):
    return datetime.datetime.strptime(d0['date']+' '+str(d0['hour'])+' '+str(d0['minute']),'%Y-%m-%d %H %M')
# update the dataframe - first create a new column filled with zeros, then update it with the datetime stamp
views_sun_by_section['date_time'] = 0
views_sun_by_section['date_time']= views_sun_by_section[['date','hour','minute']].apply(extract_date_time,axis=1)


# Add new column for end time
from datetime import datetime, timedelta

def extract_date_time(dataframe):
    """
    :param dataframe:
    """
    return datetime.strptime(dataframe['date']+' '+str(dataframe['hour'])+' '+str(dataframe['minute']),'%Y-%m-%d %H %M') + timedelta(minutes = dataframe['width'])


def insert_end_date_time_columns(input_data_frame):
    """
    Inserts columns with date times.
    Assumes that time is in minutes.

    :param input_data_frame: pandas dataframe with day, hour and minute columns.
    :return: input_data_frame with new column containing datetime rounded to minutes.

    """

    input_data_frame['end_date_time'] = 0
    input_data_frame['end_date_time'] = input_data_frame[['date', 'hour', 'minute', 'width']].apply(extract_date_time, axis=1)

    return input_data_frame


# GET DATE OF THE LAST SATURDAY - from Edmond
import datetime
from dateutil import relativedelta
today = datetime.datetime.now()
start = today - datetime.timedelta((today.weekday() + 1) % 7)

sat = start + relativedelta.relativedelta(weekday=relativedelta.SA(-1))
last_week_sat = sat  - relativedelta.relativedelta(weeks=1)

two_years_ago = sat - relativedelta.relativedelta(years=2)
date_input_string =  two_years_ago.strftime("%Y%m%d")
last_week_sat = last_week_sat.strftime("%Y%m%d")
last_week_sat

# get date from timestamp
date_only = mydate.strftime("%Y%m%d")

# get date from datetime in a pandas series
df['just_date'] = df['dates'].dt.date

# convert string date to date type (datetime.date)
datetime.date(datetime.strptime('2018-04-17', "%Y-%m-%d"))

# filter df by date
emailed_18th = emailed.loc[emailed.date == datetime.date(datetime.strptime('2018-04-17', "%Y-%m-%d"))]



# =============================================================================
###  """ DICTIONARIES """
# =============================================================================
# some useful snippets in Kabi's python book (chapter 10)

# (implemented like hash tables)

# curly brackets
# {'hydrogen': 1, 'helium: 2}  - set of <key, value> pairs
# mutable
# d[k]  # returns the value associated with key k in dictionary d

# keys can be strings or numbers
# can contain lists
# have no specific order (as they are implemented like hash tables)

## Create an empty dictionary
menu = {}

## Add new elements using an assignment, as with lists

# each key will be a dish

# NB if Chicken Alfredo is already in the dictionary, it will UPDATE the value (here the value is the meal price)
menu['Chicken Alfredo'] = 14.50  # Adding new key-value pair
menu['Chicken Calzone'] = 12.50  # Adding new key-value pair
menu.update({'Steak': 17.00}) # alternative syntax to add new data

# add multiple values to the same key - will be saved in () brackets.
menu['Chicken Balti'] = 'Hot', 'Mild'
# returns {'Chicken Balti': ('Hot', 'Mild')}

# add multiple values to the same key, in a list.
menu['Chicken Balti'] = ['Hot', 'Mild']
# returns {'Chicken Balti': ['Hot', 'Mild']}

# another example
# dict values can be strings, numbers, booleans, lists or dictionaries
dog_dict = {'name': 'Freddie',
'age': 9,
'is_vaccinated': True,
'height': 1.1,
'birth_year': 2001,
'belongings': ['bone', 'little ball'],
'walks': {'Monday': ['12:00', '18:00'],
          'Tuesday': ['09:00', '18:00']
          }
}

## Accessing values in dictionaries
print menu # view the whole dictionary
menu.keys()      # get all the keys
menu.values()    # get all the values
menu.items()     # get all the key/value pairs
menu['Chicken Alfredo'] # view the value  for Chicken Alfredo.

# print all keys (dishes)
for key in menu:
    print(menu[key])
# print all prices
for val in menu.values()
    print(val)
# iterate over all items in dict
for key, val in menu.items():
    print(key)
    print(val)

### lists of dicts

# load csv file as a list of dicts: one dict per row (vehicle), with colnames as keys
import csv
with open('mpg.csv') as csvfile:
    list_of_dicts = list(csv.DictReader(csvfile)) # list of dicts: colnames as keys

list_of_dicts[:2] # the first 2 dicts in the list

# print colnames and values dict (row) #25
for dict in list_of_dicts[25]:
    print(dict,':', list_of_dicts[25][dict])

# access values in one column across all dicts in a list of dicts
[print(float(dict['keyname'])) for dict in list_of_dicts]

# set() - get uniwue values in a list --unique() only works for np.arrays
# unique values in one column in across all dicts: use set() - creates an unordered collection of unique elements.
unique_levels = set(dict['keyname'] for dict in list_of_dicts)



menu.clear() # delete everything from the dictionary

# Nice print out of menu contents (key-value pairs) - in ascending order
print("Dish\tPrice\n")
for key, value in sorted(menu.items()):
    print("{0}\t{1}".format(key, value))

# alternative syntax to return the value associated with a key, or "XX" if the
#   key doesn't exist:
menu.get('Chicken Alfredo', 'XX')   # returns 14.5 (as above)
menu.get('Chicken', 'XX')           # returns 'XX'


## Comparng how to access items in dictionaries, modules and variables
mystuff['apple']  # get apple from dict
mystuff.apple()  # get apple from the module
mystuff.tangerine  # same thing, it's just a variable


## Dictionaries of dictionaries
elements = {}
elements['H'] = {'name': 'Hydrogen',
                 'number' : 1,
                 'weight' : 1.00794}

# So now the dictionary elements contains a key 'H' who's value is itself a dictionary, containing the name, number and weight of the element H.

# Accessing individual keys/values in a dictionary of dictionaries:
elements['H']  # returns the vaue associated with key 'H', which is another dictionary:
               # {'name': 'Hydrogen', 'number': 1, 'weight': 1.00794}

elements['H']['name']   # returns 'Hydrogen' (looks up the key 'H' in dictionary
                        # elements, and then the key 'name' in dictionary 'H', and
                        # returns the value associated with 'name' (='Hydrogen').
# each key in elements could contain a different sized sub-dictionary



## Add more data to elements
elements['O'] = {'name': 'Oxygen',
                 'number' : 8,
                 'weight' : 15.999,
                 'melting point' : -218.8}

## Print name and atomic number from elements
for key in elements:
    print(elements[key]["name"], elements[key]["number"])

""" prints:
    Hydrogen 1
    Oxygen 8
in some order """

# print all the keys
for key in elements.keys():
    print(key)


# Another example of accessing specific values in dictionaries:

dict = {"apple":(1,2,3), "banana":(4,5,6), "carrot":(7,8,9)}

for key in dict:
    """ print every 3rd element in a tuple, for each key """
    print(key)          # print the key (name)
    print(dic[key][2])
    print("--")

    """ prints:
        carrot
        9
        --
        apple
        3
        --
        banana
        6
        --
    note dictionaries are not ordered """


# View just the rows for June 3rd (from all devices)
[i[i["date"] == "2017-06-03"] for i in categorised_anomalies.values()]



## Extract a Subset of a Dictionary
# https://www.safaribooksonline.com/library/view/python-cookbook-3rd/9781449357337/ch01s17.html

prices = {
   'ACME': 45.23,
   'AAPL': 612.78,
   'IBM': 205.55,
   'HPQ': 37.20,
   'FB': 10.75
}

# Make a dictionary of all prices over 200
p1 = { key:value for key, value in prices.items() if value > 200 }

# Make a dictionary of tech stocks
tech_names = { 'AAPL', 'IBM', 'HPQ', 'MSFT' }
p2 = { key:value for key,value in prices.items() if key in tech_names }



## Searching within dictionaries
'H' in elements  # will return True or False
'Hydrogen' in elements['H']['name']  # will return True or False

# Can use a function to search within dictionaries:
# searching through indexes that are dictionaries is faster than looping through indexes that are lists.
def add_to_index(index, keyword, url):
    if keyword in index:
        index[keyword]  # returns list of urls associated with keyword
    else:
        # not found, add new keyword to index
        index[keyword] = [url]


# Functions to search within dict of dicts:

def findmax(data_dict, feature):
    """ Search within a dict of dicts and return the MAXIMUM value
        of a given feature, e.g. "salary" """
    result = ["", 0]
    for person in data_dict:
        if data_dict[person][feature] != 'NaN':    # ignore NAs
            if data_dict[person][feature] > result[1]:
                result[0] = person
                result[1] = data_dict[person][feature]
    return result
findmax(data_dict, "exercised_stock_options")  # ['LAY KENNETH L', 34348384]

def findmin(data_dict, feature):
    """ Search within a dict of dicts and return the MINIMUM value
        of a given feature, e.g. "salary" """
    # start with the maximum value, using the findmax function above
    max = findmax(data_dict, feature)
    result = ["", max]
    for person in data_dict:
        if data_dict[person][feature] != 'NaN':    # ignore NAs
            if data_dict[person][feature] < result[1]:
                result[0] = person
                result[1] = data_dict[person][feature]
    return result
findmin(data_dict, "exercised_stock_options")  # ['BELFER ROBERT', 3285]


### Ordered dictionaries

# see: https://docs.python.org/2/library/collections.html# (python 2) / https://docs.python.org/3/library/collections.html (python 3)

from collections import OrderedDict

d = {'banana': 3, 'apple': 4, 'pear': 1, 'orange': 2}
print[i for i in d]  # does not print in (same) order

e = OrderedDict(sorted(d.items(), key=lambda t: t[0]))
print[i for i in d]  # prints in order


# ===============================================================
### """ DISPLAY - to display more columns in Pycharm """
# ===============================================================

#adjust printing widths to show more columns of output
desired_width=320
pd.set_option('display.width', desired_width)
pd.set_option('display.max_columns', 10)


# ===============================================================
### """ DISTRIBUTIONS (Numpy) """
# ===============================================================

## Measures of a distribution
# most 'common' values: mean, mode etc
# variability: standard deviation
# shape of the tails: kurtosis

## Sampling distributions
# e.g. a sample from a binomial distribution will be either 0 or 1
np.random.binomial(n, p, size)
np.random.binomial(1, 0.5)    # takes 1 sample, with a 0.5 probability of getting a zero
# 1000 coin flips: probability of getting a zero number
np.random.binomial(1000, 0.5)/1000
# 10 x 'simulations' of 1000 coin flips: probability of getting a zero number
x = np.random.binomial(1000, 0.5, 10)    # returns array of n/1000 that were zero
print((x>=500).mean())    # proportion of times when 0 was more likely than 1

# simulate the chance of having a tornado on any given day - based on 200 simulated days of data
chance_of_tornado = 0.1    # 1% chance of tornado e.g. 1 in every 10 days
tornado_events = np.random.binomial(1, chance_of_tornado, 200)
print((tornado_events>0).mean())

# other distributions
np.random.gaussian(0.75)
np.random.uniform(0,1)

## Standard deviation
distribution = np.random.normal(0.75,size=1000)
np.std(distribution)
# exact formula: (gives same result)
np.sqrt(np.sum((np.mean(distribution)-distribution)**2)/len(distribution))

## Kurtosis
# negative value: curve is flatter than a normal distribution
# positive value: curve is more peaky than a normal distribution
# the closer to 0 the better
import scipy.stats as stats
stats.kurtosis(distribution)

## Chi_squared distribution
# left-skewed, with just one parameter: degrees of freedom
# increasing df reduces the left skew (but it will never become right skewed)
# e.g...
# more left skew
chi_squared_df2 = np.random.chisquare(2, size=10000)
stats.skew(chi_squared_df2)
# slightly less left skew
chi_squared_df5 = np.random.chisquare(5, size=10000)
stats.skew(chi_squared_df5)

import matplotlib.pyplot as plt
output = plt.hist([chi_squared_df2, chi_squared_df5], bins=50, histtype='step',
                  label=['2 degrees of freedom','5 degrees of freedom'])
plt.legend(loc='upper right')

## Bimodal distributions
# have >1 peak
# can model them using two normal distributions with different parameters: Gaussian mixture models
# Gaussian mixture models are useful for clustering data



# ===============================================================
### """ EMAIL WITH ATTACHMENTS FROM PYTHON """
# ===============================================================

import os
import smtplib
import mimetypes
from email.mime.multipart import MIMEMultipart
from email import encoders
from email.message import Message
from email.mime.audio import MIMEAudio
from email.mime.base import MIMEBase
from email.mime.image import MIMEImage
from email.mime.text import MIMEText

def mail(to, subject, text, attach=None):
    """
    From https://stackoverflow.com/questions/26582811/gmail-python-multiple-attachments
    to: recipients as a string or list.
    subject: subject text
    attach: (optional) filenames to attach as a string or list. Can attach txt/csv and image files.
    """
    # allow either one recipient as string, or multiple as list
    if not isinstance(to,list):
        to = [to]

    # allow either no attachments, one attachment as string, or multiple as list
    if attach:
        if not isinstance(attach,list):
            attach = [attach]

    # Login credentials: gmail_pwd is an App password generated here: https://myaccount.google.com/apppasswords)
    gmail_user = YOUR_EMAIL_ADDRESS
    gmail_pwd = YOUR_APP_PASSWORD

    # set the msg object
    msg = MIMEMultipart()
    msg['From'] = gmail_user
    msg['To'] = ", ".join(to)
    msg['Subject'] = subject

    msg.attach(MIMEText(text))

    #get all the attachments(if any)
    if attach:
        for file in attach:
            print(file)
            part = MIMEBase('application', 'octet-stream')
            part.set_payload(open(file, 'rb').read())
            encoders.encode_base64(part)
            part.add_header('Content-Disposition', 'attachment; filename="%s"' % os.path.basename(file))
            msg.attach(part)

    # set up Gmail server
    mailServer = smtplib.SMTP("smtp.gmail.com", 587)
    mailServer.ehlo()
    # Open the connection
    mailServer.starttls()
    mailServer.ehlo()
    mailServer.login(gmail_user, gmail_pwd)
    mailServer.sendmail(gmail_user, to, msg.as_string())
    mailServer.quit()

#to execute:
#mail(to=['recipient1', 'recipient2'], subject='subject text', text='body text', attach=['attachment1.csv', 'attachment2.png'])


# ===============================================================
### """ ENVIRONMENTS """
# ===============================================================

# copied from txt file "Using conda to create and manage Python 2.7 and 3.6 environments""

# Create multiple python environments to use either Python 2 or Python 3 on the same computer.
# Anaconda works with Python 2.7 or Python 3.6
# So must use one environment to use Python 2.7 and a different one to use Python 3.6
# NOTE creating new environments takes quite a while as there are a lot of programs/files to create.

# See this website for details on how Python environments work in Anaconda:
https://conda.io/docs/py2or3.html#create-a-python-2-7-environment


### Can create/manage environments either using the cmd line or in the Anaconda Navigator.

## Instructions to create a new environment in cmd line:
<conda create -n ENVIRONAME python=2.7 anaconda>    # sets up a new environment called ENVIRONAME, that's configured to use Python 2.7 and includes all packages based on the anaconda package (=automatically installs all anaconda's dependencies, as anaconda is basically a collection of the most useful packages).
e.g.
<conda create -n py27 python=2.7 anaconda>    # creates an enviro called 'py27' that runs Python 2.7
<conda create -n py36 python=3.6 anaconda>    # creates an enviro called 'py36' that runs Python 3.6

# It's OK to use the default Path C:\Users\User\ (assuming it's been set to C:\Users\User\Anaconda3\Scripts as below)
# If the conda command is not recognised:
# Go to the ControlPanel\System\Advanced system settings\Environment Variables\Path
#   Then hit edit and add <C:\Users\User\Anaconda3\Scripts> to the end (this changes the filepath associated with 'Path' in the cmd line).
#   Then restart the cmd line.


## Instructions to create a new environment in the Anaconda Navigator:
# Start > Anaconda > Anaconda Navigator > Environments
# Click 'Create' to create a new environment: select the version of Python to use and choose an appropriate name (not easy to change it afterwards!).
# Select 'Not Installed' from the drop-down box above the right hand pane. Check the box by 'anaconda' and click Apply.
# This lists all the dependencies (packages that anaconda depends on).
# Click Apply and it will set up the new environment.


# To rename an environment, e.g. from pythonenv27 to py27, change the directory name at the file location in Windows Explorer:
C:\Users\User\Anaconda3\envs
# NOTE this doesn't seem to rename all the shortcuts to various anaconda programs, so might break the links/filepaths.
# Prob better to just name the file what you want to start with.

# Anaconda will create a shortcut to all it's files/programs (Jupyter Notebook, IPython, Navigator...) for each environment.
# So there will be a shortcut to "IPython" AND one to "IPython (py27)".

# virtual environments
# to allow different package versions for different projects


# ===============================================================
###  """ ERRORS & BUGS """
# ===============================================================

# When running commands make sure to test them thoroughly with different inputs including empty strings '' to avoid edge cases:

The 'edge case' or 'boundary case'	# situation when a variable is an empty string '' that causes an error/bug because there is not character at position 0.
# Always test how functions cope with the empty string to check they are robust.

# get current time
time.clock()
# can use this at start and end of functions to time how long they take to run

# Evaluate some code to see what it does
eval()



# ============================================================================
### """ FEATURE SCALING """
# ============================================================================

# features = of datapoints, e.g. a point's weight, height, age...
# these notes are in terms of machine learning - from Udacity mach learn lesson 10

""" When different features have vastly different scales, e.g. house age
    vs. house price, features with a larger range can have more influence on results.
    Udacity mach learn lesson 10 vid 8.

    Feature scaling rescales features between 0-1 using this formula:

    X1 = (X - Xmin) / (Xmax - Xmin)

        X1 = the new rescaled feature
        X = the value to rescale
        Xmin and Xmax = the minimum and maximum values from the whole dataset.

    Scaling can be a problem if you have outliers with extreme values.

    Rescaling is only useful for some algorithms:
    It will affect the results of algorithms like SVMs with rbf (non-linear)
    kernels, and k-means clustering, work in multiple dimensions (trade-off
    one dimension against the other).

    Scaling won't affect the results of decision trees, that use vertical/
    horizonal lines to group data (e.g. resizing an image doesn't change it's
    proportions), or linear regression, which calculates independent coefficients
    for each variable that are unrelated to each other).
"""

# Can rescale feature data in sklearn using MinMaxRescaler or StandardScaler:
#       http://scikit-learn.org/stable/modules/preprocessing.html
# the input data should be a NUMPY ARRAY of FLOATs.

# MinMaxScaler
from sklearn.preprocessing import MinMaxScaler
scaler = MinMaxScaler()
rescaled_data = scaler.fit_transform(data)
# FIT finds the xmin and xmax and TRANSFORM rescales each datapoint (can also
# implement fit & transform separately).

# Find the rescaled value of a feature with an original value of 200,000
# using the min/max in the dataset and MinMaxScaler:
from sklearn.preprocessing import MinMaxScaler
import numpy
salaries = numpy.array([[min_val],[max_val],[x_val]]) # all floats!
salaries = numpy.array([[477.],[1111258.], [200000.]])  # example
scaler = MinMaxScaler()
scaler.fit_transform(salaries)  # 200,000 is rescaled to 0.179

# StandardScaler
# to standardize features by removing the mean and scaling to unit variance.
from sklearn.preprocessing import StandardScaler
scaler = StandardScaler().fit(data)


# ============================================================================
### """ FILE INPUT/OUTPUT (I/O) """
# ============================================================================

# = reading & writing to external files

# Open a file and assign it to a variable:
variable = open("filename", "mode")
# e.g.
my_file = open("output.txt", "r")   # "r" means READ ONLY mode
my_file = open("output.txt", "w")   # "w" means WRITE mode
my_file = open("output.txt", "r+")  # "r+" means READ & WRITE mode
my_file = open("output.txt", "a")   # "a" means APPEND mode - only adds new data to current file.

# Read (print) the file
print my_file.read()  # show the whole file

print my_file.readline()  # show only one line of the file: starts from 1st line.
print my_file.readline()  # running this command again will show the 2nd line.


# Write to file
my_file.write("A string")  # write() function only takes strings
my_file.write(str(num) + "\n")  # str(num) converts num to a string. "\n" means each number is placed on a New Line, e.g. when using with a FOR: statement.

my_file.close()  # MUST close the file after writing for reading the file back, or the changes won't be saved (changes are stored in a temporary 'buffer' file that isn't refreshed until the main file is closed).

# Typical procedure for opening and writing to a file:
# 1. Set working directory where file is saved.
cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\
pwd
# 2. Open the file for reading
read_file = open("text.txt", "r")
# 3. See what's already in the file
print(read_file.read())  # initially it's empty (check in folder though)
# 4. Use a second file handler to open the file for writing
write_file = open("text.txt", "w")
# 5. Write to the file
write_file.write("Not closing files is VERY BADDD.")
# 6. CLOSE THE WRITE FILE
write_file.close()
# 7. Read from the updated file
print(read_file.read())
# 8. Then CLOSE THE READ FILE.
read_file.close()

# Using WITH and AS gets Python to close the file automatically:
with open("text.txt", "w") as my_file:  # saves text.txt as variable 'my_file'
    my_file.write("Some data")          # writes to the file & closes it.

# Check if file is closed
my_file.closed  # will return True if it's closed and False if not.


## finding files in directories

import os
# get list of files in folder
query_list = os.listdir('/home/ubuntu/Jo/package/propensity_to_buy_model/propensity_to_buy_model/separated_queries')
# select only the .txt files
query_list = [x for x in query_list if x.endswith(".txt")]
# return sorted by filename
query_list = sorted(query_list)


# get only filenames containing yesterdays date
import fnmatch
import os
yesterday = date.today() - timedelta(1)
yesterday_date = yesterday.strftime("%Y%m%d")

# path = 'propensity_scores/'
for file in os.listdir('propensity_scores/'):
    if fnmatch.fnmatch(file, ("*" + yesterday_date + "*")):
        print(file)


# Get list of files containing 'machine_feeding' and ending in '.txt'
import glob
import os
# method 1. change directory
machine_feeding_queries=[]
os.chdir('/home/supriya/01_recengine_SQL/01_SQL_stepwise_main_pipeline/')
for file in glob.glob('*machine_feeding*.txt'):
  machine_feeding_queries.append(file)
# method 2. read from path
machine_feeding_queries=[]
for file in glob.glob('/home/supriya/01_recengine_SQL/01_SQL_stepwise_main_pipeline/*machine_feeding*.txt'):
file = file.split('/')[-1]
machine_feeding_queries.append(file)


# read file from path starting from root directory
from os.path import expanduser
home = expanduser("~")
p = open(home + '/Documents/credentials/jd_redshift_credentials_dwh.txt', 'r').read()

# (because ~ wont work in open(), e.g. below)
p = open('~/Documents/credentials/jd_redshift_credentials_dwh.txt', 'r').read()


# =============================================================================
###  """ FOR LOOPS """
# =============================================================================

# These loop through lists
for <variable_name> in <list>:      # variable_name is a new name introduced in the loop
    <block>
# this syntax means that for each element in the list we assign that element to the variable_name and evaluate the block.


# Loop syntax to refer to multiple variables in a list, e.g. a list of lists
for <variable_name_1>, <variable_name_2>, <variable_name_3> in <list>:
    <block>
# variable names can be anything, (a,b,c, billy,bob,harry...). Only the order matters - as it matches the order in the list.
# Naming variables in lists at the start of the loop saves having to refer to them by index, using i[].

# E.g. to calculate total fees paid at a collection of universities
# uni_list is a list of lists, each contains the uni name, total students enrolled & tutition fees.
def total_enrollment(uni_list):
    total_students = 0                       # create empty variable for running total of students
    total_fees = 0                           # ...and for running total of fees
    for uni, students, fees in uni_list:     # alternative syntax to 'for i in uni_list:'
        total_students += students           # add number of students to the running total
        total_fees += students * fees          # add fees to running total
    return total_fees, total_students


# Return the index position of a certain value, rather than the actual value.
mylist.index(<value>)   # returns the index of the FIRST position where the value is found.
                        # returns an error if the value doesn't exist in the list.

# use enumerate() to loop through lists automatically
# optional argument 1 means start numbering the output list from 1 (default 0)
my_list = ['apple', 'banana', 'grapes', 'pear']
for i, value in enumerate(my_list, 1):
    print(i, value)

## For Else loops
# http://book.pythontips.com/en/latest/for_-_else.html
# acts like a 'then' statement, so if you dont break out of a for loop it will run whatever is in the else statement.
for item in container:
    if search_something(item):
        # Found it!
        process(item)
        break
else:
    # Didn't find anything..
    not_found_in_container()

# =============================================================================
###  """ FUNCTIONS """
# =============================================================================

## Python functions vs Python methods:
# functions work like function(object)
# contrast to methods which work like object.method()
# e.g. len(mylist) vs mylist.count('horse')

## If you update a function in a .py file use reload() in the imp module to
# refresh the function in a notebook that calls that function
from imp import reload
reload(time_series)   # reloads time_series.py without having to reload the kernel.


## Basic in-built functions, e.g. len()

len()    # length
min(1, 2, 3)    # minimum
max()    # max
abs()    # absolute value

+=      # add to (append), e.g. a list or a (running) total
-=      # remove/deduct from, e.g. a list or a total
range(start, stop, step)  # creates/refers to a list, where start defaults to 0 and step defaults to 1.
range(1, 6)  # 1,2,3,4,5 (excludes the STEP number)
range(6)  # 0,1,2,3,4,5 (excludes the START and the STEP numbers - they default to 0 and 1) - can use this to create a list from 0-10 e.g.
my_list = list(range(11))
range(1, 6, 2)  # 1,3,5 (increases in steps of 2)
# view the whole range
for i in range(1,6,2):
	print(i)

### More complex functions using def...():

# functions (procedures) take one or more INPUTS (aka 'arguments' / 'operands'), run a procedure and return one or more OUTPUTS
# standard syntax:
def function_name(input1, input2, input3):  # Must include a colon
    """ Docstring - description of what the function does / warning
    - usually split over two lines """
    output1 = input1*input2    # code to run the function/procedure (INDENTED)
    output2 = input1*input3
    return output1, output2    # return one or more outputs separated by commas


# Assign function outputs to a new variable:

# for functions with just one output:
apple = function_name(inputs)

# for a function with 2 outputs, use multiple assignment:
apple, pear = function_name(inputs) # output1 will be assigned to apple; output 2 to pear.


### Programming defensively: assert() to check assumptions in procedures/functions
# Add an assert <testExpression> inside the function code, where if the result is False this invokes a failure and stops the procedure from continuing.
# syntax
assert <testExpression> # if False the procedure is stopped and it prints "AssertionError".
# e.g.
assert date1 < date2 # to check date 1 is less than date2.
assert not date2 < date1 # check date2 is not before date1.

### Helper Procedures
# These check assumptions in code
# E.g. the helper procedure below checks whether date1 is before date2.
def dateIsBefore(year1, month1, day1, year2, month2, day2):
    """ check whether date1 is before date2 """
    if year1 < year2:
        return True
    if year1 == year2:
        if month1 < month2:
            return True
        if month1 == month2:
            return day1 < day2
    return False

### Testing procedures/functions

def test():
    """ test the daysBetweenDates and nextDay functions with various inputs """
    assert daysBetweenDates(2013, 1, 1, 2013, 1, 1) == 0    # =for this input the result should be zero
    assert daysBetweenDates(2013, 1, 1, 2013, 1, 2) == 1    # result should be 1
    assert nextDay(2013, 1, 1,) == (2013, 1, 2)             # result should be (2013, 1, 2)
    assert nextDay(2013, 4, 30,) == (2013, 5, 1)
    assert nextDay(2012, 12, 31,) == (2013, 1, 1)
    print("Tests finished.")

# run the test
test()  # if the tests all pass it returns "Tests finished". If it fails it returns a load of red error text.



###  Anonymous functions

# lambda creates anonymous functions (with no name) that still return values; useful for quick functions but not ones you'll need to use again and again.

# e.g.:
lambda x: x % 3 == 0
# Is the same as:
def by_three(x):
    """ Returns the cube of a given number """  # <-documentation string used in help
    return x % 3 == 0

# Can use lambda functions for filtering:
languages = ["HTML", "JavaScript", "Python", "Ruby"]    # list
print(filter(lambda x: x == "Python", languages))        # filters the languages list to show only items equal to "Python"
# filter() takes 2 arguments, the function that tells it what to filter (e.g. a lambda expression), and the object to perform the filtering on (list).


### Import a function from a different python code file
# set working directory
cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\
import sys
sys.path.append("./tools/")    # sys tells Python where to look; doesn't change the working directory (sometimes you still have to cd into the folder though)
from email_preprocess import preprocess   # imports the function preprocess from the file email_preprocess in the tools folder

""" NOTE if I make changes to the function in the email_preprocess file, might
    need to restart the kernel (and reload in all the data/modules) for the
    changes to be recognised. Otherwise Python will work from the cache. """


# =============================================================================
###  """ Get name of Python object
# =============================================================================

# search for the object in list of all local objects (stored as key-value pairs)
# https://www.tutorialspoint.com/How-to-get-a-variable-name-as-a-string-in-Python
# where my_object = contents of the object, e.g. “SELECT * FROM table” - only works if unique, or will return a random object name from all possibilities.
object_name = [k for k,v in locals().items() if v == my_object][0]

### Make dictionary of object names (keys) and object contents (values), e.g. query names and queries
 def get_query_list(path):
    """
    List the names of queries saved as .txt files in a directory.
    Requires os
    """
    query_list = os.listdir(path)
    query_list = [x for x in query_list if x.endswith(".txt")]
    # sort so they run in order!
    return sorted(query_list)

path = '/home/jo/projects/p2b/package/propensity_to_buy_model/propensity_to_buy_model/separated_queries/'
query_list = get_query_list(path)
query_list # e.g. [q1.txt, q2.txt, q3.txt]

# convert to dictionary
query_dict = {}
for query_name in query_list:
    query = open(path + query_name, 'r').read()
    query_dict[query_name] = query
# optional: convert to ordered dictionary so queries run in order
query_dict = OrderedDict(sorted(query_dict.items(), key=lambda x: x[0]))


# =============================================================================
###  """ HTML & BeautifulSoup
# =============================================================================

# encode a string as utf-8
views_df['content'] = views_df['content'].str.encode('utf-8')

# loops through rows in dataframe column called headline
# converts to a soup object using the lxml parser
# extracts the text and removes the html tags (supposedly) ## not sure how well this works!
for line in views_df['headline']:
    soup = bs(line, "lxml")
    line = bs.get_text(soup)
    soup.original_encoding    # to see what the original encoding of 'line' was.




# =============================================================================
###  """ Inspecting objects defined in the console / name space """
# =============================================================================

# List all defined objects in the current name space of the IPython console
dir()
# Can use the help function to find out more about any object in the name space
help(quit)  # explains how the quit function works. The info comes from the documentation string (the first string immediately below the 'def' line. e.g.:
def hello():
    """Print "Hello World" and return None"""  #  documentation string used in help
    print("Hello World")

# OR:
# Hover the cursor over an object and press Ctrl+i to view info in top right help pane

# Clear the name space
%reset

# ====================================================
#### Installing / loading packages libraries in Jupyter
# ====================================================
# useful link: http://jakevdp.github.io/blog/2017/12/05/installing-python-packages-from-jupyter/index.html
import sys
!{sys.executable} -m pip install numpy


# ================================
### Jupyter Notebook
# ================================

# https://ipython.org/ipython-doc/3/notebook/nbformat.html

""" Jupyter (né IPython) notebook files are simple JSON documents, containing
    text, source code, rich media output, and metadata. each segment of the
    document is stored in a cell.

    Useful on computers without Python or Spyder installed on the hard drive.

    Open Jupyter Notebook via the start menu or from Git Bash, but if you have
    set up different environments in Anaconda it's best to open it via Anaconda
    Navigator: right click the PLAY button on the environment and open Jupyter
    Notebook from there. Best practice is not to have multiple jupyter notebook
    terminals open/running at the same time as can cause problems.

    This opens http://localhost:8888/tree in the web browser and lists all my
    local files.

    Can drag and drop text or content from desktop programs into cells.

    Set the working directory using cd C:\User\... the same as in Spyder.

    Can tag cells with keywords (new feature April 2017: not yet searchable).
"""
# ? to get more info on functions
from scipy import stats
stats.ttest_ind?

### ipython magic functions for notebooks
%precision 2 # custom floating point precision - print to 2 d.p.
%matplotlib.inline # to plot inline

% <tab> to view available magic functions
%% to use magic in current cell only

# time code
%%timeit -n 100    # run code in cell for 100 loops to see on average how long it takes


### Use ! in Jupyter to communicate with local operating system

# e.g. view a csv file
!cat filename.csv

# view information about a function - use ?
df.fillna?



# =======================================================================
### """ LINEAR REGRESSION """
# =======================================================================
# http://www.learndatasci.com/predicting-housing-prices-linear-regression-using-python-pandas-statsmodels/

import statsmodels.api as sm
from statsmodels.formula.api import ols

# Fit linear regression model - uses R-like formula
condition_model = ols("ConditionScore ~ year_code + VehicleType + Manufacturer", data = veh_data).fit()


# Dummy encoding categorical variables using the C() function in statsmodels package

import statsmodels.formula.api as smf

condition_model = ols("ConditionScore ~ year_code + C(VehicleType)", data = veh_data).fit()

# can also use C() to change the reference level of categorical variables
condition_model = ols("ConditionScore ~ year_code + C(VehicleType, Treatment(reference='Light Goods Vehicle'))", data = veh_data).fit()


# model summary
condition_model.summary()
condition_model.params  # model coefficients
condition_model.conf_int()  # confidence intervals
condition_model.pvalues    # pvalues

# R Squared

condition_model.rsquared   # proportion of variance explained

# NOTE:
    # R-squared will always increase as you add more features to the model, even if they are unrelated to the response...

    # Selecting the model with the highest R-squared is not a reliable approach for choosing the best linear model.


## Diagnostic plots for regression

# http://www.learndatasci.com/predicting-housing-prices-linear-regression-using-python-pandas-statsmodels/

fig = plt.figure(figsize=(15,8))
fig = sm.graphics.plot_regress_exog(condition_model, "year_code", fig=fig) # year_code = predictor variable

# plot with confidence intervals
from statsmodels.sandbox.regression.predstd import wls_prediction_std
import numpy as np

x = df[['total_unemployed']] # predictor variable
y = df[['housing_price_index']] # dependent variable

# get confidence intervals
# _ is a dummy variable - we don't actually use it for plotting but need it as a placeholder since wls_prediction_std(housing_model) returns 3 values
_, confidence_interval_lower, confidence_interval_upper = wls_prediction_std(housing_model)

# select and run all the following lines at once
fig, ax = plt.subplots(figsize=(10,7))
ax.plot(x, y, 'o', label="data") # plot the dots
ax.plot(x, housing_model.fittedvalues, 'g--.', label="OLS") # plot the trend line
ax.plot(x, confidence_interval_upper, 'r--')
ax.plot(x, confidence_interval_lower, 'r--')
ax.legend(loc='best'); # plot legend


## Post-hoc tests

# http://jpktd.blogspot.co.uk/2013/03/multiple-comparison-and-tukey-hsd-or_25.html

# https://stackoverflow.com/questions/16049552/what-statistics-module-for-python-supports-one-way-anova-with-post-hoc-tests-tu (where I got the code to calculate p-values with psturng from)

# Pairwise comparisons (Tukey HSD) tests, corrected for multiple testing
from scipy import stats
from statsmodels.stats.multicomp import pairwise_tukeyhsd

pairs = pairwise_tukeyhsd(data, group) # syntax, e.g...

# see whether condition differs significantly between manufacturers
pairs = pairwise_tukeyhsd(veh_data['ConditionScore'], veh_data['Manufacturer'], alpha=0.05) # default alpha = 0.05
print(pairs)

# note statsmodels doesn't compute p-values.

# get p-values from res object
from statsmodels.stats.libqsturng import psturng
p = psturng(np.abs(res.meandiffs / res.std_pairs), len(res.groupsunique), res.df_total)
print(p)


# =============================================================================
###  """ LISTS """
# =============================================================================

# ['alpha', 23]    # list of elements
# mutable
# p[i]             # greturns the ith element of list p
# p[i] = u         # replace the value of ith element of p with u

# Denoted by square brackets [], containing a series of elements separated by commas.
# Elements can be strings, numbers, lists or expressions.

# Create a list
square_list = []    # creates an empty list called square_list.
square_list = ['Blue', 10, 900, 'Red', 'Billy Joel']    # create list containing values/elements of various data types.

# Create a list with 2 elements for use in a function
result = ["", 0]  # has a string element and an integer element
result  # returns ['', 0]
result[0], result[1] = 'a', 90  # to update the result from within the function
result  # returns ['a', 90]


### List comprehension

# list comprehension to create lists
mylist = [x ** 2 for x in range(1, 11)]  # makes a list of squared numbers (x**2) from 1 to 10 (range 1,11). Here, the list should be: [1, 4, 9, 16, 25, 36, 49, 64, 81, 100].
mylist[::-1]  # negative strides print the list backwards / traverses from right to left.

# list comprehension to compare two lists (3 examples)

""" Print all items in list_a that do not appear in list_b. """
print [item for item in list_a if item not in list_b]  # list comprehension
# the line above is equivalent to: (just puts the print statement at the start instead)
for item in list_a:
    if item not in list_b:
        print item

""" To compare two ordered lists of equal length, e.g. labels_test vs pred
    in machine learning """
len([1 for i, j in zip(list_a, list_b) if i == j]) # makes a new list with a 1 for each instance when items in list_a & list_b (at the same index position) match.
# the list comprehension above is equivalent to:
counter = 0
for i, j in zip(list_a, list_b):
    if i == j:
        counter += 1
print counter  # print the total

""" list comprehension with 2 if statements """
len([1 for i, j in zip(list_a, list_b) if i == 1.0 if i == j])
# long-hand equivalent:
counter = 0
for i, j in zip(list_a, list_b):
    if i == 1.0:   # only look at values in list_a that are equal to 1.0.
        if i == j:
            counter += 1
print counter  # print the total.

# list comprehension to make a list of lists from a DataFrame containing
list_of_latlongs = [[row['lat'],row['long']] for index, row in df.iterrows()]



# length of list
len(mylist)   # returns number of elements in the list (only counts outer elements, so in a list of two lists len = 2)

# Refer to elements in a list
square_list[0]  # refers to the first item in the list (= index #0 - counting starts at 0)

# Refer to elements in a list of lists, e.g. [['John', 1990], [['Jim', 1949], 'Sarah', 1970]
square_list[0][0]   # refers to the first element of the first list in square_list, i.e. 'John'.
square_list[0][1]   # refers to the second element of the first list in square_list, i.e. 1990.

# Create a new list containing the first character from each list (index 0) in a list of lists.
char = [row[0] for row in grid]    # here, 'grid' is a list of n lists of length n

# Sorting lists
# ascending
square_list.sort()
square_list.sort(key=lambda x: x[0])
# descending
square_list.sort(key=lambda x: x[1])

# list slicing
[start:end:stride]  # starting index = inclusive, default 0; ending index = exclusive, default end of list; stride = space between items, default 1, e.g. (::2) would select every other item in whole list).
print(mylist[2:11:])   # prints ALL numbers between index positions 2 and 10.
print(mylist[2:11:2])  # prints EVEN numbers between index positions 2 and 10.

# Combine lists - using the plus operator
# joins the contents of lists to create a new list, whereas 'append' (below) inserts a new list into a existing list.
new_list = list_1 + list_2    # concatenates list1 and list2 to make a new list (doesn't change the original two lists)
new_list = list1 + [5, 6]  # [1,2,3,4,5,6]      # concatenates the two lists

# Compare lists
## get values in list_2 that are not in list_1
main_list = np.setdiff1d(list_1, list_2)
## get values in list_2 that are also in list_1
main_list = [item for i, item in enumerate(list_2) if item in list_1]

# Compare series in pandas dataframes
# e.g. compare selected features (=true/false) between outputs from two models (default and tuned)
a[a.selected_tuned == True | (a['selected_default'] == True)] # both true
a[a.selected_default == a.selected_tuned] # true or false for both
a[a.selected_default != a.selected_tuned] # not the same


### List mutation: change/add/delete
""" lists are mutable (can be changed/appended) unlike strings, e.g. can't
    change letters in strings using string[0]='s' will cause an error; but can
    change values in lists or add new values."""

# Change values in lists
square_list[1] = 70  # changes the item at index #1 to 70.

# Add values to lists
square_list += "dog"  # Add "dog" to the list

# Append (add) new values to lists
mylist.append = <element_to_add>  # add a new element to the list
# e.g.
square_list.append("dog")  # append = add. Add "dog" to the end of the list ('append' mutates the old list, rather than creating a new list)
list1.append([7, 8])    #[1,2,3,4,5,6,[7,8]]    # inserts the new list [7,8] as an element in list1
square_list.append(2*round_list)  # append = add (not replace). This means multiply values in the round_list by 2 and add them to the square_list.


### Remove values from lists

# using the -= command
square_list -= "dog"  # Remove "dog" from the list

# using the remove command
mylist.remove()
# e.g.
mylist = ['dogs', 1, 'cats', 3, 'horses', 0, 'fish', 10, 'horses']
mylist.remove(1)  # Removes the number 1 from the list n (if it contained 1) - NOT the item at index #1

# using the Pop command .pop()
mylist.pop()    # mutates the list by removing and returning its last element (to show what it removed).
                # if the () parentheses are empty, it pops the LAST element in the list.
x = square_list.pop(1)  # removes the element at index #1 in square_list (pops it out) and saves it as 'x'

# using the Delete command del()
del(mylist[])
del(square_list[1])  # removes the item at index 1 from list n but does not return anything.

### Count number of occurences of an element in a list
mylist = ['dog', 'cat', 'horse', 'fish', 'horse']
mylist.count('horse') # number of occurences of the word 'horse'


### Completely empty a list - remove all elements from it
mylist.clear() # returns []


### Search in lists
# In
myvalue in mylist
print(3 in [1,2,3,4,5])    # will print True, as 3 appears in the list
print(6 in [1,2,3,4,5])    # will print False, as 6 does not appear in the list

# Not In
myvalue not in mylist
print(3 not in [1,2,3,4,5])    # will return False, as 3 appears in the list

# Example: check whether an element appears in a list; if yes, return the first index of the element in the list; if no, return -1.
def find_element(p, e):         # p = the list variable; e = the element variable
    if e in p:                  # uses the In command to return True/False if e appears in p or not.
        return p.index(e)       # uses the list.index() command to return the index number of e in p.
    return -1                   # otherwise (else:) returns -1. Didn't put 'else:' to reduce no. lines.
find_element([1,2,3,4,5], 3)    # 2 (index #2)
find_element([1,2,3,4,5], 6)    # -1 (not in list)

find_element(p, e)
a = [1,2,3,4,5, [6,7]]
a.index([6])



# =============================================================================
### """ MACHINE LEARNING """
# =============================================================================

# And see my notepad files in S2DS Online Course Notes folder.

# sklearn is the key module for machine learning in Python.

### Data structure for sklearn
# sklearn requires all inputs to be numeric (no text)
# and arranged in a NumPy array (it won't accept dictionaries)

""" A python dictionary can't be read directly into an sklearn classification
    or regression algorithm; instead, it needs a NUMPY ARRAY or a LIST OF
    LISTS (where each element of the list (itself a list) is a data point, and the
    elements of the smaller list are the features of that point).

    ..........................................................................
    !!! Udacity Intro to Machine Learning wrote some helper functions:
    featureFormat() and targetFeatureSplit() in tools/feature_format.py
    These functions can take a list of feature names and the data dictionary,
    and return a numpy array. Can also choose which features to extract.
    .........................................................................

    The Udacity example dict is the enron dataset: the helper functions make
    a list of people, where each person is a list of features including their
    name, email address, salary etc.

    If a feature doesn't have a value (e.g. no email available) the functions
    replace the NaN with 0. """



### SUPERVISED MACHINE LEARNING

# HOW IT WORKS
""" You subset your dataset into training and testing data, fit (train) the
    algorithm using the training data, and test it by using it to make
    predictions about the testing data.

    # The training set comprises FEATURES and LABELS (unsupervised learning has only FEATURES):
    # Features = datapoints with measurements of one or more variable, e.g. height
        (can have multiple columns, e.g. gradient, bumpiness and speed limit)
    # Labels = the response/output/result/value/target/class associated with
        each datapoint, e.g. tall/short, fast/slow.

    # You test the algorithm by giving it a set of features (heights) to predict
      the labels for, when you already know the answers, e.g. tall/short; and
      then calculate the accuracy score to show how many preds are correct.

    # More training data generally gives you a better result (higher accuracy) than
    # a fine-tuned algorithm (Udacity Lesson 6 video 3).
    # So accuracy is more influenced by the size of the training set than the parameters.
    # Being able to use more data will almost always improve algorithm performance.
"""

# For train/test split see validation section below

### Classification vs Regression
 """ Supervised learning is divided into two main categories:

# Supervised classification:
    # continuous or discrete inputs
    # discrete outputs (CLASS LABELS) - fast/slow, name, phone number, ID number
    # finds a DECISION BOUNDARY
    # evaluate using ACCURACY

# Regression:
    # continuous or discrete inputs
    # continuous outputs (NUMBERS) - continuous implies ordering, e.g. salary, height
    # finds a BEST FIT LINE
    # evaluate using the SSE or R-SQUARED
"""

### Common Supervised Classification algorithms (produce discrete outputs)

### (1) NAIVE BAYES
""" Good for text datasets, doesnt account for word order """


### (2) SVM (support vector machines)
""" More accurate than Naive Bayes
    Can produce non-linear decision boundaries
    Can be optimised using parameters including: kernels, gamma, and C
    Slow, especially for large, noisy datasets with many features (e.g. text)
    Speed up optimisation by using smaller training datasets.
    ## SVMs are called SVC in sklearn - support vector classifier """

# To take a subset of 1% of a training dataset:
features_train = features_train[:len(features_train)/100]
labels_train = labels_train[:len(labels_train)/100]


### (3) DECISION TREES
""" Allow you to ask multiple linear questions one after another.
    - Easy to use
    - Easier to interpret visually than SVMs
    - You can build bigger classifiers out of decision trees using ENSEMBLE METHODS.

    - Prone to overfitting, especially when using many features and/or small training sets.
    - Overfit decision trees would give high accuracy on training set and low on the test set.
    - Tune parameters to avoid overfitting, e.g. using min_samples_split: if nodes end up with just one
        datapoint you've almost always overfit.

    - Important to measure how well you're doing and stop the tree at the appropriate time.
    - Also play with the variance-bias tradeoff and the split criterion (entropy, gini...)
"""

# Random forests note

# can view features in order of importance (where X is the dataset of features)
print(sorted(zip(map(lambda x: round(x, 4), clf.feature_importances_), X.columns), reverse=True))
### (4) K NEAREST NEIGHBOUR (KNN)
""" Widely used classification technique
    (Can also be used for regression).
    Easy to interpret output, reasonable predictive power and low calculation
    time (compared to e.g. random forest).

    HOW IT WORKS:
    Assigns a query point to the class most common among it's k-nearest
    neighbours in the feature space.
"""

### sklearn syntax for classifier algorithms

# 1) Import the algorithm / function
        from sklearn.naive_bayes import GaussianNB    # for naive Bayes
        from sklearn import svm                       # for SVM
        from sklearn import tree                      # for decision trees
        from sklearn.neighbors import KNeighborsClassifier    # for K nearest neighbour
        from sklearn.ensemble import RandomForestClassifier
# 2) Create the classifier
        clf = GaussianNB()      # for naive Bayes
        clf = svm.SVC()         # for SVM (svms are called SVC in sklearn)
        clf = tree.DecisionTreeClassifier()     # for decision trees
        clf = KNeighborsClassifier(n_neighbors=5, weights="uniform")   # for KNN
	    clf = RandomForestClassifier(n_estimators = 500, min_samples_leaf = 50)

# 3) Train/fit the classifier using the training features/labels
        clf.fit(features_train, labels_train)   # i.e. clf.fit(X, y)

# 4) Use the classifier to make predictions about a new point/s from a test set (of data).
    # i.e. you pass in the test features and the algorithm predicts the labels.
        pred = clf.predict(features_test)

# Calculate the accuracy of predictions
from sklearn.metrics import accuracy_score    # import the accuracy function
accuracy_score(pred, labels_test)    # output is a % accuracy score (proportion
                                     # of predictions that matched the test
                                     # labels (=known/correct labels).
print clf.score(features_test, labels_test)  # another way to get the accuracy

# Can also create and train classifiers/regressions using a function
def classify(features_train, labels_train):
    from sklearn.naive_bayes import GaussianNB
    clf = LinearRegression()
    clf.fit(features_train, labels_train)
    return clf
clf = classify(features_train, labels_train)


# For feature selection: list the relative importance of each feature
# (used this command on a decision tree in C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\feature_selection\find_signature.py
clf.feature_importances_    # returns a list of importance scores for each feature


## To check for outliers / features with unusually high importance:
# Loop through the features and print only the features with importance > 0.1.
# Use a counter to get the feature number.
counter = -1
for importance in clf.feature_importances_:
    counter += 1
    if importance > 0.2:
        print "Feature number:", counter, "Importance:", importance



### A note about classification datasets
""" When generating / augmenting a dataset, be VERY careful if your data are
    coming from different sources for different classes.

    Introducing features that come from different sources depending on the
    class is a classic way to accidentally introduce biases and mistakes.

    E.g. different sources can contain missing values for different features,
    e.g. old weather datasets may be missing wind speed or have less accurate
    precipitation measures than new ones, suggesting that weather has changed
    over time (less rain or lower wind speed).
    (See note in Udacity intro to mach learn Lesson 6 quiz 35).

    One solution is only using features that appear with a similar frequency or
    accuracy in datapoints from all sources, e.g. temperature, and ignore
    features that vary with source, e.g. wind. """


### REGRESSION (produce continuous outputs)

# Output = slope * input + intercept    (y=mx+b)
# intercept=0 if line passes through the origin.

# fit and train regression
from sklearn.linear_model import LinearRegression
reg = LinearRegression()
reg.fit(features_train, labels_train)

# predictions
preds = reg.predict(features_test)  # where features_test is an array of shape = (n_samples, n_features)

# e.g. to predict the label of a new point [2, 4] (i.e. value for feature_1 = 2 and value for feature_2 = 4)
reg.predict([[2,4]])         # list of lists (one per point, despite there being only one point in this example)
# e.g. to predict the label of two new points
reg.predict([[2,4], [2,7]])
# e.g. to predict the label of a new point when the regression has only one feature, e.g. age
pred = reg.predict([[27]])
# NOTE: for regressions values to predict for must be in a list, even if there's only one value:
pred = reg.predict(27)  # won't work as 27 is not in a list.


# Evaluating regresssions
""" Assess the regression fit by:

# 1. Plotting the regression line over the scatter points
# 2. Examining some performance metrics:
     - the errors that the regression makes (measured as the sum of squared errors, see below)
     - the r-squared score (see below)
     - the regression coefficients (slope and intercept) - e.g. how they
        change when you regress on different variables/remove outliers.
"""

# Plotting regressions
plt.scatter(x,y)                # x=inputs, y=outputs
plt.plot(x, reg.predict(x))     # x=inputs, reg.predict(x)=inputs and predicted
                                # outputs from the trained regression algorithm 'reg'.
plt.show()

# e.g...
plt.scatter(ages, net_worths)                    # make a scatter plot of all data points
plt.plot(ages, reg.predict(ages), color="blue")  # draw predicted regression line
plt.show()


## The Sum of Squared Errors, SSE

""" Regression algorithms work by finding the slope and intercept that minimise
    the SSE (distance between each training point and the
    regression line, squared, and summed over all training points).

    Squared errors are used rather than absolute errors (SUM|error|) as there can
    be multiple lines that minimise SUM|error| but only one line will minimise SSE.
    (it's also computationally much easier to calc regression using SSE than SUM|error|)

    SSE is related to sample size!!
        - It increases as you add more data (each point adds to the sum of errors),
          but it doesn't necessarily mean the fit is worse.
        - This makes it hard to compare fits between different-sized datasets.
          (Udacity Intro to Mach Learn Lesson 7 video 29)

    Popular regression algorithms that minimise SSE:
        - Ordinary Least Squares (OLS) - used in sklearn LinearRegression
        - Gradient Descent
"""

## R-squared score (0-1, higher=better)

# See details on: http://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LinearRegression.html

# Indicates how much variation in the output (y) is explained by variation in the input (x) """
# Popular metric for describing the goodness of fit of a linear regression.
# Not influenced by sample size (unlike SSE), so more reliable if dataset size could change.
# Calc as 1-u/v, where u=regression sum of squares and v=residual sum of sq.

reg.score(X, y)    # X=test samples/features, y=true values for X

print "TEST SET r-sq:", reg.score(ages_test, net_worths_test)  # for the test set
print "TRAIN SET r-sq:", reg.score(ages_train, net_worths_train)  # for the training set (for comparison)

# if there is overfitting going on,r-squared will be lower for the test set than the training set.

""" Best possible score is 1.0 and it can be negative (because the model can be
    arbitrarily worse). A constant model that always predicts the expected value
    of y, disregarding the input features, would get a R^2 score of 0.0. """

# in a Udacity quiz I got scores of:
# 0.85 - they said it was very good
# 0.046 - they said it was 'low, but could be lower'.
# -1.485 - they said it was a 'pretty bad score'.


## View the regression coefficients
print "slope:", reg.coef_            # show the slope  (as an array, shape (n_features, ) or (n_targets, n_features))
print "intercept:", reg.intercept_   # show the intercept (as an array, shape (n_targets,) or (1,) or empty)

#####################################
### UNSUPERVISED MACHINE LEARNING

# The training set has only FEATURES and no LABELS
# So you have to find strucure in data without labels

# Unsupervised learning techniques are CLUSTERING and DIMENSIONALITY REDUCTION.


# CLUSTERING
# (Udacity Intro to Mach Learning: Lesson 9+10)
""" e.g. screen sizes of devices used to view a website are small
    (mobile), medium (tablet) or large (laptops/desktops). Clustering
    algorithms recognize these clumps without help.

    ### sklearn documentation on clustering
    http://scikit-learn.org/stable/modules/clustering.html

    ## Useful table comparing different scikit-learn clustering algorithms:

    # Parameters: which ones you must specify
    # Scalability: how the algorithm performs with diff-sized datasets
    # Use case: details of what kinds of data the algorithm is useful for,
    # 	e.g. k-means is good for general purpose when you have a
    # 	smallish number of clusters that contain an even number of
    # 	points.
    # Geometry: how the algorithm works, e.g. k-means works by calculating
    	the distances between points).
"""

### K-MEANS CLUSTERING

# How it works
""" 1. Assign:
     - You pick a number of clusters and where their cluster center (centroid)
        is positioned (the initial position is just rough guess).
     - Then points are assigned to the centroid they are closest to.

    2. Optimise:
     a) update the centroids:
        - reposition the centroid closer to the centre of each cluster
    	    of points, by minimising the total quadratic distance (error)
     	    between the centroid and each of the points.
     b) reassign the points:
        - to their nearest centroid: some will change clusters.

    # 3. Repeat optimisation until points stop changing clusters.

    useful website for visualising how k-means clustering works:
    https://www.naftaliharris.com/blog/visualizing-k-means-clustering/
"""

# Key parameters
""" n_clusters=8, max_iter=300 and n_init=10.

    - n_clusters is most influenctial parameter and should be optimised.
    - n_init safeguards against local minima: increase for datasets prone to
      clustering error.

    See more detailed explanations in Lesson9_KMeansClustering.py in:
        C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning
"""

# Syntax

# import the function & create the new clustering algorithm called 'kmeans'
from sklearn.cluster import KMeans
kmeans = KMeans(n_clusters=5)    # decided to use 5 clusters

# Compute cluster centers
kmeans.fit(features)  # features = numpy array of training instances to cluster

# Predict the cluster index for each sample
pred = kmeans.predict(features)

# NB: can combine these 2 steps using a convenience function:
kmeans.fit_predict(features) # equivalent to calling fit(X) followed by predict(X).



### PCA
""" Useful for dimension reduction (feature transformation to condense many features
    into fewer 'latent' features) as a form of unsupervised learning, to prepare
    data for input into an algorithm such as an SVM.

    For details see file lesson13_pca.py in:
    C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects
"""
# can use PCA as preprocessing for a SVM:
from sklearn.decomposition import PCA

# training phase
pca.fit(features_train, labels_train)   # finds the principal components
features_train_pca = pca.transform(features_train, labels_train)   # transforms the training set into a new representation where the principal components are the new features.
svc.fit(features_train_pca, labels_train)   # fit the classifier

# testing phase
features_test_pca = pca.transform(features_test)   # transform testing set using same PCs identified in the training data.
pred = svc.predict(features_test_pca)  # use these to predict labels for


### FEATURE ENGINEERING FOR MACHINE LEARNING IN SKLEARN

# sklearn cant accept string features - convert to integers using transformers

# LabelEncoder - transforms string categorical variables to integers (= still ordered)

# DictVectorizer - transforms string categorical variables to binary matrix (binary one-hot encoding. Note DictVectorizer can only transform strings; if categorical features are represented as numeric values such as int, the DictVectorizer can be followed by OneHotEncoder to complete binary one-hot encoding.

# OneHotEncoder - transforms string and/or integer categorical variables to binary matrix in a single step.


# LabelEncoder: transform categorical strings to integers

from sklearn.preprocessing import LabelEncoder

label_encoder = LabelEncoder()
views['encoded_date'] = label_encoder.fit_transform(views['date'])


# DictVectorizer - binary onehot-encode categorical string variables

from sklearn.feature_extraction import DictVectorizer

def encode_onehot(df, cols):
    """
    Apply one-hot encoding to columns specified in a pandas DataFrame.

    From: https://gist.github.com/ramhiser/982ce339d5f8c9a769a0

    Details:
    http://en.wikipedia.org/wiki/One-hot
    http://scikit-learn.org/stable/modules/generated/sklearn.preprocessing.OneHotEncoder.html

    @param df pandas DataFrame
    @param cols a list of columns to encode
    @return a DataFrame with one-hot encoding
    """
    vec = DictVectorizer()

    vec_data = pd.DataFrame(vec.fit_transform(df[cols].to_dict(orient='records')).toarray())
    vec_data.columns = vec.get_feature_names()
    vec_data.index = df.index

    df = df.drop(cols, axis=1)
    df = df.join(vec_data)

    return df

new_df = encode_onehot(df = user_profiles_complete, cols = ['mode_device', 'mode_channel', 'mode_language'])


# OneHotEncoder - binary one-hot encode both categorical and numeric features

# http://scikit-learn.org/stable/modules/generated/sklearn.preprocessing.OneHotEncoder.html
from sklearn.preprocessing import OneHotEncoder



### FEATURE SELECTION FOR MACHINE LEARNING
""" Good algorithms employ the minimum number of features required to get as much info as possible.
      (quality not quantity)

    ### Bias-variance tradeoff
    = goodness (VARIANCE, high N features) vs simplicity (BIAS, low N features) of the fit.
    - See <...Machine_Learning/ud120-projects/Bias-variance tradeoff.txt>

    ### Recommended Process for Feature Selection:
    1. Use your human intuition to choose a potential feature of interest
    2. Code it up
    3. Visualise the result (can use colour coding, e.g. colour the POIs red if you
       are trying to discriminate them from non-POIs. Look for clustering/patterns and
       decide whether the feature gives you discriminating power in the classification
       problem you are trying to solve. If yes, keep it.
    4. Repeat this process a few times to zero in on what you think will be the most
       helpful new feature for you.

    ### Reasons to exclude a feature from a model:
    - it's noisy
    - it causes overfitting
    - it's strongly correlated with another feature in the model
    - it adds complexity that slows the training/testing process (keep things simple!)

See: Udacity Intro to Mach Learning. Lesson 12: Feature Selection
and: find_signature.py in ..Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\feature_selection
"""

### Examine feature importance from models
feature_names = X.columns
importances = rf_default.feature_importances_

importances_df = pd.DataFrame([i for i in zip(feature_names, importances)],
                           columns=["features","importance"])
# view list
with pd.option_context('display.max_rows', None):
    display(importances_df.sort_values(by = "importance", ascending = False))

# plot and save (must put plt.show() after plt.savefig() to save)
sns.set(font_scale=1.3)
plt.subplots(figsize=(12,7))
top5_features = sns.barplot(x = "importance", y = "features",
                            data = importances_df.sort_values('importance', ascending=False)[0:25])
plt.tight_layout()
#plt.savefig("rf_default_feature_importances_20180326.png", dpi=500)
plt.show()


### sklearn (univariate) feature selection tools:

## 1. SelectKBest
""" Selects the K most powerful features, e.g. if you want to condense the
    features down to a specific number, and perhaps have an idea that there
    are 2 key ones driving most of the variation in the data.
    (no example here) """

## 2. SelectPercentile
""" Selects the X% most powerful features (e.g. words that best discriminate
      email authors). Example below:  """

#   import, create the selector and fit it
from sklearn.feature_selection import SelectPercentile, f_classif
selector = SelectPercentile(f_classif, percentile = 10) # select best 10% features
selector.fit(features_train, labels_train)

#   transform and save the results as new arrays
features_train_selected = selector.transform(features_train).toarray()
features_test_selected = selector.transform(features_test).toarray()



## 3. SelectFromModel to select features with importance > median (or default is > mean)
from sklearn.feature_selection import SelectFromModel

# can select best features from a prefit model using prefit=True
rf = RandomForestClassifier()
rf.fit(X_train, y_train)
rf_sfm = SelectFromModel(rf, prefit=True, threshold="median")
X_train_sfm = rf_sfm.transform(X_train) # reduce X to the selected features

# get df with feature names and whether they are selected/not
feature_choices = pd.DataFrame( {'feature': list(X_train), 'selected_default': rf_sfm.get_support()})

# view features that were not 'selected'
feature_choices[feature_choices['selected_default'] == False].reset_index()

# save the selected features in df
selected_features = feature_choices[feature_choices.selected_default == True]

# subset original df to the selected features only
mydata_sf = mydata[list(selected_features['feature'])]



""""""
### max_df in TfIdf vectorizer also does feature selection
# see ud120-projects/lesson11_text_learning.py
vectorizer = TfIdfVectorizer(max_df=1.0)
""""""

### Regularisation / Lasso Regression
""" (Udacity intro to machine learning lesson 12 video 15-16)
    - regularization AUTOMATICALLY finds the best features to include in a model.
    - works by penalising extra features to balance goodness of fit with simplicity.
    - helps stop models overfitting the training data

    # Lasso Regression: uses regularization to find the best fit
    - adds a penalty to each feature f (Pf)
    - seeks to minimise <SSE + Pf*Cf>	# Cf = coefficient of feature f
    - so adding a new feature may reduce the SSE but not enough to offset the penalty of
      increasing the model's complexity
    - this identifies the features with the most important effect on the regression.
    - and sets the coefficients for all other features to zero (so they become irrelevant to the fit).
"""
# http://scikit-learn.org/stable/modules/generated/sklearn.linear_model.Lasso.html

from sklearn.linear_model import Lasso
lasso_reg = Lasso()
lasso_reg.fit(features, labels)
lasso_reg.predict(features_test)  # e.g. .predict([[2,2,4,5,6]])

# access the coefficients
""" this command lists the coefficients for each feature. Features with
    larger coefficients are more important. Features with zero coefficients
    are not being used in the regression so can be disregarded. """
print lasso_reg.coef_

# access the intercepts (if wanted)
print lasso_reg.intercept_



### VALIDATING & EVALUATING ALGORITHMS
""" Use either train/test split or cross validation """

### Train/Test split in sklearn

# Training set size
""" # If you have too few training examples the accuracy could be too low to be useful.
    # The only way to know if you have enough data is to try the algorithm.
    # If the accuracy is too low you could collect more data, e.g. do more training
      sessions in the self-driving car.
    # Sometimes it's not possible to collect more data, e.g. the Enron dataset.
"""

# To split a dataset (array or matrix) into random training and testing subsets
import numpy as np
from sklearn.model_selection import train_test_split

X, y = features, labels

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=0)

# OR

y = veh_data.ConditionScore
feature_list = ['year_code','VehicleType','Manufacturer']
X = veh_data[feature_list]
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=42)
X_train.shape, y_train.shape, X_test.shape, y_test.shape

# returns a list containing train-test split of inputs: list length = 2*len(arrays)
""" default test_size = 0.25 (25% of data set). But can specify the test_size
    as an integer or float (= number or proportion of samples to put in the test set).

    The optional random_state parameter is a pseudo-random number generator
    state used for random sampling, e.g. random_state=42. """

# & see validation & evaluation section for K-Fold cross validation when choosing
# training and testing set sizes


### K-Fold cross validation
""" Training and testing on single datasets is prone to error if you have a
    limited training set or a limited test set.
    K-Fold Cross Validation 'folds' (splits) the data into training and testing
    sets K times, producing K different training/testing sets, which you can
    fit separate algorithms to and take an average of the output.
    This increases training time but gives a more accurate result.
"""
from sklearn.model_selection import KFold
kf = KFold(samples, k)   # samples = N items in whole dataset; k = N folds
kf = KFold(len(authors), 2)  # add parameter shuffle=True if datapoints are ordered,
""" # shuffle=true randomises the datapoints before splitting them up into folds,
      rather than splitting the data based on index value (simply half way through
      all the datapoints if you choose 2 folds). If ordered, certain labels might
      be more common in one half of the dataset, leading to different labels in
      the training and testing sets and low accuracy scores.

    # output kf is 2 lists of indices ('random' numbers between 1 and len(authors)),
      that specify which datapoints should go in the training or testing sets.
    # can use these indices to refer to features and labels by their index number
      and assign them to the training or testing sets. """


## cross_validation (like cross_validation_score but allows >1 scoring method)

from sklearn.model_selection import cross_validate
from sklearn.model_selection import ShuffleSplit # to shuffle between folds (for ordered data) - note that random splits dont guarantee that all folds will be different, but this is still likely for large datasets

rf = RandomForestClassifier(n_estimators=50)
scorers = ['accuracy', 'roc_auc']
cv = ShuffleSplit(n_splits=3, test_size=0.25)
scores = cross_validate(rf, X, y, cv=cv, scoring=scorers, return_train_score=True)
scores # dict of train and test scores and times for each scorer

# get mean and SD scores
print('Accuracy \ntrain: ', round(scores['train_accuracy'].mean(),5), '+/-', round(scores['train_accuracy'].std(),5))
print('test: ', round(scores['test_accuracy'].mean(),5), '+/-', round(scores['test_accuracy'].std(),5))



### GridSearchCV
""" GridSearchCV is a way of systematically working through multiple
    combinations of parameter tunes (values), cross-validating as it goes to determine
    which tune gives the best algorithm performance. This saves you tuning the parameters
    yourself by trying and testing your best guesses (which takes ages). """

# http://scikit-learn.org/stable/modules/generated/sklearn.model_selection.GridSearchCV.html#sklearn.model_selection.GridSearchCV

from sklearn.model_selection import GridSearchCV
from sklearn import svm, datasets

iris = datasets.load_iris()   # get the data

svr = svm.SVC()  # specify which type of algorithm to use (here it's SVM)
parameters = {'kernel':('linear', 'rbf'), 'C':[1, 10]}  # dictionary of parameters and possible values, e.g. here they are choosing between linear & rbf kernel, and diff values of C.

# create the classifier, which for GridSearchCV is the algorithm PLUS a grid of parameters to try
clf = GridSearchCV(svr, parameters)  # the parameter grid contains all possible combinations of values for each parameter, i.e. ('linear', 1),('linear',10),('rbf',1),('rbf',10)

# train it
clf.fit(iris.data, iris.target) # fit() tries each parameter combo, assesses the performance and returns the fitted classifier with the optimal parameter combo.

# access the parameter values
clf.best_params_

# photobox interview task example
from sklearn.linear_model import LogisticRegression
lr = LogisticRegression()
param_grid = {'C': [0.001, 0.01, 0.1, 1, 10]}
lr_mod = GridSearchCV(lr, param_grid)
lr_mod.fit(X_train, y_train)
print("best params: ", lr_mod.best_params_, "\naccuracy score: ", accuracy_score(y_test, lr_mod.predict(X_test)))

### Accuracy Score
""" accuracy = [N items (datapoints) in a class that are labelled correctly /
                     N items in that class]

    Accuracy is less reliable for skewed data, i.e. with imbalanced classes
    like the Enron dataset (many more non-POIs than POIs), as you can just
    guess the more common class label for every point AND STILL GET PRETTY
    GOOD ACCURACY!

    Accuracy is also less reliable if you want to err on the side of caution
    of a yes/no answer, e.g. cancer tests - prefer a false positive than a false neg.
    ### Alternatives to accuracy for unbalanced data

    # From this very GOOD BLOG POST: https://machinelearningmastery.com/tactics-to-combat-imbalanced-classes-in-your-machine-learning-dataset/
        - get more data
        - use different metrics, e.g.
            - confusion matrix, precision, recall and F1 score
            - Kappa - accuracy normalised by the imbalance of classes in the data
            - ROC curves: divides accuracy into sensitivity/specificity so models can be chosen based on the balance thresholds of these values.
        - resample:
            - oversampling: for smallish datasets add copies of instances from the under-represented class (aka sampling-with-replacement)
            - undersampling: for large datasets (tens- or hundreds of thousands of instances or more) delete instances of the over-represented class.
        - generate syntheric samples
        - try other algorithms
        - try penalised models
        - try another approach e.g. anomaly detection or change detection
        - decompose large class into smaller classes
"""
from sklearn.metrics import accuracy_score
accuracy_score(pred, labels_test)
clf.score(features_test, labels_test)

# compare training and testing accuracy to check for overfitting
print('training accuracy: ', clf.score(X_train, y_train), '\ntesting accuracy: ', clf.score(X_test, y_test))


## Confusion matrix
""" shows N datapoints predicted to be in each class and whether the prediction
    was correct (+ve) or incorrect (-ve), as a matrix.
    So 2x2 matrix if there are 2 classes (+ve/-ve for points predicted in class 1;
    +ve/-ve for points predicted in class 2) - see Udacity Mach Learn Less 15 Vid 6-9.
"""
from sklearn.metrics import confusion_matrix
y_pred = clf.predict(X_test)  # X = features, y = labels

print(confusion_matrix(y_test, y_pred, labels=range(n_classes)))  # n_classes = N unique labels/classes, e.g 7 people (see lesson13_pca.py code)

""" e.g. confusion matrix
         Ariel  [13  4   1]
         George [0  10   6]
         Donald [0   7  19]

    true/real labels are on LHS and predicted labels on RHS, in the same order.

    rows = the true positive (tp) and any false negatives (fn)
    cols = the true positive (tp) and any false positives (fp)

    # interpretation:
        - 13 photos of Ariel were correctly identified as Ariel, but 4 were mistaken for George
        - 10 photos of George were correctly identified, but 6 were mistaken for Donald.
        - there were 18 photos of Ariel (sum of first row)
        - 21 photos were predicted to be George (sum of 2nd column) """


### Precision & Recall

#  calculated from the confusion matrix above

""" Should both be at least 0.3 (udacity https://review.udacity.com/#!/rubrics/27/view)

    tp=true positives, fp=false positives, fn = false negatives.

    # PRECISION rate = tp / (tp + fp) = 10/21 (21 is the col sum),
        i.e. probability that a photo *identified as* George is actually George.

    # RECALL rate = tp / (tp + fn) = 10/16 (16 is the row sum),
        i.e. probability that a photo *of* George is identified as George, or
             the ability of the classifier to find all the positive samples.

    # High recall/low precision = high true positives, but risk of false positives.
        e.g. "Nearly every time a POI shows up in my test set, I am able to
        identify him or her. The cost of this is that I sometimes get some
        false positives, where non-POIs get flagged."

    # High precision/low recall = low false positives, risk of false negatives.
        e.g. "Whenever a POI gets flagged in my test set, I know with a lot of
        confidence that its very likely to be a real POI and not a false alarm.
        On the other hand, the price I pay for this is that I sometimes miss
        real POIs, since Im effectively reluctant to pull the trigger on edge cases.

    # Depends on project aims as to whether you want high precision or high recall.
      For Enron we want high recall/low precision as don't want to miss any POIs (false negs).

    # A high F1-score represents a good balance between recall and precision,
        so fp and fn are low and predictions are accurate.
"""
from sklearn.metrics import precision_score
precision = precision_score(y_true, y_pred)  # y=labels

from sklearn.metrics import recall_score
recall = recall_score(y_true, y_pred)


## The F1-score (0-1)
""" # the balance between (weighted average of) RECALL and PRECISION

    # a measure of the classifier's performance: higher is better.

    # can be used to monitor how changing/tuning parameters affect the
      accuracy / performance of an algorithm.

    # https://en.wikipedia.org/wiki/F1_score
"""

# view the F1-score in the classification report:

## Classification report
""" shows precision, recall, F1-score snd support for each classification """
from sklearn.metrics import classification_report
y_pred = clf.predict(X_test)  # X = features, y = labels
print(classification_report(y_test, y_pred, target_names=target_names))



# =============================================================================
###  """ MISSING VALUES """
# =============================================================================

# python assigns empty values as NaN or None type values depending on their context:
# NaN (not a number, a numeric type, e.g. empty value in list of numbers)
# None (object type, e.g. empty value in list of strings)
# NaN != None

# Drops rows that contain NaN in ANY field (sklearn models do not work with null values!)
cleaned_wanted_columns = wanted_columns.dropna()

# count NAs (null values) in a dataframe
df.apply(lambda x: sum(x.isnull()),axis=0) # isnull() returns 1 if the value is null.


# Replace nulls (NaN) with specific values...
df.fillna?

# replace NAs with the mean
df['LoanAmount'].fillna(df['LoanAmount'].mean(), inplace = True)

# replace NAs with 'No'
df['Self_Employed'].fillna('No', inplace = True) # inplace means actually change the contents of df.

# forward filling / backwards filling - fill NaNs based on value in neighbouring rows
# requires data to be sorted first!
df = df.sort_index()
df.fillna(method=ffill())   # ffill = forward-fill to fill missing values with values from the previous row



# =============================================================================
###  """ NUMPY """
# =============================================================================
# https://docs.scipy.org/doc/numpy-dev/user/quickstart.html

# numpy array class is called ndarray.

# get array of numbers from 1 to length of df, and then shuffle these indexes
idx = np.arange(len(X_test_np))
np.random.shuffle(idx)

# convert pandas df to np array
X_test_np = X_test.values

# Create numpy array containing samples from a normal distribution (mean=0.0, SD=1.0)
# https://docs.scipy.org/doc/numpy-1.13.0/reference/generated/numpy.random.normal.html
np.random.normal(loc=0.0, scale=1.0, size=None) # loc = mean, scale = SD

np.random.randint(0, 100, (3,4))) # 2d array of random numbers between 0-100


# Basic NumPy commands
ndarray.ndim    # number of axes (dimensions) of the array, e.g. arrayname.ndim
ndarray.shape   # dimensions of the array
                # For a matrix with n rows and m columns, shape will be (n,m)

np.reshape(a, newshape)  # reshape an array into a new shape without changing its data.
                            # a = array to be reshaped
                            # newshape = tuple of integers: (n_rows, n_columns).
                            # If newshape is just one integer then the result
                            # will be a 1D array of that length.

# make sequence with set number of evenly-spaced markers
np.linespace(0, 5, 9) # 9 numbers between 0 and 5

# to copy an array
a2 = a.copy()
# if don't use .copy() then any changes to the new array are also made to the original!!

# iterate over arrays
for i, row in enumerate(test):
    print("row", i, "is", row)

# iterate over multiple arrays: use zip()
for i, j in zip(test1, test2):
    print(i+j)

# access values in np arrays
#print every yth element from the list / array
# e.g. every 3rd element
test2 = np.random.randint(0, 100, (3,4))
print(test2[::3]) # iterate over rows (same as test2[::3,:]), starting from index 0 of each column
print(test2[:,::3]) # iterate over columns, starting from index 0 of each row
test2[0:3, ::3] # print every 3rd number in rows 0-3

# select the 100th index from the shuffled array
instance = X_test_np[idx][100:101]

# flatten the 2d array into a 1d array and print every 7th number
a = test2.reshape(12)
a[::7]


### np.corrcoef

# interpreting np.corrcoef
# np.corrcoef returns a correlation matrix, where position:
# [0,0] is the correlation between variable x and itself (x,x), which will be 1.
# [1,1] is the correlation between variable y and itself (y,y), which will be 1.
# [0,1] is the correlation between variable x and y (x,y), which is the value of interest (and the same as [1,0])
np.corrcoef(df['converted'], df['lifetime_num_orders'])[0,1]


### Modify a sequence in-place by shuffling its contents.
arr = np.arange(10)
np.random.shuffle(arr)


# =============================================================================
### """ OUTLIERS """
# =============================================================================
 """ See Udacity intro to machine learning: Lesson 8 Outliers.

 To identify outliers:
    # Plot the raw data as a scatterplot and use the raw data sheets to identify unusual values.
    # Fit regression and plot scatterplot with reg line to check the fit visually
    # Also check the r-squared value
        - If there are outliers decide whether to accept (fraud etc) or reject them.

 OUTLIER REJECTION
    # Method for outlier detection in machine learning regression:
    1. Train the regression algorithm (with the training dataset).
    2. Remove the 10% values with the highest residual errors (distance from reg line)
    3. Re-format the data (if necessary) and re-train the algorithm on the
        reduced training set. The R-Sq should be higher and the line should fit better..
    4. This train-remove-retrain procedure can be repeated.

    # Function I wrote to clean data by removing values with large errors:
        function outlierCleaner in the file outlier_cleaner.py in
        C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\outliers

    # For a small number of known data points, use dictionary.pop('KEY', 0) to
    remove a key-value pair from a dictionary:
        dictionary.pop(key, 0)

    # Udacity code to re-train the regression using the cleaned data in the
        file outlier_removal_regression.py in:
        C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\outliers
"""



# =============================================================================
###  """ PACKAGE MANAGEMENT """
# =============================================================================

# A module is a .py file containing Python definitions and statements.

# First install module via terminal using conda or pip
# advice: https://www.quora.com/How-do-I-install-Python-packages-in-Anaconda

# Then import the module or specific functions from it into Python

###

# Installing modules

# If you want to use pip: in Anaconda Command Prompt, cd to C:\Anaconda\Scripts, and use pip from there.

# To resolve difficulty importing certain modules in bash, do this in python:
import pip
pip.main(['install', "pytrends"])



# Download only the packages you want with the conda command (in Git Bash, not Spyder)
conda install PACKAGENAME
conda remove PACKAGENAME # to remove/uninstall it
# e.g. conda install pyflux

# can use pip instead of conda, but pip installs packages in a different place to conda, so can cause problems when trying to import them in python (py looks in the wrong place. Doesn't cause probs for all packages though, just textacy & spacy so far!)
pip install PACKAGENAME
pip uninstall PACKAGENAME


# Import module into specific python environment
# open cmd via the Anaconda Navigator (for common/official modules), by clicking play next to the required environment
# or type
conda install --name py36 numpy
# for info see: https://conda.io/docs/user-guide/tasks/manage-pkgs.html

# NOTE for reference
# module cv2 = opencv


### Using conda to manage packages in Python (copied from txt file with same name)

# Conda is the package manager for Python in Anaconda
# pip is the package manager for Python if don't have Anaconda installed
# Use conda (and pip) commands on the cmd line

# To install a new package:
conda install PACKAGENAME		# enter this on the command line, not in Spyder/Anaconda.
e.g. conda install pyflux

# If it says 'invalid syntax' or the command is not recognised then conda needs updating.

# Conda must be updated via the cmd line using:
>>> conda update conda

# If this command is not recognised:
# Go to the ControlPanel\System\Advanced system settings\Environment Variables\Path
#   Then hit edit and add <C:\Users\User\Anaconda3\Scripts> to the end (this changes the filepath associated with 'Path' in the cmd line).
# Note the new path on S2DS laptop was: C:\Users\Client\Anaconda2\Scripts
#   Then restart the cmd line.
# I found these instructions on https://stackoverflow.com/questions/44515769/conda-is-not-recognized-as-internal-or-external-command.



### Importing modules into Python

# import entire module
import math
# possible conflicting function names between modules, so must type math.function to specify which module a function is from, e.g.
math.sqrt() # square root function from math module

# Better to import specific functions only

# See all functions contained in a module:
import math             # Imports the math module
everything = dir(math)  # Sets everything to a list of things from math
print(everything)        # Prints the names of all the functions.

from math import sqrt # import a specific function
from math import sqrt, log # import 2 functions


# View module version
import numpy as np
np.__version__

# can also do this in git bash using pip
pip show packagename  # shows location, version and dependancies/dependents



# =============================================================================
### """ PANDAS """
# =============================================================================

# amazing documentation: http://pandas.pydata.org/pandas-docs/stable/index.html
# pandas for data science tricks: https://medium.com/towards-data-science/pandas-tips-and-tricks-33bcc8a40bb9

import pandas as pd

# make empty pd dataframe (axis 1 = columns, axis 0 = rows)
df = pd.DataFrame([])

# combine 2 series s1-3 into a dataframe (e.g. 3 dictionaries each with one value per key, which will convert into
# one series per row) and label the rows as A, B and C
df = pd.DataFrame([s1, s2, s3], index=['A', 'B', 'C'])

# make a df from a Dictionary (will also work without .from_dict() but good practice to be specific)
df = pd.DataFrame.from_dict({'a': [1,2,3], 'b': ['red', 'blue', 'red']})

# import a csv file as a pandas dataframe
views_sun = pd.read_csv('file.csv', index_col=0) # tells pandas which column to index by

# Read dataframe with specific column data types (dont have to specify type of every column)
 visitor_profiles = pd.read_csv('visitor_profiles_for_2017-05-01.csv',
				dtype = {'converted': int,
					 'fullvisitorid': str,
					 'member_id': str})

# Import all files in a directory/folder and concat into single df (assuming they all have the same columns)
import os, glob, pandas as pd
visitor_profiles = pd.concat(map(pd.read_csv, glob.glob(os.path.join('', "user_profile_csvs/training_data/*.csv"))))

# import an xls/xlsx file as a pd df - can specify sheet if you know which sheet you want
mydata = pd.read_excel('test.xlsx')
mydata = pd.read_excel('test.xlsx', sheet_name = 0) # default sheet_name=0 and imports the first sheet

# to print the sheet names, first import as xl object and then convert to df
xl = pd.ExcelFile('test.xlsx')
print(xl.sheet_names)
df1 = xl.parse('Sheet1')

#----

### To write pandas dfs to excel files
from openpyxl import load_workbook

## Create a brand new excel file

# Specify a writer
writer = pd.ExcelWriter('example.xlsx', engine = 'openpyxl')

# Write your DataFrame to a file (in a tab labelled 'mydata')
mydata.to_excel(writer, 'mydata')

# Save the result
writer.save()

# load it
mydata = pd.read_excel('example.xlsx', sheet_name='mydata')

#----

## Add a new sheet to an existing excel file

# load the existing file
book = load_workbook('example.xlsx')

# make a new writer
#(optionally with the same name as existing file, prob safer to add suffix to prevent accidental file replacement!)
writer = pd.ExcelWriter('example.xlsx', engine = 'openpyxl')

# IMPORTANT - add the existing data to the writer so it's not overwritten!
writer.book = book

# get the new df to add to the excel file
import numpy as np
newdata = np.random.randn(100, 2)
newdata_df = pd.DataFrame(newdata)

# write the new data to a new sheet
newdata_df.to_excel(writer, sheet_name = 'mynewdata')

# save and close
writer.save()
writer.close()

# load each sheet
mydata_original_sheet = pd.read_excel('example.xlsx', sheet_name='mydata')
mydata_new_sheet = pd.read_excel('example.xlsx', sheet_name='mynewdata')

#----

## data types

# view data type of object (not specific to pandas)
type(df)

# view data types -like str(mydata)
df.dtypes
df['column'].dtype

# change dtype (don't have to specify every cat in the categories list if not all cats are required in output)
df['Grades'] = ['A', 'A-', 'B', 'B+', 'C']
grades = df['Grades'].astype('category', categories = ['A', 'B', 'C'], ordered=True)

# change floats to binary
threshold = 1.0
views['attribute_1_bin'] = np.where(views['attribute_1'] > threshold, 1,0)

### cut
# converts continuous data to categorical data
# by grouping data into buckets or bins; produces interval data, with equal spacing between categories.
s = pd.Series([168, 180, 174, 190, 170, 185, 179, 181, 175, 169, 182, 177, 180, 171])
pd.cut(s, 3)    # groups scores into 3 equally-spaced bins (labelled with the bin range)

#----

## transpose a df - turns colnames into indices to use .loc method for subsetting
df.T

## copy dataframe and separate predictors and response
X = veh_data.copy()
y = X.pop('ConditionScore')


## Pandas Shift
# shifts the rows (or columns) up or down by x amount
# get date from next/last 'period' in sequence (assumes df is in order), for each customer
# can integrate with time module, e.g. to use timedelta
df['NEXT_TIMESTAMP'] = df.groupby('USER_ID')['TIMESTAMP'].shift(periods = -1)
df['PREVIOUS_TIMESTAMP'] = df.groupby('USER_ID')['TIMESTAMP'].shift(periods = 1)



#----

## Remove NaNs

# drop rows with NaN in ANY column
views_notnans = views.dropna(axis=0)

# drop rows with NaN in a specific column only
notnans = views['attribute_1'].notnull()
views_notnans = views[notnans]

# alternatively, view only rows with NaN in this column
views_nans = views.loc[ ~ notnans].copy()

#----

### Indexing dfs

# copy current index to a new column
df['country'] = df.index

# set the index to the entries in the member_id column
df = df.set_index('member_id')
# note in jupyter the df.head() output will show a new blank row for member_id
# at the top of the df: this isnt really part of the df but is jupyter's way of
# showing that the index has a name (here it'll be member_id').

# reset the index to autonumbers (this also promotes the current index into a new column)
df = df.reset_index()

# multi-level index (hierarchical index) - add new index level to existing index
df = df.set_index([df.index, 'Name']) # add a Name index (from the 'Name' column) to the existing index
df.index.names = ['Location', 'Name'] # rename the existing index as 'Location'

# multi-level index (hierarchical index) - from scratch
df = df.set_index(['member_id', 'country'])

#----

# sort pandas df by date, hour and minute and reset the index column to save the order.
views_sun = views_sun.sort_values(by=['date','hour','minute'])
views_sun = views_sun.reset_index(drop=True)


### Pandas series have values in rows and label/index as row names: can be unique or not unique
# appending new values to a series (doesnt append in place, so save as a new series object!)
s2 = s.append([1])

# access value in first row of first column
df.iat[0,0] == 0



### Indexing operators: .loc and .iloc for row-based querying, [] for column-based querying

# iloc[], for querying based on index position
# loc[], for querying rows based on label
# .iloc and .loc can take up to two inputs, the row index and a list of colnames (df.loc[(row), (col)])

# series
purchase_1.iloc(3) = np.nan    # same as purchase_1[3]
# df
df.iloc[0:2] = np.nan    # marks cells in first 3 rows as NaN


## .loc to query by label

# series
df.loc['Store 1']    # select whole row

# df
df['Cost']    # select all rows in column
df.loc[:, 'Cost']    # select all rows in column more explictly using .loc[row, column]
df.loc[:, ['Name', 'Cost']]    # select all rows in both the Name and Cost columns

# list indices: view all items purchased from Store 1
df.loc['Store 1', 'Item Purchased']

# use two inputs to select rows AND columns
df.iloc(3)['Cost']    # get value in row 3 in the 'Cost' column
# same as...
df[3]['Cost']
# note this is CHAINING, and is risky as causes pandas to return a copy of the df rather than a view OF the df
# AVOID CHAINING


# using .iloc and .loc attributes tells pandas more explicitly what to do:
# if the index you're querying by is a LIST of integers (instead of just one) pandas can't determine if you're intending
# to query by index or label, so you get an error.

# .loc to add a new row
# if you reference a value that doesn't exist in the series or df, a new row is created with that value, e.g.
s = pd.Series = ([1,2,3])
# series values are [1,2,3] and rownames are [0,1,2] (the indexes)
s.loc['Animal'] = 'bear'
# series values are now [1,2,3,'bear'] and rownames are [0,1,2,'Animal']

# add new row to pandas df
df.index.names = ['Location']
df = df.append(pd.Series(data={'Cost': 3.00, 'Item Purchased': 'Kitty Food'},
                         name=('Store 2')))

# add new row to pandas df with hierarchical multi index
df.index.names = ['Location', 'Name']
df = df.append(pd.Series(data={'Cost': 3.00, 'Item Purchased': 'Kitty Food'},
                         name=('Store 2', 'Kevyn')))

# add new column (with partial data) to df
# first ensure you know what the index is
df = df.reset_index()
# populate rows for indexes 0 and 2 but add None (automatically) for index 1
df['newcol'] = pd.Series({0: 'dog', 2: 'cat'})
# (alternative is make a list with same length as df)



### Drop function - doesn't work in place; returns a copy of the df

# drop rows from a df
df = df.drop('row_name', axis = 0)    # note axis=0 by default, so must state axis=1 for column drops to work!

# drop columns from a df
df = df.drop('col_name', axis = 1)

# can also delete columns with del
del df['col_name']    # happens in place and doesn't return a view of the df!


## reduce number in col by 20% (in place) - and will modify the original df too unless you make a .copy()
df['Cost'] *= 0.8    # changes values in df

cost = df['Cost']
cost *= 0.8    # also changes values in df['Cost']

cost = df['Cost'].copy()
cost *= 0.8    # does not change values in df['Cost']



### Join or Concatenate 2 dfs
frames = [ds_content, sa_content]
content_consumption = pd.concat(frames, keys=['ds', 'sa'], axis = 0) # keys are like a new index
# count entries for each user group
print("ds: ", len(content_consumption.loc['ds']), "\nsa: ", len(content_consumption.loc['sa'])) # loc refers to named indexes, iloc refers to numbered indexes


## join 2 dfs
# join types = outer, inner, left, right

# to join dfs on their indices: specify left_index=True, right_index=True
pd.merge(df1, df2, how="outer", left_index=True, right_index=True)

# join dfs on specific columns
pd.merge(df1, df2, how="left", left_on='species_id', right_on='species_id')

# join on multiple columns
pd.merge(df1, df2, how="left", left_on=['species_id', 'animal_id'], right_on=['species_id', 'animal_id'])

# join on index in one df and column in the other
pd.merge(df1, df2, how="left", left_index=True, right_on='species_id')

pd.join(df1, df2)



### Querying / subsetting dfs - pandas uses Boolean Masks

# To subset a df, Pandas first creates a boolean mask (series of True/False
# for whether each value meets a given criterion) and overlays this on the
# original series via the where clause: this returns a df of the same shape
# but with NaN in place of values that didn't meet the criterion
# (can then exclude NaNs from subsequent analyses)
# E.g. get rows with speed over 20mph only
df_over20 = df.where[df['speed'] > 20]
df_over20 = df_over20.dropna(axis=0) # drop rows containing NaN in any column (change axis=1 to drop columns)
# same as:
df_over20 = df[df['speed'] > 20]

# filter out / remove rows with a content_id containing " "
views_df = views_df[views_df['content_id'].str.contains(' ') == False]

### to query on multiple conditions: make multiple boolean masks
# encase each mask in parentheses!
df_over20_under40 = df[(df['speed'] > 20) & (df['speed'] < 40)]
df_prints = df[(df['first_prod'] == 'print') | (df['second_prod'] == 'print')]
# get just the member_ids of those customers
prints_members = df['member_id'][df['first_prod'] == 'print']

# keep key columns
cols_to_keep = ['member_id', 'country', 'device']
df = df[cols_to_keep]


# query dfs with multi-level indexes
# e.g. for find records for bristol in a df indexed by country AND county
# specify both country and county in the filtering list
df = df.set_index(['country', 'town'])
df_bristol = df.loc['UK', 'Bristol']

# compare two towns: pass list of tuples as the filtering list
df_bristol_birm = df.loc[('UK', 'Bristol'), ('UK', 'Birmingham')]


### summarising series

# max speed (numeric output)
max_speed = df['speed'].max()

# row with highest speed
answer = df[df['speed'] == df['speed'].max()]



### Group By & Aggregation

# info: https://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.aggregate.html
# http://nbviewer.jupyter.org/github/jvns/pandas-cookbook/blob/v0.1/cookbook/Chapter%204%20-%20Find%20out%20on%20which%20weekday%20people%20bike%20the%20most%20with%20groupby%20and%20aggregate.ipynb

# splits df into chunks and returns a df.groupby object (that can be iterated upon)
# and a tuple of 2 objects: 2. the group condition, 2. the df reduced by that grouping.

# output is:
# a pandas.core.groupby.DataFrame object if you group by >1 column
# or a pandas.core.groupby.Series object if you only group by one column
# these types behave differently when using with .agg function

# uses a split-apply-combine pattern to split the data, apply a func and return the combined results.
# e.g.
for animal, frame in df.groupby('animal_id'):
    avg = np.average(frame['n_visits'])
    print('Fox' + animal + 'made on average' + str(avg) + 'visits')

# use level to group by the index
df.groupby(level=0)


# handy usecase: split a large df into groups to distribute processing between PCs / users

# first set index !
df = df.set_index('surname')

# assign surnames A-M as group 1 and N-Z as group 2.
def fun(item):
    if item[0] < 'M':
        return 1
    else:
        return 2

# split the df into groups
for group, frame in df.groupby(fun):
    print('There are ' + str(len(frame))) + 'records in group' + str(group))


## .agg function
# applies a function(s) to column(s) of data in the group and returns the results
# .agg requires a dict of colnames and the function you want to apply

# e.g. to return mean pop per country (for a df with those columns)
df.groupby('country')['population'].agg({'avg': np.average, 'sum': np.sum})
# returns 1 col with avg  and 1 with sum

df.groupby('country')['pop_2010']['pop_2011'].agg({'avg': np.average})
# returns two nested cols with avg pop 2010 and avg pop 2011

# pass same colname to the dict to overwrite the existing column with the aggregated values
 df.groupby('country')['pop_2010'].agg({'pop_2010': np.average})


# to perform calcs on a df grouped by >1 column
df.groupby('country')['population'].agg({'avg': np.average})



# calc total weight per category (weight and quantity are the input cols)
df.groupby('Category').apply(lambda df, a, b: sum(df[a] * df[b]), 'Weight', 'Quantity')
# same as:
def totalweight(df, w, q):
    return sum(df[w] * df[q])
df.groupby('Category').apply(totalweight, 'Weight', 'Quantity')



# Examine mean condition per vehicle type

# copy dataframe and separate predictors and response
veh_data['ConditionScore'].groupby(veh_data['VehicleType']).mean()
# same as
veh_data.groupby('VehicleType').['ConditionScore'].mean()

# count users per visit date
# https://stackoverflow.com/questions/19384532/how-to-count-number-of-rows-in-a-group-in-pandas-group-by-object
visitor_profiles.groupby(['visit_date']).size().reset_index(name='num_users')


###  Access particular rows or values in pandas objects using indexes

# make pandas objects with explicit indexes (rownames) - if not specified pandas uses autonumber as index

# series
purchase_1 = pd.Series({'Name': 'Chris',
                        'Item Purchased': 'Dog Food',
                        'Cost': 22.50})
purchase_2 = pd.Series({'Name': 'Kevyn',
                        'Item Purchased': 'Kitty Litter',
                        'Cost': 2.50})
purchase_3 = pd.Series({'Name': 'Vinod',
                        'Item Purchased': 'Bird Seed',
                        'Cost': 5.00})
# df
df = pd.DataFrame([purchase_1, purchase_2, purchase_3], index=['Store 1', 'Store 1', 'Store 2'])



# aggregate (calc sum and min) across columns
df.agg(['sum', 'min'])

# aggregate different functions across different columns (and put NaN in any resulting empty cells)
df.agg({'A' : ['sum', 'min'], 'B' : ['min', 'max']})

# group by and aggregate in same line - sums the number of bike rides per day of week
weekday_counts = berri_bikes.groupby('weekday').aggregate(sum)

# multiple aggregate functions
# https://www.shanelynn.ie/summarising-aggregation-and-grouping-data-in-python-pandas/
mean_daily_consumption = df.groupby(['day']).agg({'kwh': [min, max, sum, "mean", "std", "mad"]})

# Use ravel to create better names for the columns when using multiple aggregates
mean_daily_consumption.columns = ["_".join(x) for x in mean_daily_consumption.columns.ravel()]


## Get counts of unique values

# N unique vehicle IDs per year (ordered by Financial Year)
df.groupby('FinancialYear').VehicleID.nunique()
df['FinancialYear'].value_counts() # this gives same values but not ordered by year

# N records per unique VehicleID per year
df.groupby('FinancialYear')['VehicleID'].value_counts()

# unstack: switches rows to columns (.fillna(0) fills missing values with 0)
df.groupby('name')['activity'].value_counts().unstack().fillna(0)


### Time differences / durations:
# see page on pandas for data science tricks: https://medium.com/towards-data-science/pandas-tips-and-tricks-33bcc8a40bb9

# calculate time differences between consecutive events by the same person (name)
df = df.sort_values(by=['name','timestamp'])
df['time_diff'] = df.groupby('name')['timestamp'].diff()

# rename columns (pandas rename function)
df = pd.DataFrame({"A": [1, 2, 3], "B": [4, 5, 6]})
df
df = df.rename(index=str, columns={"A": "a", "B": "b"})

# rename based on first 2 characters of colname
for col in df.columns:
    if col[:2] == '01':
        df.rename[columns={col:'A'}, inplace=True)



### Pivot Tables --pandas

# get summary table with mean kwh per manufacturer (row) and year (col)
df.pivot_table(values='kwh', index='manufacturer', columns='year', aggfunc=np.mean)

# get mean of all columns by manufacturer and year (default aggfunc='mean')
print(pd.pivot_table(Bikes, index=['manufacturer','year']))

# 2+ summary statistics
# adds an 'all' row and column to show mean per year across all manufacturers and mean per manufacturer across all years

# e.g. get min and max
print(pd.pivot_table(Bikes, index=['manufacturer','year'], aggfunc=[np.min, np.max]),
      margins=True)	# margins=True outputs an All row/col

# can also pass self-defined functions to the aggfunc



# =============================================================================
### """ PLOTTING """
# =============================================================================
""" See 'python_code_plotting.py' in Online_course_notes. """




# =============================================================================
### """ PYTHON 2 vs PYTHON 3 """
# =============================================================================

# Mostly the same but some small differences, e.g. the print function:

#  Python 2 syntax:
print something  # does not require parentheses

# Python 3 syntax:
print(something)  # requires parentheses as it's a function


## Use the '2to3' python function to translate python 2 code to python 3:

# must use the 2to3 command in the cmd line
# help page: https://docs.python.org/release/3.0.1/library/2to3.html

# The 2to3 script is located in C:\Users\User\Anaconda3\Scripts  (I have set this as the Path for the cmd line already, via the Control Panel: See my notes in "Using conda to manage packages in Python.txt" / Change the path via: ControlPanel\System\Advanced system settings\Environment Variables\Path-edit)

# Can either translate code entered directly into the command line, or code saved in a file (file can be .txt or .py; haven't tried any other file types).

# Change the working directory in the cmd line to the location of the file containing the code to be translated.
2to3 FILENAME.py	# this outputs changes that need to be made to convert each line from Python2 to Python3 into the console.
2to3 -w FILENAME.py	# this makes any necessary changes directly to the source file (and makes a backup copy of the original file version called FILENAME.py.bak).
2to3 -w -n FILENAME.py	# this makes any necessary changes directly to the source file (WITHOUT making a backup of the original).


# =============================================================================
###  """ RANDOMISING or FINDING ALL COMBINATIONS OF THINGS """
# =============================================================================


# find all combos of two letters - using list comprehension
lowercase = 'abcdefghijklmnopqrstuvwxyz'
allcombos = [letter1+letter2 for letter1 in lowercase for letter2 in lowercase]

# find all possible combos of two letters followed by 2 digits, e.g. member IDs
lowercase = 'abcdefghijklmnopqrstuvwxyz'
digits = '0123456789'
allcombos = [a+b+c+d for a in lowercase for b in lowercase for c in digits for d in digits]


# =============================================================================
###  """ RAW INPUT """
# =============================================================================

# when the user can type something in
# automatically converts what they type into a string. Specify integer using int():
guess = int(raw_input("Your guess: "))

""" Functions """
# function to test whether a number is even or not:
def is_even(x):
    """ Returns True if a number is even and False if it is odd """  # = doc string
    if x % 2 == 0:          # if number x is divisible by 2 the output is zero and True is returned.
        return True
    else:
        return False
is_even(7)                  # 7 is not an even number so False is returned.

# Floats and integers (= decimals and whole numbers)
# NOTE: Division is integer division in Python 2 (Udacity/Codecademy), but not in Python 3.
# Calculations on integers may produce an integer result, e.g. 5/2=3 rather than 2.5.
# Get more accurate, decimal results by using a float (decimal number) in the calculation:
11-5.0      # 5.0 is a float, so the answer will also be a float.
8/5         # as both inputs are integers the answer is also (displayed as) an integer.


# =============================================================================
###  """ REGULAR EXPRESSIONS """
# =============================================================================

#https://docs.python.org/3/howto/regex.html
#https://www.tutorialspoint.com/python/python_reg_expressions.htm

# regular expressions regex: mini programming language to manipulate strings
import re

# to escape a backslash to set a string as '\test', prefix backslashes with another backslash, '\\test'
# \t in python is a tab, so add 2 more backslashes to ensure the escape character is shown: '\\\\test'
a = '\\\\test'
print(a) # = '\\test'

# use regex to split logfile string into data frame
regex = re.compile('[a]')
print(regex)

# THIS DOESNT WORK
a = regex.match('hellofdgsjgks')
print(a)


# =============================================================================
###  """ Rounding / reducing d.p. without rounding """
# =============================================================================

# to reduce a float to 4 d.p.
# this lambda function takes the decimal part of the coordinate,
# converts it to a str to take the first 4 numbers after the decimal point,
# then converts the str back to float and adds the whole number back on to the truncated decimal.

m = 31.56200027
int(m) + float(str(m - int(m))[1:6])

# returns 31.5620


# =============================================================================
###  """ S3 """
# =============================================================================

### see if an object exists in s3 bucket
import boto3

s3 = boto3.resource('s3')
bucket = s3.Bucket('bi-photobox-analytics-eu')
key = 'Jo/crm/recommendation_engine/weekly_recommendations/product_recommendations/year=2018/month=12/day=21/'

objs = list(bucket.objects.filter(Prefix=key))

# does it exist
if len(objs) > 0:# and objs[0].key == key:
    print("Exists!")
else:
    print("Doesn't exist")

# make list of objects without bucket prefixes
objects = []
for item in objs:
    objects.append(item.key)
print(objects)



# =============================================================================
###  """ SAVING FILES - AS CSV OR TXT """
# =============================================================================

# CSV
import pandas as pd

# read in data from a csv file and convert to a pandas dataframe
data = pd.read_csv('AirPassengers.csv')

# save pandas dataframe as a csv file
mydataframe.to_csv('my_data.csv')
mydataframe.to_csv('my_data.csv', index=False) # index=False stops pandas adding an index column 'Unnamed: 0' to the df on save!
mydataframe.to_csv('my_data.csv', compression = 'gzip') # save as compressed file

# save certain columns to a csv
# one column
a['content_id'].to_csv("filename.csv")
# multiple columns
a[['content_id' , 'content']].to_csv("filename.csv")


# TXT
# where myfile is the article
myfile = open(output_filename, 'w') # w=write mode
myfile.write(headline)
myfile.write("\n") # insert new line
myfile.write(text)
myfile.close()

# save models / python objects to disk
# https://machinelearningmastery.com/save-load-machine-learning-models-python-scikit-learn/
filename = 'my_model.sav'
pickle.dump(model_object, open(filename, 'wb'))
# load it back
loaded_model = pickle.load(open(filename, 'rb'))


# =============================================================================
### """ SAVING PYTHON OBJECTS FOR LATER USE """
# =============================================================================

""" Saving models = object serialization (representing an object with a stream of bytes).
    Restoring the model is deserialization. """


## PICKLE

""" Pickle module implements an algorithm for serializing and
    de-serializing a Python object structure (list, dict, etc.) so it can be
    saved on disk.
    Serializing means it converts the object into a character stream containing
    all the info necessary to reconstruct the object in the same layout/format
    in another python script.

    Pickle files are byte streams, so should be opened in binary mode:
       use 'wb' ('b' for binary) during file writing and 'rb' during file opening.
"""
# official user guide https://docs.python.org/2/library/pickle.html
# simpler quick ref guide https://wiki.python.org/moin/UsingPickle

import pickle

# Save a dictionary into a pickle file.
fave_col = {"lion": "yellow", "kitty": "red"}   # create dictionary
pickle.dump(fave_col, open("save.p", "wb"))     # pickle fave_col & save as "save.p"
                                                # "wb" = write in binary mode
# Load the dictionary back from the pickle file.
fave_col_p = pickle.load(open("save.p", "rb"))
        # 'rb' = opens the file for reading in binary mode.

# compress pickle files with gzip - saves lots of space!
import pickle
import gzip

#save
#filename = 'saved_models/p2b_rf_rscv_343dates_20180323.sav'
#pickle.dump(random_search, gzip.open(filename, 'wb'))

#open
#filename = 'saved_models/p2b_rf_rscv_343dates_20180323.sav'
#random_search = pickle.load(gzip.open(filename, 'rb'))

## NOTE: pickle can't save (or gzip compress) objects > 1GB...
## Instead use sklearn's joblib library:

## JOBLIB

# Intended to be a replacement for Pickle, for objects containing large data. Works with sklearn models.
# Add compress parameter between 0-9 to adjust compression; default = 0, more compression increases save/load time.
# https://pythonhosted.org/joblib/generated/joblib.dump.html

from sklearn.externals import joblib

#save model without compression
#filename = 'saved_models/p2b_rf_default_363dates_200estimators_20180326-joblib'
joblib.dump(rf_default, filename)

# save with compression (compress=0-9, where 9 is highest compression but also slowest to save/load)
#filename = 'saved_models/p2b_rf_default_363dates_200estimators_20180404_c9'
joblib.dump(rf_default, filename, compress = 9)

# open/load (same code for compressed and uncompressed files)
#filename = 'saved_models/p2b_rf_default_363dates_200estimators_20180404_c9'
rf_default_test = joblib.load(open(filename, 'rb'))


# ============================================================================
### """ SCIPY: hypothesis testing in Python """
# ============================================================================
from scipy import stats

## t-test
# compare the means of 2 independent samples (h0 = no difference; if p<0.05 (or whatever alpha you set) then h0=false)
stats.ttest_ind() # takes as input two series
stats.ttest_ind(early['assignment1_grade'], late['assignment1_grade'])
# output is a tuple with a t-value and p-value

## multiple testing ('p-hacking')

# bonferroni correction: very conservative
# adjust p-values if running multiple t-tests by the number of tests run, e.g
# if run 3 t-tests, multiple all p-values by 1/3

## cross-fold validation
# run tests on a sample of data, e.g. 80%
# make some hypotheses based on these test results
# test the new hypotheses (SMALLer number of tests) on the remaining 20% data




# ============================================================================
### """ SET WORKING DIRECTORY """
# ============================================================================

# use <cd FILEPATH> (or select folder manually using top-right menu in Spyder)
cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\

# print current working directory
pwd

# probably better practice to organise files within Spyder projects


### """ Spyder projects """

# Projects > New Project (saves them in User/AnacondaProjects)
# Manage files in Project explorer pane.
# https://pythonhosted.org/spyder/projects.html
# =============================================================================
###  """ Shortcuts for running / manipulating code """
# =============================================================================

# Modify / find them via Tools>Preferences>Keyboard shortcuts.
# F5 to execute the entire current file
# Ctrl+E (default was F9) to execute the current line / highlighted chunk


###  Cells
# A cell is the code between two lines which start with the agreed tag #%%
# #%%  # have put hashtag before this to stop all the code being highlighted!
hello()
# #%%

# Shift + Enter to run current cell
# Ctrl + Enter to run current cell and advance to the next

###  Moving code
# Alt+<Up Arrow> moves the current line/selection up, same with down arrow


# =============================================================================
###  """ SQLite DATABASES """
# =============================================================================
# https://docs.python.org/2/library/sqlite3.html

import sqlite3
# connect to a database - if one called example.db doesnt already exist in
# the working directory then this will create a new empty database.
conn = sqlite3.connect('example.db')

# create a cursor to execute on
# need to call this to do anything to the database - add/get data, tables etc.
c = conn.cursor()

# Create table
c.execute('''CREATE TABLE stocks
             (date text, trans text, symbol text, qty real, price real)''')

# Insert a row of data
c.execute("INSERT INTO stocks VALUES ('2006-01-05','BUY','RHAT',100,35.14)")

# Save (commit) the changes
conn.commit()

# We can also close the connection if we are done with it.
# Just be sure any changes have been committed or they will be lost.
conn.close()



# =============================================================================
###  """ STRINGS """
# =============================================================================

# 'hello' - sequence of characters (must be enclosed by single or double quotes)
# immutable
# s[i]          # returns the ith character in string s

# Must be enclosed by single or double quotes.
"My dog's name is"    # Can use a single quote as an apostrophe within a pair of double quotes.
'My dog"s name is'    # and vice-versa.

# Can multiply strings
'!' * 10    # will print 10 exclamation marks

# Can concatenate strings
'Hello ' + 'Jo' + '!'    # will print 'Hello Jo!'

# To add to a string
sentence = "I have a dog"
sentence2 = sentence + " " + "cat"
print sentence2   # = 'I have a dog cat'

# to insert things into strings use .format()
# .format() acts on string types
x = 5
my_string = "My number is {}".format(x)
print(my_string) # >>> My number is 5


# convert to upper and lower case
s.upper()
s.lower()


# Indexing strings
'Joanne'[0]     # selects the 0th character in the string ('J').
'Joanne'[-1]    # counts from the right side (backwards) so will show the last character in the string ('e').
'Joanne'[-2]    # selects the second last character ('n').

# Indexing subsequences from strings
# Index subsequencing is v.resilient and works even when we reference sequences that aren't in the string, e.g. [0:10] for a 2-character string.
# (whereas indexing specific single positions in the string would produce an error)
'Joanne'[0:2]   # prints characters from start index 0 up to BUT NOT INCLUDING stop index 2 ('Jo')
'Joanne'[0:3]   # prints characters between index 0 and 3 ('Joa')
'Joanne'[:2]    # prints from 0th character to index 1 ('Jo')
'Joanne'[2:]    # prints from index 2 to the end ('anne)

#  Filling in strings using %
print("Her name was %s and she was %s" % ('Betty', 'beautiful'))

### Searching/finding within strings (CASE SENSITIVE)
searchstring.find(targetstring)    # targetstring = the substring to find
# e.g.
sentence = "I have a dog"
print sentence.find('dog')    # output is index of the FIRST occurence of the word 'dog' within 'sentence' (9)
# If the target string is not found then the result is '-1' which means False.
# So when using if statements on search results, differentiate positive/negative results using:
if result == -1:
    # OR:
if result:
# Both of these statements mean "if result is False"

# CASE SENSITIVE: e.g. print sentence.find('Dog') would return -1 as 'dog' is all lowercase in the sentence.

# Search string after a given index position
print sentence.find('dog', 5)   # searches for the first occurence of dog after position 5. If dog started at position 4 it would return -1. If it started at position 5 it would return 5. If the sentence was 'my dog ate a dog' then although the first occurence is at position 3, the first occurence AFTER position 5 is at position 13.

# Example: to extract a URL from html code of a web page (from Udacity intro to comp sci course):
    # 1. find the location of the (first) URL/link on the page
start_link = page.find('"', '<a href=')  # <a href= is the link tag at the start of each clickable link on a html web page, e.g. <a href="http://www.google.com"
    # 2. find the start of the link
start_quote = page.find('"', start_link)    # finds the first quotation mark after the start_link
    # 3. find the end of the link
end_quote = page.find('"', start_quote+1)  # finds the first quotation mark after the start_quote, i.e. the second quotation mark. start_quote+1 ensures the quote excludes the quotation mark (as index 0 = character 1 etc).
    # 4. save the full link
url = page[start_quote+1:end_quote]


### Split string into a list
# This separates words in strings - not perfect as doesn't separate out punctuation such as ',' or '!'
# so 'dog!' wouldn't show up if I searched for 'dog'.
sentence = "I have a dog!"
sentence.split() # using the default delimiter, which is a space s.split(' ')
# returns ['I', 'have', 'a', 'dog!']
sentence.split()[0]  # view the first word in sentence

### Join elements of a list into a string
'delimiter'.join(a)
# e.g. to join elements of list a
a=['Hello', 'World']
' '.join(a) # returns 'Hello World'

# convert list to string, and insert characters between the items in a list
" <3 ".join(["Jo", "Antonio", "Flowers"]) # Jo <3 Antonio <3 Flowers'


### Strip whitespaces from strings
a = ' Mug '
a.strip() # returns 'Mug'


# iterate through words in a string
for word in sentence.split():   # <for word in sentence:> would print each character, not word.
    print word

# Remove key words from a string using str.replace()
blacklist = ["have ", "a ", "!"]
for word in blacklist:
    sentence = sentence.replace(word, "")
print sentence  # returns <I dog>

# Use triple quotes for long quotes over multiple lines:
quote = """I have a dog, I have a dog, I have a dog, I have a dog,
I have a dog, I have a dog, I have a dog, I have a dog! """
quote.split()

### Convert strings to numbers and back
ord(<string>)  # converts a one-letter string to a number ('a' = 97, 'b' = 98, 'A' = 65, 'B' = 66). Numbers are based on ASCII character encoding.
# !! ord only works on one letter strings!!
# NOTE: ord(str(<number>)) treats the str(number) as a 'string' so ord(str(1)) = 49 rather than 1.

chr(<number>)  # converts a number to a one-letter string
# ord and chr are interchangeable, e.g. chr(ord('a')) = 'a' and ord(chr(1)) = 1

# to print a string in a nice format
print("%s" %query)

# strip numbers from end of country names ('\d+' means numbers matching 0-9)
energy['Country'] = energy['Country'].str.replace('\d+', '')




# =============================================================================
###  """  SUBSETTING DATA FRAMES """
# =============================================================================

# subset by location
views_GBR = views_sun_by_location[views_sun_by_location['location'] == "GBR"]

# subset by date
emailed_18th = emailed.loc[emailed.date == datetime.date(datetime.strptime('2018-04-17', "%Y-%m-%d"))]




# =============================================================================
###  """  TENSORFLOW """
# =============================================================================

# DNNRegressor = deep neural network regressor
# https://www.tensorflow.org/api_docs/python/tf/contrib/learn/DNNRegressor
# Also have LinearRegressor

# Note TensorFlow usually gives lots of errors as it's constantly under development!

# Note on performance: notebooks are OK for running TensorFlow up to a point,
# but Google Cloud ML Engine is better fpr large scale training and serving models
# as APIs (APIs are trained ML models, e.g. Google Vision API for object detection
# and facial recognition, Google Translate API...)

# Can take Google pretrained models and continue to train them with
# your own data to improve them/make them more specific, e.g. Translate, Vision...

# Train a model (havent tested this so read documentation...)
import tensorflow as tf
X_train = predictors
Y_train = targets

model = tf.contrib.learn.DNNRegressor(hidden_units = [5], feature_columns = [10])
#model = tf.contrib.learn.LinearRegressor(X_train, y_train)
model.fit(X_train, y_train, steps = 100)

# Open a pretrained model
model = tf.contrib.learn.DNNRegressor(model_dir = 'path_to_my_trained_model', hidden_units = [5])

# Make predictions
preds = model.predict(X_test)
print(preds)

# Evaluate, e.g. root-mean-square error (RMSE), a frequenctly used measure that
# measures differences between actual and predicted values
rmse = np.sqrt(np.mean((predictions-targets)**2))
# or:
rmse = rmse(np.array([2,2,3]), np.array([0,2,6]))



# =============================================================================
###  """  TEXT PROCESSING """
# =============================================================================

# Bag of Words (sklearn CountVectorizer) and Tf-idf (sklearn TfdfVectorizer) representations
""" TF: Just counting the number of words in each document will give more
    weight to longer documents. To avoid this, use Term Frequencies.
    i.e. #count(word) / #Total words, in each document. """
# Filtering out stopwords
# Stemming

# For details see lesson11_text_learning.py in:
# C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects
# (and ..\ud120-projects\text_learning\vectorize_text.py for some text processing functions).

### Textacy
""" to install use CONDA and not PIP - otherwise it is only installed in the
    pkgs folder (in Anaconda) and not in site-packages, which is where python
    looks for libraries """
conda install -c conda-forge textacy
""" I had trouble with python not looking in the right place for textacy when
    I tried to import it. Had to install the latest Windows windows software
    development kit  (and Windows C++ visual compiler, though not sure if this
    was required as I installed it before the SD kit and it wouldn't work
    without the kit too). """


# =============================================================================
### """ TIMING CODE """
# =============================================================================

# E.g. to make predictions with a machine learning classifier
from time import time    # import the time module
t0 = time()              # show current clock time
pred = clf.predict(features_test)       # run your code of interest
print "prediction time:", round(time()-t0, 3), "s"   # use current clock time to calculate time elapsed

# Display progress logs on stdout
logging.basicConfig(level=logging.INFO, format='%(asctime)s %(message)s')

# progress bar
# https://pypi.python.org/pypi/tqdm
import tqdm
with tqdm(total=100) as pbar:
    for i in range(10):
        pbar.update(10)



# =============================================================================
###  """ TUPLES """
# =============================================================================
""" A tuple is a comma-separated list of values.
    Values stored in a tuple can be any type, and they are indexed by integers.
    Unlike lists, tuples are immutable.

    Tuples are comparable and hashable so we can sort lists of them and use
    tuples as key values in Python dictionaries.

    Although unnecessary, it's common to enclose tuples in parentheses () to
    help people identify them in code. """

t = ('a', 'b', 'c', 'd', 'e')

# To create a tuple with a single element, you have to include the final comma:
t1 = ('a',)
type(t1)    # returns <type 'tuple'>
            # without the comma it would evaluate to a string.


## Sort a list of tuples
mytuplist = [('app', 121, 4),('ban', 231, 3),('car', 148, 2), ('dat',221, 1)]
# key = function that identifies the thing to sort by.

# use sorted() to return the sorted list without changing the original
sorted(mytuplist, key=lambda x: x[1]) # x[1]=sort by 2nd value in each tuple (in index position 1)

# use list.sort() to sort the list and save it in the new order.
mytuplist.sort(key=lambda x: x[1])

## Alternatives to lambda
# itemgetter
from operator import itemgetter
sorted(mytuplist, key=itemgetter(2)) # x[2]=sort by the 3rd value in each tuple.

# a self-defined function
def mysortfun(a):
    return a[1]   # to sort by 2nd element
sorted(mytuplist, key=mysortfun)


# =============================================================================
###  """ VARIABLES """
# =============================================================================

# variable names must start with a letter or underscore, but can contain any combination of letters, numbers and underscores
# variable names rarely start with capital letters.

# Assign a value to a variable using '='
a = 5     # assigns the value 5 to the variable 'var'


### Aliasing

# variable names refer to objects; sometimes multiple variables refer to the same object
# e.g. if you create a variable 'animals' to refer to a list:
animals = ['tiger', 'lion', 'zebra']
# and then 'make a copy' of the list using the following code:
new_animals = animals
# Both animals and new_animals refer to the same object: they are aliases - different names for the same list.
# Lists are mutable (numbers aren't - can't change 3 to 4 as that would be too confusing!)
# So changing the list that animals refers to also changes the list that new_animals refers to. E.g. if 'James Bond' dies, so too does 'Agent 007'
animals[1] = 'hippo'    # changes 'lion' to 'hippo' in the animals list
new_animals[1]     # is now also 'hippo' instead of 'lion'
new_animals[2] = 'seal'     # changes 'zebra' to 'seal' in the new_animals list
animals[2]  # is now also 'seal' instead of 'zebra', because animals refers to the same object/list as new_animals.


### Making copies of mutable variables such as lists

# to make a separate list called new_animals that is a TRUE COPY that won't also be affected by changes to the animals list, reassign the original string to the new_animals variable
new_animals = ['tiger', 'lion', 'zebra']
new_animals[0] = 'chimp'  # changes the first element of new_animals from 'tiger' to 'chimp'
animals[0]    # the first element is still 'tiger' because this time animals refers to a different object to new_animals.



### Multiple assignment

value_1, value_2 = newvalue_1, newvalue_2     # can assign any number of variables in one line, as long as the number of values matches the number of newvalues and they're in the right order.
# e.g.
a,b = 1,2    # this assigns a=1 and b=2
a,b = b,a    # means 'a should equal b and b should equal a' so their values are swapped.

# can also use functions/procedures for multiple assignment, e.g. if function <def get_url(page):> returns two outputs (url and endpos), you can assign these outputs to new variable names thus:
url, endpos = get_url(page)

# To specify what to return in functions with multiple outputs:
# e.g.when using if statement in function that returns two outputs:
if result:  # this means if the result is True, exists or is anything other than False/empty/None.
    return None, 0  # This returns "None, 0" for the two output values if result is False, e.g. if a find() command returned nothing.





# =============================================================================
###  """ WHILE LOOPS """
# =============================================================================

# Permits code to execute repeatedly until a certain condition is / not met.
# syntax:
while condition:
    <block>     # keeps going as long as the TestExpression is True
                # as soon as the TestExpression is False, Python goes to the next expression.

# Instead of a condition such as <while result = True>, can just say:
while True:     # this keeps the while loop going as long as the if statement in the block below is True.
    if blah > 5:
        return x
    else:
        break

# Be careful of infinite loops!!!
# E.g. the loop below will run forever, because i will never equal 10:
# i = 1
# while i != 10:    # = as long as i is not equal to 10, run the block below
#     i=i+2         # adds 2 to i each time, so returns odd numbers only.
#     print(i)

# Example while loops
while list_name:    # equivalent to 'while len(list_name)>0' / 'as long as the list is not empty'.
    <block>
    # opposite to while list_name: is while not list_name: (i.e. as long as the list IS empty)
while a < b:
    <block>
while a not in b:
    <block>
