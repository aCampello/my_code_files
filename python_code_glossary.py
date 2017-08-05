# -*- coding: utf-8 -*-

                    ## -------------------------------- ##
                    """ Python - useful bits of code """
                    ## -------------------------------- ##


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



### Jupyter / iPython Notebook
# ================================

# https://ipython.org/ipython-doc/3/notebook/nbformat.html

""" Jupyter (né IPython) notebook files are simple JSON documents, containing 
    text, source code, rich media output, and metadata. each segment of the 
    document is stored in a cell.

    Works like the python interpreter in spyder, but with more menu-driven
    features for annotating code. 

    Useful on computers without Python or Spyder installed on the hard drive.

    Open Jupyter Notebook via the start menu or from Git Bash
    
    This opens http://localhost:8888/tree in the web browser and lists all my
    local files.
    
    Keep Git Bash open on the desktop to keep the browser session connected.
    Can set it to work for Python 2 or 3.

    Click New on top right to create a new Notebook. 
    
    Can drag and drop text or content from desktop programs into cells.
    
    Set the working directory using cd C:\User\... the same as in Spyder.

    Displays output below the cell containing the code (can toggle output on/off 
    or just clear it).
    
    Can rearrange cells.
    
    Can tag cells with keywords (new feature April 2017: not yet searchable).
    
    Can create both code cells and rich text cells to display notes
    
    Print preview for printing with formatting. 
"""




# =============================================================================
###  """ DICTIONARIES """
# =============================================================================

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
# NB if Chicken Alfredo is already in the dictionary, it will UPDATE the value.
menu['Chicken Alfredo'] = 14.50  # Adding new key-value pair
menu['Chicken Calzone'] = 12.50  # Adding new key-value pair

menu['Chicken Balti'] = 'Hot', 'Mild' # add multiple values to the same key - will be saved in () brackets.
                                            # returns {'Chicken Balti': ('Hot', 'Mild')}
menu['Chicken Balti'] = ['Hot', 'Mild'] # add multiple values to the same key, in a list.
                                            # returns {'Chicken Balti': ['Hot', 'Mild']}

## Accessing values in dictionaries
print menu # view the whole dictionary
menu.keys()      # get all the keys
menu.values()    # get all the values
menu.items()     # get all the key/value pairs
menu['Chicken Alfredo'] # view the value  for Chicken Alfredo.

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
    print elements[key]["name"], elements[key]["number"]
    
""" prints:
    Hydrogen 1 
    Oxygen 8
in some order """


# Another example of accessing specific values in dictionaries:

dic = {"apple":(1,2,3), "banana":(4,5,6), "carrot":(7,8,9)}

for item in dic:
    """ print every 3rd element in a tuple, for each key """
    print item          # print the key (name)
    print dic[item][2]  
    print "--"  
    
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

# =============================================================================
###  """ ERRORS & BUGS """
# =============================================================================

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
<list>.index(<value>)   # returns the index of the FIRST position where the value is found.
                        # returns an error if the value doesn't exist in the list.


# =============================================================================
###  """ FUNCTIONS """
# =============================================================================

###  basic in-built functions, e.g. len()

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
###  """ IF / ELSE STATEMENTS """
# =============================================================================

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
my_list = [x ** 2 for x in range(1, 11)]  # makes a list of squared numbers (x**2) from 1 to 10 (range 1,11). Here, the list should be: [1, 4, 9, 16, 25, 36, 49, 64, 81, 100].
my_list[::-1]  # negative strides print the list backwards / traverses from right to left.

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



# length of list
len(list)   # returns number of elements in the list (only counts outer elements, so in a list of two lists len = 2)

# Refer to elements in a list
square_list[0]  # refers to the first item in the list (= index #0 - counting starts at 0)

# Refer to elements in a list of lists, e.g. [['John', 1990], [['Jim', 1949], 'Sarah', 1970]
square_list[0][0]   # refers to the first element of the first list in square_list, i.e. 'John'.
square_list[0][1]   # refers to the second element of the first list in square_list, i.e. 1990.

# Create a new list containing the first character from each list (index 0) in a list of lists.
char = [row[0] for row in grid]    # here, 'grid' is a list of n lists of length n

# Sorting lists
square_list.sort()  # sort list ascending

# list slicing
[start:end:stride]  # starting index = inclusive, default 0; ending index = exclusive, default end of list; stride = space between items, default 1, e.g. (::2) would select every other item in whole list).
print(my_list[2:11:])   # prints ALL numbers between index positions 2 and 10.
print(my_list[2:11:2])  # prints EVEN numbers between index positions 2 and 10.

# Combine lists - using the plus operator
# joins the contents of lists to create a new list, whereas 'append' (below) inserts a new list into a existing list.
new_list = list_1 + list_2    # concatenates list1 and list2 to make a new list (doesn't change the original two lists)
new_list = list1 + [5, 6]  # [1,2,3,4,5,6]      # concatenates the two lists

### List mutation: change/add/delete
""" lists are mutable (can be changed/appended) unlike strings, e.g. can't 
    change letters in strings using string[0]='s' will cause an error; but can 
    change values in lists or add new values."""

# Change values in lists
square_list[1] = 70  # changes the item at index #1 to 70.

# Add values to lists
square_list += "dog"  # Add "dog" to the list

# Append (add) new values to lists
<list_name>.append = <element_to_add>  # add a new element to the list
# e.g.
square_list.append("dog")  # append = add. Add "dog" to the end of the list ('append' mutates the old list, rather than creating a new list)
list1.append([7, 8])    #[1,2,3,4,5,6,[7,8]]    # inserts the new list [7,8] as an element in list1
square_list.append(2*round_list)  # append = add (not replace). This means multiply values in the round_list by 2 and add them to the square_list.

### Remove values from lists
# using the Remove command .remove()
<list>.remove()
square_list -= "dog"  # Remove "dog" from the list
square_list.remove(1)  # Removes the number 1 from the list n (if it contained 1) - NOT the item at index #1

# using the Pop command .pop()
<list>.pop()    # mutates the list by removing and returning its last element (to show what it removed).
                # if the () parentheses are empty, it pops the LAST element in the list.
x = square_list.pop(1)  # removes the element at index #1 in square_list (pops it out) and saves it as 'x'

# Using the Delete command del()
del(list[])
del(square_list[1])  # removes the item at index 1 from list n but does not return anything.

### Search in lists
# In
<value> in <list>
print(3 in [1,2,3,4,5])    # will print True, as 3 appears in the list
print(6 in [1,2,3,4,5])    # will print False, as 6 does not appear in the list

# Not In
<value> not in <list>
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

""" A python dictionary can’t be read directly into an sklearn classification
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

### (4) K NEAREST NEIGHBOUR (KNN)
""" Widely used classification technique
    (Can also be used for regression).
    Easy to interpret output, reasonable predictive power and low calculation 
    time (compared to e.g. random forest).

    HOW IT WORKS:
    Assigns a query point to the class most common among it's k-nearest 
    neighbours in the feature space.
"""

### sklearn syntax for clasfrom sklearn import treesifier algorithms
    
# 1) Import the algorithm / function
        from sklearn.naive_bayes import GaussianNB    # for naive Bayes
        from sklearn import svm                       # for SVM
        from sklearn import tree                      # for decision trees
        from sklearn.neighbors import KNeighborsClassifier    # for K nearest neighbour
        
# 2) Create the classifier
        clf = GaussianNB()      # for naive Bayes
        clf = svm.SVC()         # for SVM (svms are called SVC in sklearn)
        clf = tree.DecisionTreeClassifier()     # for decision trees
        clf = KNeighborsClassifier(n_neighbors=5, weights="uniform")   # for KNN
        
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


### Accuracy Score
""" accuracy = [N items (datapoints) in a class that are labelled correctly / 
                     N items in that class]

    Accuracy is less reliable for skewed data, i.e. with imbalanced classes 
    like the Enron dataset (many more non-POIs than POIs), as you can just 
    guess the more common class label for every point AND STILL GET PRETTY 
    GOOD ACCURACY!
    
    Accuracy is also less reliable if you want to err on the side of caution 
    of a yes/no answer, e.g. cancer tests - prefer a false positive than a false neg. 
"""
from sklearn.metrics import accuracy_score
accuracy_score(pred, labels_test)
clf.score(features_test, labels_test)


## Confusion matrix
""" shows N datapoints predicted to be in each class and whether the prediction
    was correct (+ve) or incorrect (-ve), as a matrix. 
    So 2x2 matrix if there are 2 classes (+ve/-ve for points predicted in class 1; 
    +ve/-ve for points predicted in class 2) - see Udacity Mach Learn Less 15 Vid 6-9.
"""
from sklearn.metrics import confusion_matrix
y_pred = clf.predict(X_test)  # X = features, y = labels

print confusion_matrix(y_test, y_pred, labels=range(n_classes))  # n_classes = N unique labels/classes, e.g 7 people (see lesson13_pca.py code)

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
        confidence that it’s very likely to be a real POI and not a false alarm. 
        On the other hand, the price I pay for this is that I sometimes miss 
        real POIs, since I’m effectively reluctant to pull the trigger on edge cases.”
        
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
print classification_report(y_test, y_pred, target_names=target_names)  


# =============================================================================
### """ MEASURE TIME TAKEN TO RUN CODE """
# =============================================================================

# E.g. to make predictions with a machine learning classifier
from time import time    # import the time module 
t0 = time()              # show current clock time
pred = clf.predict(features_test)       # run your code of interest
print "prediction time:", round(time()-t0, 3), "s"   # use current clock time to calculate time elapsed

# Display progress logs on stdout
logging.basicConfig(level=logging.INFO, format='%(asctime)s %(message)s')


# =============================================================================
###  """ MODULES """
# =============================================================================

""" A module is a .py file containing Python definitions and statements. 
    use import module to access functions in that file, e.g. to import two
    functions called featureFormat and targetFeatureSplit from a file called
    feature_format.py, use this command:
        
    from feature_format import featureFormat, targetFeatureSplit
    
    Then you can use those functions in the current file.
"""

# can also import an entire module
import math
# then have to type math.function to specify where the function is from, e.g.:
math.sqrt() # to run the square root function in the math module
print(math.variable) # to print variable contained within the math module

# Could import all functions from math so don't have to type math.function but shared function names can get confusing
from math import *

# But it's better to import a specific function from a module only, e.g. 'sqrt' from 'math'
from math import sqrt

# To see all functions contained in a module:
import math             # Imports the math module
everything = dir(math)  # Sets everything to a list of things from math
print(everything)        # Prints the names of all the functions.

# Download only the packages you want with the conda command (in Git Bash, not Spyder)
conda install PACKAGENAME
# e.g. conda install pyflux

# can use pip instead of conda, but not recommended for Anaconda users 
pip install PACKAGENAME

# View module version
import numpy as np
np.__version__


# =============================================================================
###  """ NumPy """
# =============================================================================
# https://docs.scipy.org/doc/numpy-dev/user/quickstart.html

# NumPy’s array class is called ndarray. 

# Basic NumPy commands
ndarray.ndim    # number of axes (dimensions) of the array, e.g. arrayname.ndim
ndarray.shape   # dimensions of the array 
                # For a matrix with n rows and m columns, shape will be (n,m)

numpy.reshape(a, newshape)  # reshape an array into a new shape without changing its data.
                            # a = array to be reshaped
                            # newshape = tuple of integers: (n_rows, n_columns). 
                            # If newshape is just one integer then the result 
                            # will be a 1D array of that length.
                
                               

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
### """ PICKLE FILES """
# =============================================================================

""" The Pickle module implements an algorithm for serializing and 
    de-serializing a Python object structure (list, dict, etc.) so it can be 
    saved on disk. 
    Serializing means it converts the object into a character stream containing
    all the info necessary to reconstruct the object in the same layout/format
    in another python script.
    
    Python pickle files are byte streams, so should be opened in binary mode:
       use 'wb' ('b' for binary) during file writing and 'rb' during file opening.
"""
# official user guide https://docs.python.org/2/library/pickle.html
# simpler quick ref guide https://wiki.python.org/moin/UsingPickle
 
## EXAMPLE:
    
import pickle

# Save a dictionary into a pickle file.
fave_col = {"lion": "yellow", "kitty": "red"}   # create dictionary
pickle.dump(fave_col, open("save.p", "wb"))     # pickle fave_col & save as "save.p"
                                                # "wb" = write in binary mode
# Load the dictionary back from the pickle file.  
fave_col_p = pickle.load(open("save.p", "rb"))    
        # 'rb' = opens the file for reading in binary mode.



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
### """ SET WORKING DIRECTORY """
# =============================================================================

# use <cd FILEPATH> (or select folder manually using top-right menu in Spyder)
cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\

# print current working directory
pwd


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


### Split strings
# This separates words in strings - not perfect as doesn't separate out punctuation such as ',' or '!'
# so 'dog!' wouldn't show up if I searched for 'dog'.
sentence = "I have a dog!"
sentence.split() # returns ['I', 'have', 'a', 'dog!']

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




# =============================================================================
###  """  TEXT PROCESSING """
# =============================================================================

# Bag of Words (sklearn CountVectorizer) and Tf-idf (sklearn TfdfVectorizer) representations
# Filtering out stopwords
# Stemming

# For details see lesson11_text_learning.py in:
# C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects
# (and ..\ud120-projects\text_learning\vectorize_text.py for some text processing functions).



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

# While means 'as long as', so will continue to loop while the statement is true
# syntax:
while <TestExpression>:
    <block>     # keeps going as long as the TestExpression is True
                # as soon as the TestExpression is False, Python goes to the next expression.

# Instead of a test expression such as <while result = True>, can just say:
while True:     # this keeps the while loop going as long as the if statement in the block below is True.
    if blah > 5:
        return x
    else:
        break

# Be careful of infinite loops!!!
# E.g. the loop below will run forever, because i will never equal 10:
# i = 1
# while i != 10:    # = as long as i is not equal to 10, run the block below
#     i=i+2         # adds 2 to i each time, so returns 3,5,7,9,11 and skips 10.
#     print(i)

# Example while loops
while list_name:    # equivalent to 'while len(list_name)>0' / 'as long as the list is not empty'.
    <block>
    # opposite to while list_name: is while not list_name: (i.e. as long as the list IS empty)
while a < b:
    <block>
while a not in b:
    <block>


