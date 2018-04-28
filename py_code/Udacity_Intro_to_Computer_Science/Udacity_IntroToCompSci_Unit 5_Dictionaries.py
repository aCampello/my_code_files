# -*- coding: utf-8 -*-
"""
Created on Tue Jul 11 21:33:05 2017

Udacity Intro to Comp Sci Unit 5: Lesson 18: How Programs Run

Functions to lookup and modify dictionaries

"""

# Dictionaries of Dictionaries (of Dictionaries)

# The next several questions concern the data structure below for keeping
# track of Udacity's courses (where all of the values are strings):

#    { <hexamester>, { <class>: { <property>: <value>, ... },
#                                     ... },
#      ... }

# For example,

courses = {
    'feb2012': { 'cs101': {'name': 'Building a Search Engine',
                           'teacher': 'Dave',
                           'assistant': 'Peter C.'},
                 'cs373': {'name': 'Programming a Robotic Car',
                           'teacher': 'Sebastian',
                           'assistant': 'Andy'}},
    'apr2012': { 'cs101': {'name': 'Building a Search Engine',
                           'teacher': 'Dave',
                           'assistant': 'Sarah'},
                 'cs212': {'name': 'The Design of Computer Programs',
                           'teacher': 'Peter N.',
                           'assistant': 'Andy',
                           'prereq': 'cs101'},
                 'cs253': {'name': 'Web Application Engineering - Building a Blog',
                           'teacher': 'Steve',
                           'prereq': 'cs101'},
                 'cs262': {'name': 'Programming Languages - Building a Web Browser',
                           'teacher': 'Wes',
                           'assistant': 'Peter C.',
                           'prereq': 'cs101'},
                 'cs373': {'name': 'Programming a Robotic Car',
                           'teacher': 'Sebastian'},
                 'cs387': {'name': 'Applied Cryptography',
                           'teacher': 'Dave'}},
    'jan2044': { 'cs001': {'name': 'Building a Quantum Holodeck',
                           'teacher': 'Dorina'},
                        'cs003': {'name': 'Programming a Robotic Robotics Teacher',
                           'teacher': 'Jasper'},
                     }
    }

# lookup a course in a specific hexamester:
'cs001' in courses['jan2044']  # returns True if it is run in that semester.

# lookup a course, e.g. cs101, and return all hexamesters when it is run.
def when_offered(courses, course):
    result = []
    for hexamester in courses:          # loop through each hexamester in the courses dictionary.
        if course in courses[hexamester]:   # if the course is in the dictionary for that hexamester
            result.append(hexamester)          # append the course to the result list
    return result
# test
when_offered(courses, 'cs101') # prints the two hexamesters when cs101 is run.


# List courses offered in a given hexamester
def courses_offered(courses, hexamester):
    result = []
    for c in courses[hexamester]:   # for <key> in <dictionary>
        result.append(c)
    return result


# See which hexamester(s) a given course is offered in.
def when_offered(courses,course):
    offered = []
    for hexamester in courses:
        if course in courses[hexamester]:
            offered.append(hexamester)
    return offered

# test
print(when_offered (courses, 'cs101'))  # ['apr2012', 'feb2012']
print(when_offered(courses, 'bio893')) # []


# see is name Jasper appears in info about course cs003 (just a test)
'Jasper' in courses['jan2044']['cs003']['teacher']

# See which courses a given person is involved with, and when they run.
# Returns a dictionary with hexamesters as its keys and a list of courses as each value.
def involved(courses, person):
    result = {}
    for hexamester in courses:                  # loop through each hexamester
        for course in courses[hexamester]:      # loop through each course in the hexamester
            for key in courses[hexamester][course]: # loop through each key-value pair in the course
                if courses[hexamester][course][key] == person:  # if the person's name appears:
                    if hexamester not in result:        # if hexamester is already in the result list:
                        result[hexamester] = [course]   # ...create new list with the course as first entry.
                    else:
                        result[hexamester].append(course)  # ...otherwise, append course to existing list.
    return result

# test
print(involved(courses, 'Dave'))
# {'apr2012': ['cs101', 'cs387'], 'feb2012': ['cs101']}
