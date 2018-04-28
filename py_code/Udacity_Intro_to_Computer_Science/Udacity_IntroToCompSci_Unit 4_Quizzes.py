# -*- coding: utf-8 -*-
"""
Created on Tue Jul  4 15:19:26 2017

Udacity Unit 4 Responding to Queries: Quizzes
"""

# Define a procedure, add_to_index, that takes 3 inputs:

# - an index: [[<keyword>,[<url>,...]],...]
# - a keyword: String
# - a url: String

# If the keyword is already
# in the index, add the url
# to the list of urls associated
# with that keyword.

# If the keyword is not in the index,
# add an entry to the index: [keyword,[url]]


# The structure of index is a list of lists. Each list ('entry') has 2 elements: a keyword and a list of URLs where that keyword appears.
# So each entry is a list [] containing a keyword and a list of URLs: [keyword,[url]].
# In each list the keyword is at index 0, so entry[0], and the list of URLs is at index 1, so entry[1].
index = [[keyword1, [url1, url2, url3]],
         [keyword2, [url1]],
         [keyword3, [url1, url2]],
         [keyword4, [url1, url2, url3]]]



# Procedure to add more entries to the index.
def add_to_index(index,keyword,url):
    """ Add new urls and keywords to an index to keep
    track of which pages contain which keywords """
    for entry in index:                  # search each entry in the index
        if entry[0] == keyword:          # if keyword in the entry matches the keyword in the command:
           entry[1].append(url)          # add the url to the existing list of URLs within that entry
           return                        # 'return' on its own ends the whole procedure (not just the loop).
    index.append([keyword,[url]])        # if reach end of loop then index doesn't contain the keyword
                                         # so it's added as a new entry on the end of the list, in the
                                         # matching lists-within-list format.
# Testing
# create index as an empty list
index = []
print(index)

# then add stuff
add_to_index(index,'udacity','http://udacity.com')
print(index)

add_to_index(index,'computing','http://acm.org')
print(index)

add_to_index(index,'udacity','http://npr.org')
print(index)
# Result: [['udacity', ['http://udacity.com', 'http://npr.org']], ['computing', ['http://acm.org']]]


""" ------------------------ Next Problem ------------------------ """

### Lesson 16 Problem Set, Quiz 4.

# Define a procedure to take a string (source) and a list of punctuation marks on which
# to split the string (splitlist). Split the string and return the separated words.
def split_string(source, splitlist):
    """ Split the string based on characters in the splitlist
    return the output in a list"""
    result = []                 # to store the output
    atsplit = True              # Boolean to mark whether we're at a split point.
    for char in source:         # loop through each character in the string
        if char in splitlist:   # if the character's in the splitlist set atsplit to True.
            atsplit = True      # The procedure skips the else below and loops back to this if statement
                                # and if the next character's not in splitlist it'll execute the else statement:
        else:
            if atsplit:         # if atsplit is True, as set by the if statement above, then append the character to result.
                result.append(char)
                atsplit = False     # and reset atsplit to False.
            else:
                result[-1] = result[-1] + char  # add character to previous word (instead of starting a new one).
    return output

""" ------------------------ Next Problem ------------------------ """

