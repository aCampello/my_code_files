# -*- coding: utf-8 -*-
"""
Created on Tue Jul  4 09:15:40 2017

Udacity Intro to Comp Sci Unit 3 Problem Sets / 'Homework'

Referring to rows and columns in matrices by looping through i and j (a.k.a. row and col).

"""

### Problem Set 2, Quiz 2: Symmetric Square

# A list is symmetric if the first row is the same as the first column,
# the second row is the same as the second column and so on. Write a
# procedure, symmetric, which takes a list as input, and returns the
# boolean True if the list is symmetric and False if it is not.

# symmetric list for testing
correct = [[1,2,3],     # correct[0] (row 1) / correct[0][0:4] (column 1-3, row 1)
           [2,3,1],     # correct[1]
           [3,1,2]]     # correct[2] (row 3) / correct[0:3][2] (column 1-3, row 3)

# asymmetric list for testing
incorrect = [[1,5,5],
           [2,3,1],
           [3,1,2]]

# testing how to refer to rows and columns in the list 'correct'
correct[0]                      # row 1
[row[0] for row in correct]     # = col 1 (got this line of code online)

correct[1]                      # row 2
[row[1] for row in correct]     # col 2

correct[2]                      # row 3
[row[2] for row in correct]     # col 3

# final code
def symmetric(grid):
    """ Check whether a grid 'p' is symmetrical """
    n = len(grid)       # get size of list
    x = 0
    while x < n:     # while row/col number is less than the length of the grid/list
        row = grid[x]
        col = [i[x] for i in grid]
        # print(row,col)                # optional print to check code
        if row == col:
            x += 1
        else:
            return False
    return True

# Test the code
symmetric(correct)      # works
symmetric(incorrect)    # works


""" ------------------------ Next Problem ------------------------ """


### Problem Set Optional 2, Quiz 1: Antisymmetric Square

# By Dimitris_GR from forums
# Modify Problem Set 31's (Optional) Symmetric Square to return True
# if the given square is antisymmetric and False otherwise.
# An nxn square is called antisymmetric if A[i][j] = -A[j][i] (i.e. Aij is equal to negative Aji)
# for each i=0,1,...,n-1 and for each j=0,1,...,n-1.

def antisymmetric(square):
    """ Check whether a grid square A is antisymmetric, defined as when square[i][j] == -square[j][i] """
    n = len(square)                                     # get size of list
    row = 0                                             # start with the first row
    while row < n:                                      # cycle through the rows
        col = 0                                         # start with the first column
        while col < n:                                  # cycle through the columns
            if square[row][col] != -square[col][row]:
                return False                            # stop if square is not antisymmetric
            else:
                col += 1                                # otherwise move on to the next column
        row += 1                                        # then move on to the next row until all rows've been searched.
    return True                                         # return True if nothing was wrong with the square.

# Test the code
as_correct = [[0, 0, 0],
              [0, 0, 0],
              [0, 0, 0]]

as_correct2 = ([[0, 1, 2],
                [-1, 0, 3],
                [-2, -3, 0]])

as_incorrect = [[0, 1, 2],
                [-1, 0, -2],
                [2, 2,  3]]

antisymmetric(as_correct)      # works
antisymmetric(as_correct2)    # works
antisymmetric(as_incorrect)    # works



""" ------------------------ Next Problem ------------------------ """


### Problem Set Optional 2, Quiz 2: Identity Matrix

# By Ashwath from forums
# Given a list of lists representing a n * n matrix as input,
# define a  procedure that returns True if the input is an identity matrix
# and False otherwise.

# An IDENTITY matrix is a square matrix in which all the elements
# on the principal/main diagonal are 1 and all the elements outside
# the principal diagonal are 0.
# (A square matrix is a matrix in which the number of rows
# is equal to the number of columns)

def is_identity_matrix(matrix):
    """ Check if matrix is an identity matrix: a square matrix
    with ones along the diagonal and zeroes elsewhere """
    n = len(matrix)
    digit = 1
    row = 0
    for row in range(n):              # check matrix is squar (with equal row and column length)
        if len(matrix[row]) != n:
            return False
    while row < n:                    # cycle through each row
        col = 0
        while col < n:                # cycle through each column
            if row == col and matrix [row][col] != 1:   # return False if diagonal values != 1.
                return False
            if row != col and (matrix[col][row] != 0 or matrix[row][col] != 0):
                return False
            col += 1
        row += 1
    return True

# Test Cases:
matrix1 = [[1,0,0,0],
           [0,1,0,0],
           [0,0,1,0],
           [0,0,0,1]]   # True.

matrix2 = [[1,0,0],
           [0,1,0],
           [0,0,0]]     # False (no zero in bottom right).

is_identity_matrix(matrix1) # works
is_identity_matrix(matrix2) # works


""" ------------------------ Next Problem ------------------------ """

# Numbers in lists by SeanMc from forums
# define a procedure that takes in a string of numbers from 1-9 and
# outputs a list with the following parameters:

# Every number in the string should be inserted into the list.
# If a number x in the string is less than or equal to the preceding number y,
#   the number x should be inserted into a sublist.
# Continue adding the following numbers to the sublist until reaching a
#    number z that is greater than the number y.
# Then add this number z to the normal list and continue.
# Hint - "int()" turns a string's element into a number

def numbers_in_lists(string):
    """ Takes a string of numbers from 1-9 and outputs a list, where:
        1) if x < preceding number, x is inserted into a sublist.
        2) All further numbers are added to this sublist until number
        z > preceding number...
        3) ... after which z is added to the normal list and the process continues. """
    result = [int(string[0])]       # add first letter of string to result
    sublist = []
    x = 1
    while x < len(string):
        if int(string[x]) <= int(string[x-1]):     # must convert string to integer to compare numbers
            sublist += [int(string[x])]             # save string character to a sublist
        else:
            if sublist:
                result.append(sublist)         # at end of loop, add sublist to result (if it contains something).
                sublist = []                                # empty the sublist ready for the next cycle.
            if int(string[x]) > int(string[x-1]):
                result += [int(string[x])]                      # plus command to concatenate with 'result'
        x += 1
    if len(sublist) != 0:               # at the end of code, add sublist to result (if it contains something).
        result.append(sublist)
    return result

# Test cases
string = '455532123266'
result = [4, 5, [5, 5, 3, 2, 1, 2, 3, 2], 6, [6]]
numbers_in_lists(string)

string = '123456789'
numbers_in_lists(string)
result = [1, 2, 3, 4, 5, 6, 7, 8, 9]

string = '543987'
numbers_in_lists(string)
result = [5,[4,3],9,[8,7]]


""" ------------------------ Next Problem ------------------------ """

### Frequency Analysis
#
# To analyze encrypted messages, to find out information about the possible
# algorithm or even language of the clear text message, one could perform
# frequency analysis. This process could be described as simply counting
# the number of times a certain symbol occurs in the given text.
# For example:
# For the text "test" the frequency of 'e' is 1, 's' is 1 and 't' is 2.
#
# The input to the function will be an encrypted body of text that only contains
# the lowercase letters a-z.
# As output you should return a list of the normalized frequency
# for each of the letters a-z.
# The normalized frequency is simply the number of occurrences, i,
# divided by the total number of characters in the message, n.

def freq_analysis(message):
    result = []
    letters = 'abcdefghijklmnopqrstuvwxyz'
    let = 0
    count = 0
    while let < len(letters):        # Loop once for each letter in alphabet.
        for char in message:            # for each character in the message.
            if char == letters[let]:    # if character = letter
                count += 1              # increment count by one
        result.append(count) # append the count to the result
        count = 0   # reset the count
        let += 1      # move to the next letter
        freq_list = [x/len(message) for x in result]    # normalise the counts
    return freq_list


# Test cases
print(freq_analysis("abcd"))
#>>> [0.25, 0.25, 0.25, 0.25, 0.0, ..., 0.0]

print(freq_analysis("adca"))
#>>> [0.5, 0.0, 0.25, 0.25, 0.0, ..., 0.0]

print(freq_analysis('bewarethebunnies'))
#>>> [0.0625, 0.125, 0.0, 0.0, ..., 0.0]