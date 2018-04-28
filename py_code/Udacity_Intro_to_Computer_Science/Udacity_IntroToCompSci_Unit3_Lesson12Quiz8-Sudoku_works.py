# -*- coding: utf-8 -*-
"""
Created on Mon Jul  3 17:35:37 2017

@author: User
"""

# Quite complicated so split into two separate functions to check the rows and then the columns:

# check rows
def check_rows(grid):
    """ check whether rows follow sudoku rules """
    n = len(grid)
    for row in grid:
        values = list(range(1, n+1))
        for num in row:
            # print(num)            # optional print statement for checking
            if num not in values:
                result = False
                break
            else:
                result = True
                while num in values:
                    values.remove(num)
    return result

# check columns
def check_cols(grid):
    """ check whether columns follow sudoku rules """
    n = len(grid)
    cycle = 0
    while cycle < n:
        char = [row[cycle] for row in grid] # create new list containing nth digit from each row(list)
        values = list(range(1, n+1))
        for i in char:                      # cycle through the new list
            if i in values:                 # if the digit is not in the list of values, break
                result = True
                #print(i,'true')            # optional print statement for checking
                while i in values:          # remove all instances of digit i from the list of values
                    values.remove(i)
            else:
                result = False
                #print(i, 'false')          # optional print statement for checking
        cycle = cycle + 1                   # +1 to the cycle number before re-starting the while loop
        if result == False:               # break the while loop as soon as result becomes False
            break
    return result


# combine check_cols and check_rows into same function
def check_sudoku(grid):
    if check_rows(grid) and check_cols(grid):   # if both return True...
        return True
    return False

# Testing
correct = [[1,2,3],
           [2,3,1],
           [3,1,2]]

incorrect = [[1,2,3,4],
             [2,3,1,3],
             [3,1,2,3],
             [4,4,4,4]]

incorrect2 = [[1,2,3,4],
             [2,3,1,4],
             [4,1,2,3],
             [3,4,1,2]]

check_sudoku(correct) # works
check_sudoku(incorrect) # works
check_sudoku(incorrect2) # works


# ===================

# Alternative solution posted after the quiz by the course leaders:

# Cycles through digits from 1 to n, counting the number of times each digit appears in each row & column.
# if count != 1 then return False.
def check_sudoku(grid):
    """ Udacity course leaders' code to check grid follows sudoku rules """
    n = len(grid)   # extract size of grid
    digit = 1       # which digit to look for first
    while digit <= n:   # go through each digit
        row = 0    # row number: could also be 'i'
        while row < n:    # go through each row and column
            row_count = 0
            col_count = 0
            col = 0    # column number: could also be 'j'
            while col < n:    # for each entry in the ith row/column
                if grid[row][col] == digit:     # if the digit in row i, col j is the same as 'digit',
                    row_count += 1              # increase row_count by 1.
                if grid[col][row] == digit:     # if the digit in col j, row i (same as row i, col j) is
                   col_count += 1               # the same as 'digit', increase col_count by 1.
                col += 1                        # move on to the next column and continue until all columns have been checked.
            if row_count != 1 or col_count != 1:
                return False    # if sudoku rules are broken after checking row i, eturn False and stop.
            else:
                row += 1  # otherwise, move to next row within the while i<n loop (same digit).
        digit += 1  # move on to the next digit within the while digit<=n loop.
    return True     # return True if nothing was wrong with the grid.

