# -*- coding: utf-8 -*-
"""
Created on Fri Jun 30 10:19:12 2017

@author: User
"""

# By Websten from forums
#
# Given your birthday and the current date, calculate your age in days.
# Account for leap days.
#
# Assume that the birthday and current date are correct dates (and no
# time travel).
#

# Helper procedures check assumptions in code.
# eg. is the year a leap year?
def is_leap_year(year):
    if year % 4 == 0:
        if year % 100 == 0 and year % 400 > 0:
            return False
        else:
            return True
    else:
        return False

days_per_month      = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
days_per_month_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

# Count total days in whole year
def days_in_whole_year(year):
    leap = is_leap_year(year)
    if leap:
        return sum(days_per_month_leap)
    else:
        return sum(days_per_month)

# N days alive since January 1st (i.e. N days alive in current year)
def days_since_jan01(year, month, day):
    leap = is_leap_year(year)
    monthpos = month-1
    if leap:
        return sum(days_per_month_leap[0:monthpos]) + day
    else:
        return sum(days_per_month[0:monthpos]) + day

# N days alive in birth year (i.e. N days from birth to Dec 31st)
def days_alive_in_birthyear(year, month, day):
    return days_in_whole_year(year) - days_since_jan01(year, month, day)

# Final procedure:
def daysBetweenDates(year1, month1, day1, year2, month2, day2):
    if year1 == year2:
        return days_since_jan01(year2, month2, day2) - days_since_jan01(year1, month1, day1)
    else:
        # N days alive in (incomplete) first year (N days from birth to 31st Dec)
        days_lived_year1 =  days_alive_in_birthyear(year1, month1, day1)

        # N days alive in (incomplete) last year (N days from Jan01 to current date)
        days_lived_year2 = days_since_jan01(year2, month2, day2)

        # Calculate total days alive in year1 and year2
        total_days = days_lived_year1 + days_lived_year2

        # Calculate N days alive in all the complete years in between (if any)
        if year2 - year1 >= 0:
            year = year1 + 1
            while year < year2:
                total_days = total_days + days_in_whole_year(year)
                year = year + 1
            else:
                return total_days
        else:
            return total_days

# Testing:
daysBetweenDates(2012,1,1,2012,2,28)
daysBetweenDates(2012,1,1,2012,3,1)
daysBetweenDates(2011,6,30,2012,6,30)
daysBetweenDates(2011,1,1,2012,8,8)
daysBetweenDates(1900,1,1,1999,12,31)
daysBetweenDates(1987,6,25,2017,6,30)  # I've been alive for 10,963 days
daysBetweenDates(1987,4,28,2017,6,30)

# -----------------------------------

# Dave's solution

# Define a daysBetweenDates procedure that would produce the
# correct output if there was a correct nextDay procedure.
#
# Note that this will NOT produce correct outputs yet, since
# our nextDay procedure assumes all months have 30 days
# (hence a year is 360 days, instead of 365).

# Define leap years
# based on code from wikipedia and written by Dave:
def is_leap_year(year):
    if year % 400 == 0:
        return True
    if year % 100 == 0:
        return False
    if year % 4:
        return True
    return False

# define correct number of days in each month, accounting for leap years
def daysInMonth(year, month):
    if month in (1,3,5,7,8,10,12):
        return 31
    if month == 2:
        if is_leap_year(year):
            return 29
        else:
            return 28
    else:
        return 30
# test
daysInMonth(2001, 2)

# define nextDay procedure that returns the date of the next day after a given date.
# This calls the daysInMonth procedure that accounts for different month-lengths & leap years.
def nextDay(year, month, day):
    if day < daysInMonth(year, month):
        return year, month, day + 1
    else:
        if month == 12:
            return year + 1, 1, 1
        else:
            return year, month + 1, 1

# Helper Procedure to determine if the first date is before the second date:
# Helper procedures check assumptions
def dateIsBefore(year1, month1, day1, year2, month2, day2):
    """Returns True if year1-month1-day1 is before year2-month2-day2. Otherwise, returns False."""
    if year1 < year2:
        return True
    if year1 == year2:
        if month1 < month2:
            return True
        if month1 == month2:
            return day1 < day2
    return False

# The final code to count no. days between two dates
def daysBetweenDates(year1, month1, day1, year2, month2, day2):
    """Returns the number of days between year1/month1/day1
       and year2/month2/day2. Assumes inputs are valid dates
       in Gregorian calendar."""
    # check (assert) that the 2nd date > 1st date. If not, stop the procedure.
    assert not dateIsBefore(year2, month2, day2, year1, month1, day1)
    daysBetween = 0
    # call the dateIsBefore procedure to check 1st date is still before the 2nd date, and
    # keep running the loop as long as date 1 < date 2
    while dateIsBefore(year1, month1, day1, year2, month2, day2):
        # update year1, month1 and day1 to the next days
        year1, month1, day1 = nextDay(year1, month1, day1)
        # add 1 to number of days
        daysBetween += 1
    return daysBetween

# Test the final code for daysBetweenDates
def test():
    test_cases = [((2012,1,1,2012,2,28), 58),
                  ((2012,1,1,2012,3,1), 60),
                  ((2011,6,30,2012,6,30), 366),
                  ((2011,1,1,2012,8,8), 585 ),
                  ((1900,1,1,1999,12,31), 36523)]

    for (args, answer) in test_cases:
        result = daysBetweenDates(*args)
        if result != answer:
            print("Test with data:", args, "failed")
        else:
            print("Test case passed!")

test()  # all passed!

