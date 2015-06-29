# BlindDate
Date-Time Conversion Utility for R

## Motivation and Description
Despite useful packages like lubridate and zoo, dates and times still remain challenging to work with in R.
I encountered a challenge where my data source would not reliabily return the same date-time format.
So I looked for a package that would 'guess' the date-time format.
While {lubridate}'s guess_formats() is a start, it still requires you to supply the order of the month, day, and year sub-objects.
The formats I receive lack a predictable order. This project does use {lubridate}'s parse_date_time() to parse delimiters.
I also tried {TimeDate}'s whichFormat() but it did not work for my data formats.

## Scope
This project focuses on figuring out date-time formats from about 1970 forward.
It does not aim to guess geologic time formats, nor dates with years more or less than four digits.
If all dates occur on a single day on a single month, it will not work if the day is on or before the 12th.
Similarly, if 2 digit years are used there are conditions in which the format will not be guessed -- say if all data is on 12/12/12.

## Methodology
The basic idea is to parse the string into a series of date/time subcomponents,
and then assign each component to the right category (month, year, etc.)
using contextual clues: four digits are most likely a year;
a subcomponent that spans 1-31 inclusive is probably a day;
times are almost always in the order HMS; and so on.

## Getting Started
A [digraph of functional relationships](https://github.com/sckaiser/BlindDate/blob/master/Digraph_functions.png) is provided to help visualize the code; I update it on an ad hoc basis.

## Dependencies
lubridate
stringr

## Style
This project aims to follow Google's R Style Guide:
https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml


### My employer requires the following notice:
The initial contribution was made by Symantec as Symantec Open Source under the MIT License.
The project name is BlindDate.
Scott Kaiser is individual contributor who created the code.
This code was contributed on April 6, 2015.
