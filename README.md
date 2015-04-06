# BlindDate
Date-Time Conversion Utility for R

## Motivation, Description, and Scope
Despite useful packages such as lubridate and zoo, dates and times still remain challenging to work with in R.
I encountered a challenge where my data source would not reliabily return the same date-time format.
So I looked for a package that would 'guess' the date-time format.
While {lubridate}'s guess_formats() is a start, it still requires you to supply the order of the month, day, and year sub-objects.
That didn't work with the formats I was receiving, however this project uses guess_formats() for delimiter parsing.
I also tried {TimeDate}'s whichFormat() but it did not work for me.



### My employer, Symantec, requires the following notice:
This initial contribution was made by Symantec as Symantec Open Source under the MIT License.
The project name is BlindDate.
Scott Kaiser is individual contributor who created the code.
This code was contributed on April 6, 2015.
