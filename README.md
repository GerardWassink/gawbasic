# gawbasic

## Basic interpreter in Rexx
As an exercise in coding an interpreter for another language, I chose to make an interpreter for the Basic language, much along the lines of one of the earlier dialects like Commodore 64 basic.

## Line commands

### implemented

#### NN program text
Any number with BASIC text behind it will eneter that text as an extra line in the program memory

#### LIST
Show an onscreen listing of the program lines you entered

#### RENUM [number]
renumbers the lines in program memory, starting at 'number' and using 'number as increment

#### DEL [fromLine [toLine] ]
delete all lines from and including 'fromLine' up to and including 'toLine'

#### NEW
Erase the programlines from memory

#### SAVE fileName
saves all lines from program memory to a file named fileName.bas

#### LOAD fileName
loads a file named fileName.bas into program memory

#### EXIT
Leave the program

### To come


