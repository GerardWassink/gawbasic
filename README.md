# gawbasic

## Basic interpreter in Rexx
As an exercise in coding an interpreter for another language, I chose to make an interpreter for the Basic language, much along the lines of one of the earlier dialects like Commodore 64 basic.

## Line commands

### implemented

NN
	Any number with BASIC text behind it will eneter that text as an extra line in the program memory

LIST
	Show an onscreen listing of the program lines you entered

RENUM [number]
	renumbers the lines in program memory, starting at 'number' and using 'number as increment

DEL [fromLine [toLine] ]
	delete all lines from and including 'fromLine' up to and including 'toLine'

EXIT
	Leave the program

### To come

SAVE programName
	Save the program as programName

LOAD programName
	load a program from a file called programName

NEW
	Erase the programlines from memory


