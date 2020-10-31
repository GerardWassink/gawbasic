#!/usr/bin/rexx

/* -------------------------------------------------------------------------- */
/* Program name:    gawbasic.rexx                                             */
/* Author:          Gerard Wassink                                            */
/* Date:            September 2019                                            */
	versionString = "0.1"
/* Purpose:         Practise writing interpreters. learning to understand the */
/*                  inner workings of a computer language                     */
/*                                                                            */
/* History:                                                                   */
/*   v0.1     First setup skeleton for entering and listing the program       */
/*            Built in the RENUM, LIST command                                */
/*            Built in the DEL, NEW, SAVE, LOAD commands                      */
/*   v0.2     Entered function table                                          */
/*            Coded RUN and CONT statements                                   */
/*                                                                            */
/* -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------- */
/* ----- Main routine, calling init, process and End ------------------------ */
/* -------------------------------------------------------------------------- */
Main:
	If Initialize() Then
		Call Processing
	Call ProgramEnd
Exit


/* -------------------------------------------------------------------------- */
/* ----- Processing routine, ------------------------------------------------ */
/* -------------------------------------------------------------------------- */
Processing:
	Do Until cmd = "EXIT"
		input = Strip(lineIn())
		cmd = Upper(Word(input, 1))
		
		/* ------------------------------------------------------------------ */
		/* --------------------------------------------- Process commands --- */
		/* ------------------------------------------------------------------ */

		Select
			When DataType(cmd,"W") Then Call Addline(input)	/* - add line --- */
			
			When cmd == "LIST"	Then 	Call listProgram(input)

			When cmd == "RENUM"	Then 	Call renumProgram(input)
			
			When cmd == "DEL"	Then 	Call deleteLines(input)

			When cmd == "NEW"	Then 	Call newProgram

			When cmd == "SAVE"	Then 	Call saveProgram(input)

			When cmd == "LOAD"	Then 	Call loadProgram(input)

			When cmd == "RUN"	Then 	Call runProgram(input)

			When cmd == "CONT"	Then 	Call contProgram(input)

			When cmd == "IX"	Then 	Call indexMe

			When cmd == "EXIT"	Then Do
				cmd = ""
				Leave
			End
			
			Otherwise Do
				Say "Unrecognized input" input
			End
		End
	End
Return


/* -------------------------------------------------------------------------- */
/* --- runProgram routine --------------------------------------------------- */
/* -------------------------------------------------------------------------- */
runProgram:
	If program.0 > 0 Then Do
		Call sortProgram
		curLine = 0
		Call execProgram
	End; Else Do
		Say "No program in memory"
	End
Return


/* -------------------------------------------------------------------------- */
/* --- contProgram routine -------------------------------------------------- */
/* -------------------------------------------------------------------------- */
contProgram:
	If program.0 > 0 Then Do
		Call sortProgram
		If curLine <= program.0 Then Do
			Call execProgram
		End; Else Do
			Say "Can't continue, end of program reached"
		End
	End; Else Do
		Say "No program in memory"
	End
Return


/* -------------------------------------------------------------------------- */
/* --- execProgram routine -------------------------------------------------- */
/* -------------------------------------------------------------------------- */
execProgram:
	Do Until ( (curLine >= program.0) | (stmt == "STOP") | (stmt == "END") )
		curLine = curLine + 1
		execLine = program.curLine
		Parse Var execLine lineNum stmt rest
		stmt = Upper(stmt)
		Call execStatement(execLine)
	End
Return


/* -------------------------------------------------------------------------- */
/* --- execStatement routine ------------------------------------------------ */
/* -------------------------------------------------------------------------- */
execStatement:
	Parse Var execLine lnum verb rest
	verb = Upper(verb)
	Select
		When verb == "CLR" Then Do
			Say "CLR statement"
		End
		When verb == "END" Then Do
			Say "END statement"
		End
		When verb == "STOP" Then Do
			Say "STOP statement"
		End
		When verb == "REM" Then Do
			                               /* --- Do nothing for comments --- */
			Say "REM statement"
		End
		When verb == "PRINT" Then Do
			Say "PRINT statement"
		End
		When verb == "GOTO" Then Do
			Say "GOTO statement"
			nNum = Word(rest,1)
			curline = findLnum(nNum) - 1	/* will be incremented in execProgram loop */
		End
		Otherwise Do
			Say "Execution halted, syntax error in line" lnum "- Incorrect statement:" verb
			curLine = program.0 + 1
		End
	End
Return rc


/* -------------------------------------------------------------------------- */
/* --- findLnum routine ----------------------------------------------------- */
/* -------------------------------------------------------------------------- */
findLnum:
	Parse Arg l .
	l = Right(l, 6, "0")
	rc = program.0 + 1
	Do progPtr = 1 to program.0
		If l == Word(program.progPtr, 1) Then Do
			rc = progPtr
			Leave
		End
	End
	If rc == (program.0 + 1) Then Do
		Say "GOTO line" l "not found, program halts in line" curLine
	End
Return rc


/* -------------------------------------------------------------------------- */
/* --- findStatement routine ------------------------------------------------ */
/* -------------------------------------------------------------------------- */
findStatement:
	Parse Arg s .
	rc = ""
	Do sNum = 1 to statement.0
		If s == Word(statement.sNum, 1) Then Do
			rc = s
			Leave
		End
	End
Return rc


/* -------------------------------------------------------------------------- */
/* --- listProgram routine -------------------------------------------------- */
/* -------------------------------------------------------------------------- */
listProgram: Procedure Expose program.
	Parse Arg input
	If program.0 > 0 Then Do
		Call sortProgram
		If Words(input) > 1 Then Do
			Parse Var input . fromLine toLine .
			If toLine == "" Then toLine = fromLine
		End; Else Do
			fromLine = 0
			toLine = 999999
		End
		fromLine = Right(fromLine,6,"0")
		toLine   = Right(toLine,6,"0")
		Do i = 1 to program.0
			lineNum = Word(program.i,1)
			If (lineNum >= fromLine) & (lineNum <= toLine) Then Do
				Say program.i
			End
		End
	End; Else Do
		Say "No program in memory"
	End
Return


/* -------------------------------------------------------------------------- */
/* --- renumProgram routine ------------------------------------------------- */
/* -------------------------------------------------------------------------- */
renumProgram: Procedure Expose program.
	Parse Arg input
	Call sortProgram
	incr = 10
	If Words(input) > 1 Then Do
		Parse Var input . incr rest
		If rest == "" Then Do
			If DataType(incr,"W") Then Do 
				Say "renumbering program with increments of" incr
				newLine = incr
				Do i = 1 to program.0
					lineNum = Right(newLine,6,"0")
					Parse Var program.i . restOfLine
					program.i = lineNum restOfLine
					newLine = newLine + incr
				End
			End; Else Do
				Say "increment value not numeric"
			End
		End; Else Do
			Say "Usage: renum [increment]"
		End
	End
Return


/* -------------------------------------------------------------------------- */
/* --- deleteLines routine -------------------------------------------------- */
/* -------------------------------------------------------------------------- */
deleteLines: Procedure Expose program.
	Parse Arg input
	Call sortProgram
	If Words(input) > 1 Then Do
		Parse Var input . fromLine toLine .
		If toLine == "" Then toLine = fromLine
	End; Else Do
		fromLine = 0
		toLine = 999999
	End
	fromLine = Right(fromLine,6,"0")
	toLine   = Right(toLine,6,"0")
	Do i = program.0 to 1 by -1
		lineNum = Right(Word(program.i,1),6,"0")
		If (lineNum >= fromLine) & (lineNum <= toLine) Then Do
			Do j = i to (program.0 - 1)
				k = j + 1
				program.j = program.k
			End
			program.0 = program.0 - 1
		End
	End
Return


/* -------------------------------------------------------------------------- */
/* --------------------------------------------------- newProgram routine --- */
/* -------------------------------------------------------------------------- */
newProgram:
	drop program.
	program. = ""
	program.0 = 0
Return


/* -------------------------------------------------------------------------- */
/* ----- AddLine routine ---------------------------------------------------- */
/* -------------------------------------------------------------------------- */
AddLine: Procedure Expose program.
	Parse Arg programLine
	Parse Var programLine lnum programLine
	lnum = Right(lnum, 6, "0")
	programLine = lnum programLine
	max = program.0
	found = 0
	Do i = 1 to max
		If lnum == Word(program.i,1) Then Do
			program.i = programLine
			found = 1
		End
	End
	If found = 0 Then Do
		program.0 = program.0 + 1
		ix = program.0
		program.ix = programLine
	End
Return


/* -------------------------------------------------------------------------- */
/* ----- sortProgram routine ------------------------------------------------ */
/* -------------------------------------------------------------------------- */
sortProgram: Procedure Expose program.
	max = program.0
	Do While (1==1)
		Do i = 1 to (max - 1)
			j = i + 1
			If program.i > program.j Then Do
				temp = program.i
				program.i = program.j
				program.j = temp
			End
		End
		max = max - 1
		If max < 1 Then Leave
	End
Return


/* -------------------------------------------------------------------------- */
/* ----- Save Program to disk -------------------------------- saveProgram--- */
/* -------------------------------------------------------------------------- */
saveProgram:
	Procedure Expose program.
	Parse Arg input
	Parse Var input cmd fileName .
	
	If fileName <> "" Then  Do
		fileName = fileName || ".bas"
		testValue = CheckOutputFileName(fileName)
		If testValue <> "" Then Do
			If Stream(fileName, 'C', 'OPEN WRITE REPLACE') = "READY:" Then Do
				Do i = 1 to program.0
					lc = Lineout(fileName, program.i)
				End
				Say "Saved program to" fileName 
			End; Else Do
				Say "Error writing to file" fileName
				Exit 8
			End
		End; Else Do
			Say "Not a valid filename" fileName
		End
	End; Else Do
		Say "No filename specified for SAVE command"
	End
Return


/* -------------------------------------------------------------------------- */
/* ----- Load Program from disk ------------------------------ loadProgram--- */
/* -------------------------------------------------------------------------- */
loadProgram:
	Procedure Expose program.
	Parse Arg input
	Parse Var input cmd fileName .
	
	If fileName <> "" Then  Do
		fileName = fileName || ".bas"
		testValue = CheckInputFileName(fileName)
		If testValue <> "" Then Do
			program.0 = 0
			program. = ""
			i = 0
			Do While Lines(fileName)
				line = Linein(FileName)
				i = i + 1
				program.i = line
			End
			program.0 = i
			Say "program" fileName "loaded"
		End; Else Do
			Say "Not a valid filename" fileName
		End
	End; Else Do
		Say "No filename specified for LOAD command"
	End
Return


/* -------------------------------------------------------------------------- */
/* ----- Check Input file name ----------------------- CheckInputFileName --- */
/* -------------------------------------------------------------------------- */
CheckInputFileName:
	Procedure Expose errorMsg
	Parse arg filename
	If Stream(filename, 'C', 'OPEN READ') == "READY:" Then Do
		errorMsg = "file" filename "exists, ready to read"
	End; Else Do
		errorMsg = "file can not be found:" filename
		filename = ""
	End
Return filename


/* -------------------------------------------------------------------------- */
/* ----- Check Output file name --------------------- CheckOutputFileName --- */
/* -------------------------------------------------------------------------- */
CheckOutputFileName:
	Procedure Expose errorMsg
	Parse arg filename
	If Stream(filename, 'C', 'OPEN WRITE') == "READY:" Then Do
		errorMsg = "file" filename "ready to write"
	End; Else Do
		errorMsg = "file not available for writing:" filename
		filename = ""
	End
	retCod = Stream(filename, 'C', 'CLOSE')
Return filename


/* -------------------------------------------------------------------------- */
/* ----- Initialisation routine --------------------------------------------- */
/* -------------------------------------------------------------------------- */
Initialize:
	rc = 1
	Say Left("--- gawbasic --- version" versionString "---" Date() "---", 72, "-")
	
	/* ------------------------------------------ array for program lines --- */
	program. = ""
	program.0 = 0
	
	/* ------------------------------------- array for program statements --- */
	/* --- Syntax description:                                            --- */
	/* ---    C      Constant argument can be N or A                      --- */
	/* ---    CS     Constant string argument                             --- */
	/* ---    FN     file name                                            --- */
	/* ---    LN     Line Number argument                                 --- */
	/* ---    N      Numerical argument                                   --- */
	/* ---    S      String argument                                      --- */
	/* ---    V      Undetermined variable name                           --- */
	/* ---    VN     Numeric variable name                                --- */
	/* ---    VS     String variable name                                 --- */
	/* ---    X      Expression                                           --- */
	/* ---    [ ]    Optional argument enclosed                           --- */
	/* ---    |      OR sign - i.e.  N|A : Numeric OR String              --- */
	/* ---    ...    elipses - last item can be repeated                  --- */
	
	statement. = ""
	statement.0 = 0
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "CLR"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "DATA C [, C] ..."
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "DIM V(N [, N] ...)"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "END"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "FOR VN = N TO N [STEP N]"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "GET V [, V] ..."
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "GOSUB NL"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "GOTO NL"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "IF X [THEN | GOTO] LN"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "INPUT [CS] V [, V] ..."
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "[LET] V = X"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "LOAD FN"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "NEW"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "NEXT [N] [, N] ..."
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "ON X [GOTO | GOSUB] LN [, LN] ..."
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "PRINT [X [[,|;] X] ...]"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "READ V [, V] ..."
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "REM arbitrary text"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "RESTORE"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "RETURN"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "SAVE FN"
	ix = statement.0 + 1; statement.0 = ix; statement.ix = "STOP"
	
	/* ------------------------------------- array for program functions --- */
	/* --- .1 is the input type -------------------------------------------- */
	/* --- .2 is the output type ------------------------------------------- */
	function. = ""
	function.0 = 0
	ix = function.0 + 1; function.0 = ix; function.ix = "ABS";		function.ix.1 = "N";		function.ix.2 = "N"
	ix = function.0 + 1; function.0 = ix; function.ix = "ASC";		function.ix.1 = "S";		function.ix.2 = "N"
	ix = function.0 + 1; function.0 = ix; function.ix = "CHR$";		function.ix.1 = "N";		function.ix.2 = "S"
	ix = function.0 + 1; function.0 = ix; function.ix = "COS";		function.ix.1 = "N";		function.ix.2 = "N"
	ix = function.0 + 1; function.0 = ix; function.ix = "INT";		function.ix.1 = "N";		function.ix.2 = "N"
	ix = function.0 + 1; function.0 = ix; function.ix = "LEFT$";	function.ix.1 = "S N";		function.ix.2 = "S"
	ix = function.0 + 1; function.0 = ix; function.ix = "LEN";		function.ix.1 = "S";		function.ix.2 = "N"
	ix = function.0 + 1; function.0 = ix; function.ix = "MID$";		function.ix.1 = "S N [N]";	function.ix.2 = "S"
	ix = function.0 + 1; function.0 = ix; function.ix = "RIGHT$";	function.ix.1 = "S N";		function.ix.2 = "S"
	ix = function.0 + 1; function.0 = ix; function.ix = "RND";		function.ix.1 = "[N]";		function.ix.2 = "N"
	ix = function.0 + 1; function.0 = ix; function.ix = "SIN";		function.ix.1 = "N";		function.ix.2 = "N"
	ix = function.0 + 1; function.0 = ix; function.ix = "SQR";		function.ix.1 = "N";		function.ix.2 = "N"
	ix = function.0 + 1; function.0 = ix; function.ix = "STR$";		function.ix.1 = "N";		function.ix.2 = "S"
	ix = function.0 + 1; function.0 = ix; function.ix = "TAN";		function.ix.1 = "N";		function.ix.2 = "N"
	ix = function.0 + 1; function.0 = ix; function.ix = "VAL";		function.ix.1 = "S";		function.ix.2 = "N"

Return rc


/* -------------------------------------------------------------------------- */
/* ----- End Program routine ------------------------------------------------ */
/* -------------------------------------------------------------------------- */
ProgramEnd:
	Say "Program gawbasic ends"
	Say Left("--- gawbasic --- version" versionString "---" Date() "---", 72, "-")
Return


/* -------------------------------------------------------------------------- */
/* ----- Index of labels in this Rexx file ---------------------- indexMe --- */
/* -------------------------------------------------------------------------- */
indexMe:
	Procedure
	lnum = 0
	longest = 0
	srcFile = "./gawbasic.rexx"			/* Read our own source -------------- */
	If Stream(srcFile, 'C', 'OPEN READ') = "READY:" Then Do
		i = 1
		Do While Lines(srcFile)
			line = Strip(Linein(srcFile))
			lnum = lnum + 1
			If (line <> "") Then Do
				w = Word(line,1)
				If (Right(w, 1) == ":") Then Do	/* Do we have a label? ------ */
					If Length(w) > longest Then longest = Length(w)
					index.i.1 = Right("      "||lnum, 6)
					index.i.2 = w
					index.0 = i
					i = i + 1
				End
			End
		End
		status = Stream(srcFile, 'C', 'CLOSE')
		Do i = 1 to index.0
			If (Length(index.i.2)/2) <> Trunc(Length(index.i.2)/2) Then index.i.2 = index.i.2||" "
			Say index.i.2 Copies(" .", Trunc((longest - Length(index.i.2)) / 2 )) index.i.1 
		End
	End; Else Do
		srcMsg = "Error opening file" srcFile
		Exit 8
	End
Return
