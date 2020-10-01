#!/usr/bin/rexx

/* -------------------------------------------------------------------------- */
/* Program name:    gawbasic.rexx                                             */
/* Author:          Gerard Wassink                                            */
/* Date:            September 2019                                            */
	versionString = "1.1"
/* Purpose:         Practise writing interpreters. learning to understand the */
/*                  inner workings of a computer language                     */
/*                                                                            */
/* History:                                                                   */
/*   v1.0     First setup skeleton for entering and listing the program       */
/*   v1.1     Built in the RENUM command                                      */
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
/* --- listProgram routine -------------------------------------------------- */
/* -------------------------------------------------------------------------- */
listProgram: Procedure Expose program.
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
	Do i = 1 to program.0
		lineNum = Word(program.i,1)
		If (lineNum >= fromLine) & (lineNum <= toLine) Then Do
			Say program.i
		End
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
/* ----- Initialisation routine --------------------------------------------- */
/* -------------------------------------------------------------------------- */
Initialize:
	rc = 1
	Say Left("--- gawbasic --- version" versionString "---" Date() "---", 72, "-")
	
	/* ----------------------------------------- array for program lines --- */
	program. = ""
	program.0 = 0
	
	/* -------------------------------------- array for program keywords --- */
	keywords. = ""
	keywords.0 = 0
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "CLR"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "DATA"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "DIM"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "END"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "FOR"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "GET"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "GOTO"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "GOSUB"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "IF"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "INPUT"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "LET"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "NEW"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "NEXT"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "ON"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "PRINT"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "READ"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "REM"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "RESTORE"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "RETURN"
	ix = keywords.0 + 1; keywords.0 = ix; keywords.ix = "STOP"
	
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
