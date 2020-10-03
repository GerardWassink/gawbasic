/* rexx */

/* -------------------------------------------------------------------------------------------------------- */
/* --- infix to postfix conversion, based on simple algorithm that does not take into account functions	--- */
/* --- Need tokenization, and then rework to shunting yard algorithm:									--- */
/* --- 	 https://en.wikipedia.org/wiki/Shunting-yard_algorithm											--- */
/* -------------------------------------------------------------------------------------------------------- */


Main:
	stack. = ""
	stackPtr = 0
	stack.0 = stackPtr
	
	functions = "ABS ASC CHR$ COS INT LEFT$ LEN MID RIGHT$ RND SIN SQR STR$ TAN VAL"
	
	Do Until expr == ""
		expr = Strip(lineIn())
		post = Infix2Postfix(expr)
		Say post
	End
	
Return

Infix2Postfix:
	Parse Arg infix
	postfix = ""
	Call push("#")
	
	Do i = 1 To Length(infix)
		sym = SubStr(infix, i, 1)
Say "processing" sym
		If \isOperator(sym) Then Do
			postfix = postfix sym
		End; Else Do
			If sym == "(" Then Do
				Call push(sym)
			End; Else Do
				If sym == ")" Then Do
					Do While stack.stackPtr <> "("
						postfix = postfix pop()
					End
					junk = pop()
				End; Else Do
					If (precedence(sym) > precedence(stack.stackPtr)) Then Do
						call push(sym)
					End; Else Do
						Do While (precedence(sym) <= precedence(stack.stackPtr))
							postfix = postfix pop()
						End
						Call push(sym)
					End
				End
			End
		End
	End
	Do While stack.stackPtr <> "#"
		postfix = postfix pop()
	End
Return postfix

precedence: Procedure
	Parse Arg symbol .
	Select
		When Pos(symbol, "+-") > 0		Then	Return 2
		When Pos(symbol, "*/") > 0		Then	Return 3
		When Pos(symbol, "^") > 0		Then	Return 4
		When Pos(symbol, "()#") > 0		Then	Return 1
		Otherwise								Return 0
	End
Return 0

isOperator: Procedure
	Parse Arg symbol .
	rc = (1==0)
	If Pos(symbol, "+-*/^()#") > 0 Then rc = (1==1)
Return rc

push:
	Parse Arg item .
	stackPtr = stackPtr + 1
	stack.stackPtr = item
	stack.0 = stackPtr
Say "pushed" item
Return

pop:
	rv = stack.stackPtr
	drop stack.stackPtr
	stackPtr = stackPtr -1
	stack.0 = stackPtr
	If stackPtr < 0 Then Do	
		Say "PANIC!!! - stackPtr less then ZERO"
		Exit
	End
Say "popped" rv
Return rv

