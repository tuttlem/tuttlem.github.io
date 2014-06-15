---
layout: post
title: UniVerse development cheatsheet
date: 2012-11-16
comments: false
categories: [ "UniVerse", "Development", "Cheatsheet", "Database" ]
---

A cheatsheet of UniVerse commands

### Command Prompt

| Action											   | Command 							  |
|------------------------------------------------------|-------------------------------------:|
| Quitting the session								   | `QUIT`								  |
| Logging off a session								   | `LO`								  |
| Breaking out into a shell							   | `SH`								  |
| Change to a different account 					   | `LOGTO accountname`				  |
| Start online help 								   | `HELP commandname`					  |
| Execute a UniVerse BASIC command 					   | `XEQ command` 						  |
| Echo a string out to screen 						   | `DISPLAY string` 					  |

### Command Stack

All of the items with an <em>"n"</em> in their execution can either be removed (to operate on the most recent item) or replaced with a number indicating the item in the stack the operation is to effect. 

| Action											   | Command 							  |
|------------------------------------------------------|-------------------------------------:|
| Viewing your command history						   | `.L` 								  |
| Executing a command from the stack 				   | `.Xn`								  |	
| Delete an item from the stack 					   | `.Dn`								  |
| Recall an older command to the head of the stack 	   | `.Rn`								  |
| Change a string in a command on the stack 		   | `.Cn/oldstring/newstring`			  |
| Change all occurences of a string in a command on the stack | `.Cn/oldstring/newstring/G`	  |
| Appending something to the end of a command on the stack 	  | `.An extra`					  |

### Files

Creating a new file

{% highlight text %}	
CREATE.FILE filename filetype other
{% endhighlight %}

Filenames should be uppercase with words divided by periods (.)

| File type | Type 					| Description
|-----------|-----------------------|----------------------------------------------------
| 1 & 19	| directory				|
| 2			| static				| Keys end with numbers.
| 3			| static				| Keys end mainly with numbers.
| 4			| static				| Keys end with letters.
| 5			| static				| Keys end with full range of ASCII characters.
| 6			| static				| Keys begin with numbers.
| 7			| static				| Keys begin mainly with numbers.
| 8			| static				| Keys begin with letters.
| 9			| static				| Keys begin with full range of ASCII characters.
| 10		| static				| Keys are numbers.
| 11		| static				| Keys are mainly numbers.
| 12		| static				| Keys are letters.
| 13		| static				| Keys are full range of ASCII characters.
| 14		| static				| Entire keys are numbers.
| 15		| static				| Entire keys are mainly numbers.
| 16		| static				| Entire keys are letters.
| 17		| static				| Entire keys are full range of ASCII characters.
| 18		| static				| Entire keys are of arbitrary form.
| 30		| dynamic				|

`other` depends on the filetype being created Creating a new directory file
	
{% highlight text %}	
CREATE.FILE filename 1
CREATE.FILE filename 19
{% endhighlight %}

Creating a new static file
	
{% highlight text %}	
CREATE.FILE filename (2-18) modulo separation
{% endhighlight %}

`modulo` is the number of groups the file has. 
`separation` is the number of 512 bytes disk frames are allocated to each group 

Maintaining a file
	
{% highlight text %}	
RESIZE filename newtype newmodulo newseparation
{% endhighlight %}

Creating a dynamic file
	
{% highlight text %}	
CREATE.FILE filename 30
{% endhighlight %}

Copy a file
	
{% highlight text %}	
COPY FROM sourcefile TO destfile ALL OVERWRITING
{% endhighlight %}

### Editing Records 

| Action											   | Command 							  |
|------------------------------------------------------|-------------------------------------:|
| Invoking the Editor 								   | `ED filename recordkey`			  |
| Display a pageful of the record 					   | `P`								  |
| Qutting a record 									   | `Q`								  |
| Inserting lines 									   | `I`								  |
| Moving between lines								   | `n`    (move to line n)			  |
|													   | `+n`   (move forward n lines)		  |
|													   | `-n`   (move backward n lines)		  |
|													   | `t`    (move to top)				  |
|													   | `b`    (move to bottom)			  |
| Changing lines (/G for global) 					   | `C/oldstring/newstring/G`			  |
| Undo 												   | `OOPS`								  |
| Appending to a line 								   | `A extra-string`					  |
| Breaking a line 									   | `B word-to-break-on`				  |
| Concatenating lines 								   | `CAT`								  |
| Deleting lines 									   | `D`								  |
| Duplicating lines 								   | `DUP`								  |
| Locating and finding 								   | `L something`						  |
|													   | `F something`						  |
| Setting editing blocks							   | `<`  (sets the starting block)		  |
|													   | `>`  (sets the ending block)		  |
| Copy, move or drop blocks 						   | `COPY`								  |
|													   | `MOVE`								  |
|													   | `DROP`								  |
| Saving your record 								   | `SAVE`								  |

### Editor Macros

Creating a new ED macro 							   

`@FILE` variable can be used to be the existing file being edited. `@ID` variable can be used to be the key of the current record being edited. Example usage of `@FILE` and `@ID` within a macro
	
{% highlight text %}	
> ED &ED& MACRONAME				  
----: I
0001= E
0002= SAVE
0003= XEQ BASIC @FILE @ID
0004= XEQ RUN @FILE @ID
0005=
{% endhighlight %}

### The VOC File 

Creating a verb (command word)
	
{% highlight text %}	
> ED VOC verbname
----: I
0001= V
0002= programname
0003= type
0004=
{% endhighlight %}

All verbs start with a V. The name of the program to execute is specified by the second line. The type of the program is specified on the third line. Possible options are:

| Letter | Language
|--------|--------------------
| B		 | UniVerse Basic
| C		 | C shell script
| D		 | DOS batch file
| E		 | External
| I		 | Internal
| P		 | Primitive
| Q		 | Query command
| S		 | Bourne sheel script
| B  	 | Operating system command

Creating a keyword (parameter or modified applied to verbs)
	
{% highlight text %}	
> ED VOC keywordname
----: I
0001= K
0002= value
0003= verbcommand (optional)
{% endhighlight %}

Keywords start with a K. Their second parameter is the value that gets substituted for the word. Creating a paragraph (script)
	
{% highlight text %}	
> ED VOC paragraphname
----: I
0001= PA
0002= script ....
0003= script ....
{% endhighlight %}

Paragraphs start with PA, then continue to define the script itself. For variables that are unknown at runtime the <<...>> syntax can be used to prompt the user inline for the value. Creating a sentence (a one line paragraph)
	
{% highlight text %}	
> ED VOC sentencename
----: I
0001= PA
0002= script ....
{% endhighlight %}

Creating a file pointer
	
{% highlight text %}	
> ED VOC filename
----: I
0001= F
0002= directoryname
0003= filename
{% endhighlight %}

Creating a qpointer
	
{% highlight text %}	
> ED VOC synonymname
----: I
0001= Q
0002= (space)
0003= filename
{% endhighlight %}

Creating a remote command
	
{% highlight text %}	
> ED VOC remotecommandname
----: I
0001= R
0002= filename that contains the command record
0003= key of the command record
{% endhighlight %}

### Dictionaries

A description of common fields for a dictionary record

| Field 					| Description
|---------------------------|--------------------------------------------------
| Conversion 				| Blank unless a conversion is required. e.g. D DMY[2,A3,4] would store 1 for 01 JAN 1968
| Column Header 			| Title that appears at the head of the column
| Format 					| The number of characters to display and alignment. 10R - 10 characters, right aligned. 
|							| 10T - 10 characters, text aligned. 10L - 10 characters, left aligned.
| Single or Multi			| S for single value, M for multi-value

Creating a data field

{% highlight text %}	
> ED DICT filename fieldname
----: I
0001= D Description of the field
0002= field number
0003= conversion
0004= header
0005= format
0006= single or multi
{% endhighlight %}

Creating an imaginary field

{% highlight text %}		
> ED DICT filename fieldname
----: I
0001= I Description of the field
0002= formula
0003= conversion
0004= header
0005= format
0006= single or multi
{% endhighlight %}

Creating a phrase field
	
{% highlight text %}		
> ED DICT filename fieldname
----: I
0001= PH Description of the field
0002= fieldnames
{% endhighlight %}

### Indexing Fields 

| Action 									| Command
|-------------------------------------------|--------------------------------------------------
| Creating an index 						| `CREATE.INDEX filename fieldname`
| Bringing an index up to date 				| `BUILD.INDEX filename fieldname`
| Checking indexes on a file 				| `LIST.INDEX filename fieldname/ALL`
| Removing an index from a file 			| `DELETE.INDEX filename fieldname`

### RetrieVe

| Action 									| Command
|-------------------------------------------|--------------------------------------------------
| A basic list 								| `LIST filename`
| Listing fields 							| `LIST filename field1 field2 field3`
| Sorting by fields 						| `LIST filename field1 field2 BY field3`
|											| `LIST filename field1 BY field2 BY field3`
| 											| `LIST filename field1 field2 BY.DSND field3`
| 											| `SORT filename BY field1`
| Sampling a list 							| `LIST filename SAMPLE n`
| Listing with criteria 					| `LIST filename field1 field2 WITH field3 = 'data'`
| Totalling a field 						| `LIST filename field1 field2 TOTAL field3`
| Setting a breakpoint 						| `LIST filename BREAK.ON field1 field2 field3`
| Summarising a report 						| `LIST filename BREAK.ON field1 field2 DET.SUP`
| Providing headers and footers 			| `LIST filename field1 field2 field3 HEADING "report heading" FOOTING "report footing"`
| Overriding existing dictionary definitions | `LIST filename field1 FMT "override format"`
| 											 | `LIST filename field1 CONV "new conversion"`
| 											 | `LIST filename field1 COL.HDG "new heading"`
| 											 | `LIST filename field1 SINGLE.VALUE`
| 											 | `LIST filename field1 MULTI.VALUE`
| Columns without dictionary entries 		| `LIST filename EVAL "formula"`

### Criteria

| Symbol 	| Synonyms 						| Description
|-----------|-------------------------------|----------------------------------------------------
| `=` 		| `EQ` 							| Equal to
| `#` 		| `<>`,`>;<`,`NE`						| Not equal to
| `>` 		| `GT`, `AFTER` 					| Greater than
| `<` 		| `LT`, `BEFORE` 					| Lesser than
| `>=` 		| `=>`, `GE` 						| Greater than or equal to
| `<=` 		| `=<`, `LE` 						| Lesser than or equal to
| `LIKE` 		| `MATCHES`, `MATCHING` 			| Finds a matching word (... is a wildcard)
| `UNLIKE`	| `NOT.MATCHING` 					| Finds not matching word (... is a wildcard)
| `SPOKEN`	| `SAID`, `~` 						| Phonetic matching

Criteria can be reversed with NOT and concatenated with AND or OR. 

### Select Lists

| Action 									| Command
|-------------------------------------------|--------------------------------------------------
| Creating a select list 					| `SELECT filename`
| Numbered select lists 					| `SELECT filename TO n`
| Saving a select list 						| `SELECT filename`
| 											| `SAVE.LIST listname FROM n`
| Getting a saved list 						| `GET.LIST listname TO n`
| Clearing a list 							| `CLEARSELECT n`

### TCL Commands 

| Action 									| Command
|-------------------------------------------|--------------------------------------------------
| Count the records in a file 				| `COUNT filename`
| Copy a record to the terminal (display a record) | `CT filename record`
| Copy a record to the printer				| `CP filename record`
| Copy records to another file 				| `COPY FROM sourcefile TO destfile record1 record2 record3`
| 											| `COPY FROM sourcefile TO destfile ALL`
| Change the name of a file 				| `CNAME oldname TO newname`
| Change the key of a record in a file 		| `CNAME filename oldrecord, newrecord`
| Swap the contents of two records 			| `EXCHANGE filename record1 record2`
| Delete records from a file 				| `DELETE filename record1 record2`
| Remove all records from a file 			| `CLEAR.FILE filename`
| Clear the terminal 						| `CS`

### References

<a href="http://www.mannyneira.com/universe/">life, the universe, and everything</a>