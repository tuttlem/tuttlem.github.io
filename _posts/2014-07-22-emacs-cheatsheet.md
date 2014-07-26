---
layout: post
title: Emacs Cheatsheet
date: 2014-07-22
comments: false
categories: ["emacs", "cheatsheet"]
---

Today's post isn't quite a normal post. I'm going to use this as a place to put all of my notes while using [emacs](http://www.gnu.org/software/emacs/).

### Navigation

| Key      | Description			|
|----------|------------------------------------|
| `C-v`    | Page down				|
| `M-v`    | Page up				|
| `C-l`    | Move text around the cursor (center, top, bottom) |
| `C-p`	   | Previous line    	  	 	  |
| `C-n`	   | Next line				  |
| `C-b`	   | Backwards character		  |
| `C-f`	   | Forward character			  |
| `M-b`	   | Backwards word			  |
| `M-f`	   | Forward word			  |
| `C-a`	   | Beginning of line			  |
| `C-e`	   | End of line  			  |
| `M-a`	   | Beginning of sentence		  |
| `M-e`	   | End of sentence			  |
| `M-<`	   | Beginning of text			  |
| `M->`	   | End of text  			  |

### Editing

| Key      | Description			|
|----------|------------------------------------|
| `<DEL>`  | Delete character before cursor	|
| `C-d`	   | Delete character after cursor	|
| `M-<DEL>` | Kill word before cursor		|
| `M-d`	   | Kill word after cursor		|
| `C-k`	   | Kill from cursor to EOL		|
| `M-k`	   | Kill to the end of current sentence |
| `C-<SPC>` | Start selecting text		 |
| `C-w`	   | Kill selection   			 |
| `M-w`	   | Save region but don't kill it	 |
| `C-y`	   | Yank killed text			 |
| `M-y`	   | Cycle through previous kills	 |
| `C-/`	   | Undo  	   	    		 |


### Utility

| Key      | Description			|
|----------|------------------------------------|
| `C-g`	   | Exit current command		|
| `C-u`	   | Repeat command			|
| `M-x` `replace-string` | Find and replace	|
| `M-x` `recover-file`	 | Recover file backup	|


### Files and Buffers

| Key		| Description					|
|---------------|-----------------------------------------------|
| `C-x` `C-f`   | Find a file (for opening)			|
| `C-x` `C-s`	| Save current file				|
| `C-x` `C-b`	| List buffers 					|
| `C-x` `b`	| Switch to a buffer				|
| `C-x` `s`	| Save some buffers				|
| `C-x` `C-c`	| Quit emacs (prompt for saves)			|
| `C-x` `k`	| Kill buffer					|

### Windows and Frames

| Key		| Description					|
|---------------|-----------------------------------------------|
| `C-x` `1`	| Delete all but current window			|
| `C-x` `2`	| Split window horizontally			|
| `C-x` `3`	| Split window verticaly			|
| `C-x` `4` `C-f` | Find a file into a new window		|
| `C-x` `o`	| Move to other window				|
| `C-M-v`	| Scroll bottom window				|
| `M-x` `make-frame` | Create a new emacs frame			|
| `M-x` `delete-frame` | Remove the selected emacs frame	|

### Modes

| Key		| Description					|
|---------------|-----------------------------------------------|
| `M-x` `fundamental-mode` | Fundamental			|
| `M-x` `text-mode`	   | Human text mode			|

### Help

| Key		| Description					|
|---------------|-----------------------------------------------|
| `C-h` `m`	| Documentation on current major mode		|
| `C-h` `?`	| Get help on what you can get help on		|
| `C-h` `c`	| Get help on a command	       	    		|
| `C-h` `f`	| Get help on a function			|
| `C-h` `a`	| List commands by keyword			|
| `C-h` `i`	| Open manuals (open *info* buffer)		|


### Searching

| Key		| Description					|
|---------------|-----------------------------------------------|
| `C-s`		| Start forward incremental search		|
| `C-r`		| Start backward incremental search		|

When searching forwards, `C-s` will take you through all of the occurences of the search term that has been found. `<DEL>` takes you backwards. `<DEL>` starts to effect the search term once you've reached the first result.