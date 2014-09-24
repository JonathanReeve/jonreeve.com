---
layout: post
title: ! 'A Concept for a Meta Programming Language'
category: Digital Humanities
tags: syntax programming
---

#The Problem
Programming languages are usually designed to use very small sets of special characters--usually only the ones that are visible on the keyboard. The advantages of this are that those symbols are always easy to find and type (they're printed on the keyboard), but the disadvantages are many. For one, programming mechanisms are so numerous that symbols usually have to be reused, even in the same language. Take, for example, the humble dollar sign ($): 

 * PHP - used as a prefix for variables, i.e. `$var`
 * jQuery - conventionally used to refer to jQuery itself, as in `$(this)`, but also prefixes jQuery object varibles
 * regex - used as a marker of the end of the line, in `/line-end$/`
 * BASH - used as a prefix for variables when they are referenced; is often the prompt for the user commandline
 * everywhere else - used to denote a dollar amount. 
 
Thus, programming something that uses two or more of these can be tricky. Consider writing a line of PHP that contains regex that contains a dollar amount. You'd end up with something like: `$var = preg_match('/\$\d$/', $s);`. You'd just have to know that the first dollar sign was denoting a variable, the second was a literal dollar sign used to denote dollars, the third was an indication of the place within the string, and the fourth was another variable. 

Consider also that you want to substitute `</i>` end tags for `</em>`s. Since `<` and `>` are special characters in regex denoting the ends of words, you have to escape those characters. Also, since `/` is commonly used as a pattern delimiter, you have to escape that, too. This leaves you with a mess of backslashes and slashes like this: `s/\<\/i\>/\<\/em\>`. This gets even worse if you want to use literal backslashes, or literal newlines represented by `\n`. Replacing Windows newlines with regular newlines using regex in PHP is a nightmare: `preg_replace("/\\\\r\\\\n/", "\n", $line)` 

#The Solution
These days, we aren't restricted to the characters typable on an 86-key keyboard. We can easily change our keyboard layouts, and even create our own. Text expansion tools, like vim's `iabbrev`, can allow you to easily type any character you want. With access to the full unicode set, then, can't we eliminate some redundancy with these special characters? What if you could use unique symbols for programming elements, regex terms, and other entities? This wouldn't require learning anything beyond what you would already learn, and it wouldn't require any additional typing. The advantages would be: 
 
  * no more escaping things
  * cleaner, nicer look
  * takes up less space in a document
  * less ambiguous, less context-dependent meanings

##Regex remappings: 

 Char	Meaning 		Remapping
------	--------		---------
 ^	beginning of string	⇤ (a left arrow from a bar) 
 $	end of string		⇥ (a right arrow to a bar)  
 \d	a numeral		№ (the numero sign) 
 | 	logical "or"		∨ (the vel sign)  
 \w	an alphabetic character	α (alpha) 
 


