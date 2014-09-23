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
 
Thus, programming something that uses two or more of these can be tricky. Consider writing a line of PHP that contains regex that contains a dollar amount. You'd end up with something like: `$var = preg_match('/\$\d$/', $s);`. You'd just have to know that the first dollar sign was denoting a variable, the second was a literal dollar sign used to denote dollars, the third was an indication of the place within the string, and the fourth was another variable. Of course, that's just something you have to learn as a programmer, right? 

#The Solution


