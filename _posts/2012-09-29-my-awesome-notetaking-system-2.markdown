---
layout: post
title: My Awesome Notetaking System
category: hacks
tags: 
 - linux 
 - vim
---

I accumulate a lot of notes. Some are little personal ephemera, like ideas, addresses, or sketches for poems; some are commentary about books I’m reading; some are notes I take in class. I also maintain lists of things for fun: plant names I think are cool, possible band names, titles for things, new vocabulary words. I’ve used just about every possible medium for collecting these notes: little back-pocket notebooks, giant unruled composition books, moleskines, voice recorders, and even stacks of index cards. I’ve also used a ton of different word processors and notes apps: Microsoft Word, EverNote, the iPhone notes app, and Tomboy Notes, to name a few. Each of these systems has their advantages. A composition book won’t run out of batteries, and an electronic file (properly backed up) is harder to lose than an index card. Each of these also has their disadvantages, though, and I usually lose patience with a system after a while. I’m really happy with my current notetaking system, though, and so I’m excited to write about it and show it off. Here’s how it works. 

#Plain Text
Plain text files (.txt) are better because: 

 * They’re faster. Firing up Microsoft Word can often take a while, and that can be annoying when you just want to jot something down. A plain text editor like Notepad, Text Edit, or gedit, on the other hand, loads instantly. 
 * They’re more reliable. I’ve had Word documents fail on me a few times, and then I have no way of accessing my document except through messy data forensics. Plain text never fails, because there’s nothing to fail. 
 * They’re future-proof. Word processors will come and go. What happens years from now when you’re stuck with a bunch of documents, but you don’t have the program that opens them? I have a bunch of old Word documents written on an old Mac, and none of the recent word processing programs can open them. Plain text solves that problem.  

Suppose you want to use fancy formatting, though? That’s where Markdown comes in. 

#Markdown
[Markdown][2] is a way of formatting plain text to allow for rich-text-like formatting, like headers, italics, etc. It’s quick, easy to write, and looks great on the page. There was [a good article in Lifehacker recently about the virtues of markdown][1]. Markdown can be easily translated into other formats, too, like HTML or .doc files. The command-line program [markdown][4], for instance, can translate to HTML and the GUI program [ReText][3] can export to Google Docs. 

#Vim
[Vim][5] is probably the best text editor ever made. It’s tough to learn, but once you learn it, it’s the most efficient way of editing text. All the editing commands are mapped to the keyboard, so you don’t have to move your hands back and forth to the mouse all the time. Vim is great because:  

 * It’s ubiquitous. It’s installed standard on most macs, linux desktops, and servers. Furthermore, since it can run in a terminal, you can run the program remotely over SSH. As far as I know, it’s been ported to just about every platform there is: Mac OS, Windows, Linux, Android, even iOS.  
 * It’s small and lightning-fast. It takes almost no time to start up, and it doesn’t use that much RAM when it’s running. Unlike other word processors, I’ve never seen it crash.  
 * It auto-saves everything you’re working on in swap files, right down to the last letter you typed, so you never lose anything by forgetting to save. 
 * It’s endlessly extensible. There are ridiculous numbers of plugins available for it. 
 * The vim keybindings translate to other programs, as well. I use luakit and vimperator for web browsing, and they behave a lot like vim. 

Here are some of my favorite vim features: 

 * Syntax highlighting. This is really nice for editing code, like HTML files, since it makes everything easier to read, but it also works for markdown syntax, too, and the variation used by notes.vim. There are dozens, if not hundreds, of color schemes, too, and some look really pretty.  
 * Folding. Folding is a way of collapsing sections of text to just their headings. By pressing `zM`, for instance, I can collapse all folds and just see my file as a table of contents. I can then expand any section, or expand them all. 
 * Split screens. I can have a bunch of notes open simultaneously, all on the same screen.   

Some articles celebrating vim: 

 * [“Why, oh WHY, do those #?@! nutheads use vi?”][6] 
 * [”Why Vim?][7]

And if you’re interested in learning vim, there’s the game [vim-adventures.com][8]. 

#Notes.vim
Notes.vim is a vim plugin that gives you a personal wiki. It also has a ton of other functions that make notetaking easier. Here’s what’s great about notes.vim:  

 * The wiki. The best thing about notes.vim is that if you type a word that’s the name of one of your notes, it will highlight it and turn it into a link. That way, you can easily have notes that link to other notes, and build a non-linear relational note system.  
 * It can autocorrect fancy characters like word processors do, i.e. turning straight quotes into curly quotes, and turning asterixes into bullet points. 
 * It automatically handles bulleted lists with many sublevels. It’s easy to make outlines. 
 * The syntax highlighting handles Markdown, but also some more syntax, like sentences in quotes. 
 * It supports @tags, which is an awesome way of organizing notes by more than one criterion. Just putting notes into folders on the disk, or virtual “notebooks” as Tomboy does, doesn’t make sense if you have more than one criterion. 

#Fun with the Terminal
Manipulating plain text notes from the terminal is fun. 

 * Display a note quickly: `cat note.txt`
 * Join two or more notes together: `cat note1.txt note2.txt > newnote.txt`
 * Add some text to the bottom of a note: `cat >> note.txt` then type some stuff.

I added a couple of lines to my .bashrc to make it easy to open notes, too: 

     # Opens a note
     ne() {
       vim -c ":Note $*" 
     }
     
     ## New Note: calls vim notes plugin
     n() { 
       vim -c :Note
     }

     # Searches Notes
     nls() {
       ls -c ~/Notes/ | egrep -i "$*"
     }
This way I can type “ne shakes” and get my note called “Shakespeare.” 

#Other Search Tools
With over 800 notes, I need good ways to search and open them. It’s easy to do from within vim, but to open a note when vim isn’t running, I use Kupfer. I open Kupfer by pressing F13, then start typing the name of a note, and it usually finds my note after I type a few letters. I also have a keyboard shortcut in GNOME that binds Ctrl+Alt+N to the command `vim note:` so that I can easily open a new note when I need to. 

#Backup and Sync
I use Dropbox for backup and sync, and it’s awesome. Probably the best feature it has is version control. That means, if I accidentally make a change to a document that I don’t want, Dropbox keeps all the previous versions of that file, and allows me to easily revert back to them. This also works for undeleting files. 
Dropbox’s sync is pretty great, too, so I always have all my updated notes on my desktop, laptop, and phone. Then there are some iPhone apps that allow me to view and edit notes from Dropbox: [PlainText][9] is a good one.  

#Conclusion
There you have it. It sounds complicated, but it’s actually pretty simple. 

[1]: http://lifehacker.com/5943320/what-is-markdown-and-why-is-it-better-for-my-to+do-lists-and-notes?post=52991739 
[2]: http://daringfireball.net/projects/markdown/syntax 
[3]: http://sourceforge.net/p/retext/home/ReText/  
[4]: http://daringfireball.net/projects/markdown/ 
[5]: http://www.vim.org/ 
[6]: http://www.viemu.com/a-why-vi-vim.html 
[7]: http://www.terminally-incoherent.com/blog/2012/03/21/why-vim/ 
[8]: http://vim-adventures.com/ 
[9]: http://www.hogbaysoftware.com/products/plaintext  
