---
layout: post
title: Vim All the Things
category: hacks
tags: 
 - linux 
 - vim
---

Symmetrical Keybindings, or, Vim All the Things

Imagine a world where key commands have a logical symmetry to them. Command (or Windows Key) plus a letter would control the operating system or window manager, Alt plus a letter would control a group of programs, and Control would control the program on the low level. For instance, Command + l would take you to the program on the right. Alt + l would take you to that program's right pane, and Control + l would take you to the right within that pane. There's a logical hierarchy to it. Thankfully, with a little bit of configuration, this is well within reach. 

Vim is about the greatest thing ever. One of the best things about it is that it's designed to be incredibly efficient and ergonomic. It often just takes a couple keystrokes with Vim to perform an editing task that would take several key combinations or mouse movements with a WYSIWYG word processor. The problem is, it's too good. When you get used to it, you're so spoiled by it that it can be frustrating to work with other programs, because the key commands are so inefficient. With a little configuring, though, you can vimify just about everything about your computing environment. Here are the basic ingredients: 

1. Window manager: Awesome WM or i3. 
2. Shell: Vim keybindings in BASH. 
3. Web browser: dwb or the Vimperator / Pentadactyl plugins for Firefox. 
4. Python (interpreter), MySQL, or anything else that uses Readline
  
One of the great things about Vim is that it's ergonimally designed.   

## Window Manager: Map Super+HJKL

In ~/.i3/config, set these keybindings: 

```
# set mod key to super
set $mod Mod4

# change focus
bindsym $mod+h focus left
bindsym $mod+j focus down
bindsym $mod+k focus up
bindsym $mod+l focus right

# move focused window
bindsym $mod+Shift+h move left
bindsym $mod+Shift+j move down
bindsym $mod+Shift+k move up
bindsym $mod+Shift+l move right
```

## Tmux: Alt+Tabnumber

in ~/.tmux.conf, set these values to map Alt+numbers to tab switching: 

```
bind-key -n M-1 select-window -t 1
bind-key -n M-2 select-window -t 2
bind-key -n M-3 select-window -t 3
bind-key -n M-4 select-window -t 4
bind-key -n M-5 select-window -t 5
bind-key -n M-6 select-window -t 6
bind-key -n M-7 select-window -t 7
bind-key -n M-8 select-window -t 8
bind-key -n M-9 select-window -t 9
```

(props to http://superuser.com/a/686686/83457)   

## Applications: Map Ctrl+HJKL 

###Weechat: Remap Alt+Tabnumber to Ctrl+Tabnumber

Find the section in ~/.weechat/weechat.conf that reads: 

```
meta-0 = "/buffer *10"
meta-1 = "/buffer *1"
meta-2 = "/buffer *2"
meta-3 = "/buffer *3"
... etc
```

and change it so that it reads: 

```
ctrl-0 = "/buffer *10"
ctrl-1 = "/buffer *1"
ctrl-2 = "/buffer *2"
ctrl-3 = "/buffer *3"
```

###Vim: Map Ctrl+HJKL

