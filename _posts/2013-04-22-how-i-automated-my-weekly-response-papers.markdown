---
layout: post
title: How I Automated My Weekly Response Papers
category: hacks
tags: productivity markdown pandoc mutt linux
---

A course I took last semester required that I submit a weekly response paper for that week's readings. I wanted to see if I could automate that process as much as possible, so that I wouldn’t have to do all the repetitive stuff, like writing the date, putting it in MLA format, and emailing it. I came up with this workflow:

* writing in pandoc markdown
* using pandoc to convert markdown to pdf via TeX and the MLA style
* emailing the result using mutt

For those that don’t already know, [pandoc](http://johnmacfarlane.net/pandoc/) is a command-line document conversion tool that excels at converting from [markdown](http://daringfireball.net/projects/markdown/) (for reasons why markdown is awesome, see my [post below](http://jonreeve.com/blog/?p=18)). LaTeX is a professional typesetting language and command-line program that’s very good at producing PDFs. [Mutt](http://www.mutt.org/) is an excellent command-line email program. On Linux, you can install all these tools with one simple command, like this:

```sh
sudo apt-get install pandoc texlive mutt
```

Since they’re all command-line tools, they’re interoperable, which means you don’t have to have ten windows open on your desktop. With all these pieces in place, I wrote a single command to do everything I needed to do with my response papers. The source of my response papers looks something like this:

```
%Title of My Paper

Here is the body of my paper. It understands markdown syntax.
```

The % is there to tell pandoc which line contains the title. That’s the only thing that departs from traditional markdown syntax. Then I used [Ryan Aycock’s LaTeX MLA stylesheet](http://www.tex.ac.uk/tex-archive/macros/latex/contrib/mla-paper/mla.sty) and a short template I made that looks like this:

```latex
documentclass[12pt,letterpaper]{article}
usepackage{ifpdf}
usepackage{mla}
begin{document}
begin{mla}{Jonathan}{Reeve}{Prof's Name Here}{Course Name Here}{today}{$title$}
$body$
end{mla}
end{document}
```

I also have a short email.txt file which contains the body of my response paper email:

>Hi Prof. So-and-so,
Attached, please find my response paper for this week.
Best,
Jonathan

Then, with mla.sty and mla-template.tex (above) in my working directory, I ran:

```sh
pandoc response.md -o response.pdf --template=mla-template.tex && mutt -s "Response Paper" -a response.pdf my-prof@university.edu < email.txt
```
Which converts it to LaTeX, applies the template, converts the .tex file to .pdf, attaches the PDF to an email with the body from email.txt, and emails it to my professor. What's cool is that once I’ve already run the command, it's in my BASH history, so all I have to do is write the response paper in markdown, then hit Ctrl+R in the shell and type the first few characters of “pandoc,” which autocompletes my command.
