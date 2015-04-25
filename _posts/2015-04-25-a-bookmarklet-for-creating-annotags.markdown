---
layout: post
title: A Bookmarklet For Creating Annotags 
category: digital humanities
tags: 
 - annotation 
 - annotags 
 - open source
---

At [I Annotate](http://iannotate.org/) this weekend, I made a bookmarklet that will allow you to generate an [Annotag](/projects/annotags/about.html) from a book's bibliographic entry. To use it: 

 * Drag the following button onto your bookmarks bar: <a href="javascript:(function(){document.body.appendChild(document.createElement('script')).src='http://jonathanreeve.github.io/assets/js/annotag-tweetme.js' ;})();" class="button bookmarklet">Tweet This Book</a>
 * Visit a website that has books. It currently works with [worldcat.org](http://worldcat.org) and [Project Gutenberg](https://www.gutenberg.org/). 
 * Click the bookmarklet called Tweet This Book. 

Here's how it works: the bookmarklet extracts book identifiers from your URL, encodes it in base-62, and takes you to Twitter, where this information will already be encoded as a hashtag. Now it's even easier to tweet about books! 
