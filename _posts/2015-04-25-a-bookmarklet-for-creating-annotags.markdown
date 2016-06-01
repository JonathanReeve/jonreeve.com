---
layout: post
title: A Bookmarklet For Creating Annotags
category: digital humanities
tags:
 - annotation
 - open source
---

At [I Annotate](http://iannotate.org/) this weekend, I made a bookmarklet that will allow you to generate an [Annotag](/projects/annotags/about.html) from a book's bibliographic entry:

<p class="center"><a href="javascript:(function(){document.body.appendChild(document.createElement('script')).src='http://jonathanreeve.github.io/assets/js/annotags/annotag-tweetme.js' ;})();" class="button bookmarklet">Tweet This Book</a></p>

To use it:

 * Drag the button above onto your bookmarks bar.
 * Visit a website that has books. It currently works with [Worldcat](http://worldcat.org) and [Project Gutenberg](https://www.gutenberg.org/).
 * Click the bookmarklet.
 * Add a location suffix to the hashtag, if you want, like `:p26`.
 * Tweet a comment about the book.

The bookmarklet extracts book identifiers from your URL, encodes it in base-62, and takes you to Twitter, where this information will be be waiting for you, encoded as a hashtag. Now it's even easier to tweet about books!

If you'd like to help improve the code, it can be found [here on GitHub](https://github.com/JonathanReeve/annotags/blob/master/annotag-tweetme.js).
