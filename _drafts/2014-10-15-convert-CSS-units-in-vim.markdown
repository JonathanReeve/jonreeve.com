---
layout: post
title: Convert CSS Units in Vim
category: hacks
tags: 
 - linux 
 - vim
 - css
---

Relative font sizes are great, but it can be a difficult to convert your CSS or Sass file to `em`s. If you use Vim, however, it's relatively easy. Here are some useful commands: 

 * Converts `font-size: 100%;` to `font-size: 1em;`: 

```
:%s#font-size: \(\d\d\)%#\=printf("font-size: %0.3fem", submatch(1)/100.0)#gc
```

 * Converts `font-size: 16px;` to `font-size: 1em;`: 

```
:%s#\v(\d+)px#\=printf("%0.3fem", 1.0/16*submatch(1))#gc
```
