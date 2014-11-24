---
layout: post
title: Convert CSS Units in Vim
category: hacks
tags: 
 - linux 
 - vim
---

If you need to convert CSS units to `em`s in Vim, here are some useful commands: 

1. Converts `font-size: 100%;` to `font-size: 1em;`: 

    :%s#font-size: \(\d\d\)%#\=printf("font-size: %0.3fem", submatch(1)/100.0)#gc

2. Converts `font-size: 16px;` to `font-size: 1em;`: 

    :%s#\v(\d+)px#\=printf("%0.3fem", 1.0/16*submatch(1))#gc
