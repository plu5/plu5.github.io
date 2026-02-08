---
layout: post
title:  "Deciphering swansontec’s C++ Map Macro"
date:   2024-01-05 12:42
modified_date: 2024-01-05 12:42
categories: c++ metaprogramming
---

this will be significantly easier to understand if you follow along on godbolt.org with -E and try out each line for yourself, feeding things into it and seeing what it transforms to.

createforms example to simplify the c++builder boilerplate code
dialog1, dialog2, dialog3
reduce repetition

dangers of this approach section
a word of caution: what is saved in lines of code may cost in later debugging time should something go wrong. [research this]
macros obtuse errors may be difficult to trace down
and should the problem be in the macro itself the intricacies of preprocessor rules and the tricks involved in forcing recursion into what was never meant to support it [less dramatic alternative: in getting it to recurse] is not something many other devs on a project are likely to be familiar with.
2023-01-05 03:28

recomended series of articles by saad ahmad

{% include fin.html %}
