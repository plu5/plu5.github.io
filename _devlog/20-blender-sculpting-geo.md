---
layout: post
title: 20 — Sculpting geometrical forms in blender
date: 2026-05-21 12:02
modified_date: 2026-05-24 02:27
categories: blender
lang: en
redirect_from: /devlog/20
wip: true
---

Ok, this is a weird one, but something I've been doing on my breaks is sculpting chess pieces in Blender, and I am, for no valid reason, very stubborn about sculpting the whole thing as if it was real life woodcarving or clay, not using any geometrical construction (no "modelling"). It's super challenging, then, to do the geometrical forms at the base. I want to write a little about the processes I have come to.

clay strips to build out forms

move brush to correct proportions

measure distance as how much fits between my index finger and pinky against the monitor plus or minus how many squares on the grid

cutting with project to line, segment

accept imperfection. no point snapping to a straight line outside of an orthogonal view, since your view won't be perfectly straight on anyway.

face sets for overlapping parts, but most of the time ignoring them and working on the whole model (S-M-z to toggle face set visibility and the grid)

isolating parts can also help to fix proportions, because you can see a cross section (h while hovering over a face set to hide it, M-h to unhide everything)

how do to something like circularity? essentially cut around it as if making a circle out of a many-sided polygon, then smooth the edges between these "sides" very carefully

don't go too heavy on the smoothing, you need a light touch

{% include fin.html %}
