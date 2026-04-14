---
layout: post
title: 12 — Godot's Dodge the Creeps without static assets
date: 2026-04-03 11:29
modified_date: 2026-04-13 20:58
categories: godot
lang: en
redirect_from: /devlog/12
wip: true
---

## Hi
I'm interested in games without static assets, everything in code. To this end, I've had the idea to try to make [Godot's Dodge the Creeps](https://github.com/godotengine/godot-demo-projects/tree/master/2d/dodge_the_creeps) without any static assets. Since it's going to be dynamic we could also take the opportunity to make the enemies random colours or something and allow the player to customise their character.

If you haven't read the original tutorial, it's on [Getting started → Your first 2D game](https://docs.godotengine.org/en/latest/getting_started/first_2d_game/index.html) and is also available in other languages (for example: for the French version, replace `en` with `fr` in the URL).

{% include note.html content='
> [!NOTE]
> If you want to try playing the original game, some people have exported their version to the web: [salotz.info](https://salotz.info/demos/godot-2d-creeps/creeps.html) (mouse controls), [adamscott@github](https://adamscott.github.io/2d-dodge-the-creeps-demo-main-thread-samples/) (keyboard controls), [quantummike@itch](https://quantummike.itch.io/godottutorial) (both + touch), and you can find others by searching "Dodge the Creeps demo". I think the best strategy is to not move very much, just stay near the centre and move out of the paths of newly-spawned enemies, as they only move in a straight line.
' %}

Option to either use art drawn by us in shapes kind of like SVG, or art in characters like TTY games / ASCII art.

What in terms of music and sound effects, can you emit mathematical sounds in code in Godot?

## Player character
Maybe we should use a geometric form like a square and call it _Thomas Dodged the Creeps_

{% include fin.html %}
