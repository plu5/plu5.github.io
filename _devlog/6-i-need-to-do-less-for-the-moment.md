---
layout: post
title: 6 — I need to do less for the moment
date: 2026-02-20 12:01
modified_date: 2026-02-20 19:33
categories: braille.el
lang: en
redirect_from: /devlog/6
---

## braille.el
### Slowing down work
A dilemma: I have to stop with this, it's not what I'm meant to be doing, but I don't want to just leave it, I want to share it with people because it's already quite cool and useful. but my mind won't let me, because it's saying it wants to first do A B C before it's good enough. like have animation, shapes, transformations, a custom font, even a repository with GitHub Actions set up to be able to clone any repo of a font and add to it a custom braille block.

I wanted to submit to MELPA, record a demo and post it on /r/emacs, which I have been fantasising of doing since day 0 of this project. because it's a fantasy, it has fear attached to it.

"I don't stand to gain anything, the only effect it can have on me is emotional devastation from either finding out it's bad or (more likely) from the crickets of no one caring." -- No, there is a lot to gain both for me personally (maturity, fulfilment) and for braille.el (users! even if temporary). Having other people trying it is critical for a piece of software to get better, and it's how you learn the fastest. and there is nothing to lose, it's only temporary emotions that it's actually best to deal with now.

Publishing helps learn what doesn't work and what's actually needed before doing a tonne of work on something that people won't use anyway, like the GitHub Actions font modification idea. That's why I think it's a good idea to publish as soon as possible, even when it's just a proof of concept with problems. Not sure this passes on MELPA but I know more accomplished people than me would have shared it on social media already as soon as they got a single braille dot.

but I'm not going to be able to convince myself in this way. I have to slowly do all the little things I wanted to before. I guess I can maybe try to work towards it on the weekends or something :-/

I think you need _shamelessness_ essentially. I reckon that's the main thing successful people have that most people lack, but knowledge of this is not enough, you can't really suppress shame in your brain, it's one of the strongest emotions and it will unleash itself one way or another.

The important thing is to stop working on this every day, I can't afford to. I'm sorry I can't live up to the ideals of Stallman and Torvalds. If I was made of the same stuff, instead of spending the time writing this I would have just got on with it, but I'm convinced I can't change this, this is who I am and I have to work with that. It's on GitHub anyway, whoever wants to try it can do so.

### Rebind canvas creation
Did the rebinding of canvas creation shortcuts from <kbd>C-c n</kbd> and <kbd>C-c C-n</kbd> to <kbd>C-c v</kbd> and <kbd>C-c C-v</kbd> as proposed in the last devlog.

### Readme
I also took the opportunity to reorganise the readme sections and add more information. Customisation options, space vs blank grid canvases. Explain things that are to me obvious and already explained in docstrings but not all users will look there.

No longer "work in progress" text and list of things to implement at the top, moved that to a roadmap section with the explanations, usage, and keybindings above it, so it's now more like a proper package ready for someone to use rather than an unfinished wip.

Then the other things I wanted to do just add them to the roadmap and out of my IRL todo list and move on to other things ;=;
(for now) (i'll come back the weekend probably) (wait, it is now the weekend) (next weekend or sth)

{% include fin.html %}
