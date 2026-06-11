---
layout: post
title: Faux pas
date: 2026-04-19 17:58
modified_date: 2026-05-27 13:49
categories:
lang: en
---

Behaviours that bother me

## Refusing to increment major version
The idea behind semantic versioning is simple: you increment the patch (x.x.X) version for a release that fixes a bug, the minor version (x.X.x) for a release that introduces a new feature, and the major version (X.x.x) for a release containing a backwards-incompatible change. Unfortunately, the human mind does not compute. People do not like incrementing the major version ever. It's software, shit breaks, there will be releases where the authors don't realise it's backwards incompatible, then we would have to deal with some waste of human productivity resulting from these mistakes, but it would be a small percentage of cases. No. People (and companies) willingly refuse to increment the major version, because in their mind one can't simply go 1.x.x → 2.0.0 without a complete overhaul and a bombastic marketing campaign.

- TypeScript
  + despite many JavaScript package managers updating automatically on patch/minor change
- Python
  + 3.10 rendered millions of packages, programs, and scripts inoperable (raising an exception when passing floats to C APIs that want an int)
    - Making such changes willy nilly disrupts the entire infrastructure that relies on Python. Deprecation warnings are clearly not enough given what happened in reality.

## Polluting stdout with no option to turn it off
- MESA-INTEL spamming an internal TODO message in stderr that cannot be turned off without recompiling, polluting everyone's logs for no reason

## Licences
This is not really a faux pas, I just don't know where else to put this spiel. I am really put off by licences, the language, uppercase, clutter, US-centrism, having to paste it everywhere. I refuse to even spell the word licence with an S. Copyleft sounds like a good idea to spread free software, but now it feels like most of us publishing code barely even exist. Therefore, it creates bigger problems than it solves (makes fellow open-source developers not want to use your code to avoid complexity, or waste time pondering licence compatibility). I wish someone stole my code, company or not, attributed or not, I wish I could be of some use for something or to someone, and if someone makes money off my work, I would be proud.

It puts me off that GNU GPL has come to be used as a monetisation tactic: pay and get to do whatever you want with it, otherwise deal with the headache of licence compatibility, pasting the licence everywhere, changing the licence on your project (licenced with something else, and [you need permission](https://softwarefreedom.org/resources/2007/gpl-non-gpl-collaboration.html) from [95% of contributors with no objections](https://dolphin-emu.org/blog/2015/05/25/relicensing-dolphin/) to change, and [some can't because of publishing on closed platforms](https://github.com/godotengine/godot-proposals/discussions/4237)). GPL as punishment. It has become synonymous with restriction rather than liberty. A permissive licence renders the same code more valuable, accessible, and usable by more people in more situations. GPL is also seen as a way of protecting the code by people who are unhappy about the idea of others profiting off their work without having paid them (though [it offers no such protection](https://gitlab.gnome.org/GNOME/gimp/-/issues/7274)). That's not me and I don't want anyone to think that it is. I have no motivation at all to "protect" my code.

More permissive licences still carry some of those requirements, and still repulse me. Public domain appeals to me, but it's incredibly hard (or impossible in some jurisdictions) for individuals to put something in the public domain. Look at SQLite to see the efforts some have gone to, which most of us can't even come close to, but even that is not good enough.

Because of being put off every time I try to look at this subject, I left all of my code unlicenced for years. So long as I live, I can give permission for anyone to use it should they need explicit permission, but if I die, and someone needs it?

It may not be considered legally valid, but I'll try just having a COPYING file and state the licence I need to have to be compliant with my dependencies, list all files with contributors and the licence that "wins over" for each, and add a line that any code solely authored by me is in the public domain, or CC0 in countries where that is not possible. At least it states my wishes, so it's better than nothing? I have to assume that the risk of getting sued for not perfectly complying with the licence by stating it in one place instead of pasting it everywhere verbatim is incredibly minimal. The spirit of copyleft licences is that the source code of derivative programs is accessible to the user [+ the freedoms to use, modify, and redistribute], and it is. In reality, of course, no one is likely to care or use my code anyway.

{% include fin.html %}
