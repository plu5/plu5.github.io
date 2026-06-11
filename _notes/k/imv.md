---
layout: post
title: imv
date: 2026-05-18 16:32
modified_date: 2026-05-18 17:10
categories:
lang: fr
---

- notif chemin : `:exec notify-send $imv_current_file`
- copier chemin : `:exec xclip -selection clipboard <<< "$imv_current_file"`
- copier image : `:exec xclip -selection clipboard -t image/png -i "$imv_current_file"`
  + ne marche pas toujours, et autre types que `-t image/png` ne marchent pas du tout. alors ce que je fais est copier le chemin et coller dans le navigateur

{% include fin.html %}
