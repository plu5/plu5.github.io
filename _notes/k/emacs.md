---
layout: post
title: Notes Emacs
date: 2026-04-08 12:10
modified_date: 2026-10-18 13:58
categories: emacs soft
lang: fr
---

## Excentricités
- Les lignes sont indexées à partir de un, les colonnes sont indexées à partir de zéro. (0,0) → ligne,colonne (1,0)
- Emacs supporte mal les lignes longues. `so-long-mode` (intégré dans les versions récentes) aide.

## Quand ça fige et C-g n'aide pas
Ça ne va pas le tuer, juste envoyer un signal d'arrêter ce qu'il est en train de faire, et entrer dans le débogueur :
- killall -USR2 emacs
- ou : pkill -SIGUSR2 emacs

Un effet secondaire est que ça active le débogage sur C-g. Désactiver :
- (setq debug-on-quit nil)
- ou : M-x toggle-debug-on-quit

## Sauvegarder sans déclencher les hooks de sauvegarde
C-x C-q C-x C-s C-x C-q

read-only, save, revert read-only ([Nicolas Douma, 2019](https://stackoverflow.com/a/59032112/18396947)

## Sous Linux
- `setxkbmap -option lv3:ralt_switch`
  + pour éviter que compose key soit détecté comme meta

{% include fin.html %}
