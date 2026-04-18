---
layout: post
title: Notes Jekyll
date: 2026-04-17 05:54
modified_date: 2026-04-17 06:02
categories:
lang: fr
---

## Build local
- `bundle exec jekyll serve` ou `s`
- `bundle exec jekyll s -H 0.0.0.0`
  + pour pouvoir tester sur d'autres appareils. ça va être sur l'ip local du hôte (vérifier `ip address` / `ifconfig` / `ipconfig`), port 4000
- `bundle exec jekyll s -H 0.0.0.0 -P 8000`
  + pour spécifier le port aussi

{% include fin.html %}
