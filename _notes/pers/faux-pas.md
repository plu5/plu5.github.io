---
layout: post
title: Faux pas
date: 2026-04-19 17:58
modified_date: 2026-04-20 06:26
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

{% include fin.html %}
