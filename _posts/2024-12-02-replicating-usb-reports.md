---
layout: post
title:  "Replicating USB reports to commercial keyboard"
date:   2024-12-02 13:58
modified_date: 2024-12-02 13:58
categories: lowlevel
---

{% include note.html content="I have not 100% verified this but pyusb may not work on more recent versions on windows unless you load a different driver onto the interface like something like winusb, which seems a worse way of doing it. i could not successfully open my device on windows with pyusb despite the same instructions working on linux." %}

an approach that works on all operating systems without having to install a custom driver
