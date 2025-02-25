---
layout: post
title:  "Godot mouse capturing that doesn’t feel annoying / invasive"
date:   2023-06-03 02:03
modified_date: 2023-06-03 02:30
categories: godot gamedev
---

instead of:

```gdscript
func _ready():
    Input.mouse_mode = Input.MOUSE_MODE_CAPTURED
```

```gdscript
func maybe_capture_mouse():
    if Input.mouse_mode == Input.MOUSE_MODE_CAPTURED:
        return
    if get_viewport().get_mouse_position().y > 0:  # i.e. not on titlebar
        Input.mouse_mode = Input.MOUSE_MODE_CAPTURED

func _notification(what):
    match what:
        NOTIFICATION_APPLICATION_FOCUS_IN:
            maybe_capture_mouse()
        NOTIFICATION_WM_WINDOW_FOCUS_OUT:
            Input.mouse_mode = Input.MOUSE_MODE_VISIBLE

func _input(event):
    if event is InputEventMouseButton:
        maybe_capture_mouse()
```

- Prevents the user from being able to interact with the titlebar, can’t move the window if windowed or press titlebar buttons like minimise, maximise, or close
- Capturing mouse on launch feels invasive

with the new way player can interact with the titlebar without being interrupted. if they click in the game coordinates or alt tab back into the game then the mouse is captured, as they’d expect.

{% include fin.html %}
