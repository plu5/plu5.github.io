---
layout: post
title: 18 — retype command bugs
date: 2026-05-17 14:07
modified_date: 2026-05-20 15:03
categories: retype
lang: en
redirect_from: /devlog/18
---

## Bugs sadness
[I discovered a lot of bugs in retype recently](https://github.com/plu5/retype/issues/48#issuecomment-4468728087) around console commands.

It's been a while (I think over a year [actually 7 months. 21 Oct]) since I last worked on it, and it's always painful working on bugs, so my brain is looking for every possible way to procrastinate. I get the sense this part of the codebase is very old and not well thought out, so I fear even opening it. I think writing along in the devlog might reduce the stress. If I can fix the bugs, I will feel a lot better after.

## Bug 1: skipline visual desync
First, let me describe the bug and how to reproduce it.

retype is a typing program. While typing, there are two main widgets that concern us:
1. The BookDisplay, where the text to type is shown and a cursor visually advances through it, with text already typed highlighted.
2. The Console, which is an input line at the bottom where we do the actual typing. It gets cleared at the end of each line successfully typed (either automatically, or by pressing Enter, depending on the "newline characters advance automatically" setting under "line splits" in the settings).

Other than that, the Console has commands you can type and press Enter to do things in the program. For example `>typespeed` to launch the typespeed game. The first character is the "prompt" character and it's customisable and can be set even to nothing to be able to type just `typespeed`. If the console input matches prompt + a command, the corresponding command is executed, and the console is cleared. There is also command history to be able to enter the previous command by pressing <kbd>Up</kbd> <kbd>Enter</kbd>.

The command that particularly concerns us is `skipline` (alias `l`). Input rapidly, instead of jumping to the start of the next line, the visual cursor and highlighting sometimes jump to `n` characters past the start of that line, where `n` is the number of characters we had to input for the command (if it's just `l` 1 character, `>l` 2, `>skipline` 9). This bug occurs regardless of what the prompt is set to.

This bug is kind of annoying to reproduce, because you have to input the command quickly, but if I do it as quickly as I can with <kbd>Up</kbd> <kbd>Enter</kbd>, I can reproduce it reliably. It would be nice though if it's possible to write a unit test that reproduces it.

Skip to half a day later: I can't reproduce it in a unit test. I wrote a separate test file, `test_commands_affecting_cursor.py`, because this command, unlike the others, has to do with the highlighting service not just the command service, so it requires more set up involving the highlighting service. In the test I'm doing `console.submitText(">advanceLine")` and checking `cursor.position()`, but it's not much use, it's always correct, doesn't matter how many times I run this command or how fast. I'm not sure even in the real program that the cursor position really goes off sync or it's visual, not sure which variable to test. I think it's visual, because calling `gotoCursorPosition` gets it back to the correct position, and this is not a command that changes the cursor position. But also the position in the line is a transient thing and depends on the content of the cursor, only the position at the start of the line you are on is "persistent", "locked in". I'm not sure if my explanation is clear, I explained a little more [in a comment on PR 49](https://github.com/plu5/retype/pull/49#issuecomment-4468846075) how it works in terms of the cutting up the text into lines, but it doesn't really go into position vs persistent position. It could be some kind of desync where it thinks you still have text in the console when it's actually already been cleared, but, I don't know, it doesn't really make sense because the text wasn't matching, so it'd be in red.

Anyway, I get the strong sense this will be fixed if I simply clear the console _before_ executing the command instead of after. But this may fix bug 2, so let me first describe bug 2.

## Bug 2: gotoCursorPosition deletes text
This one is easy to reproduce because it happens always when launched via the console command. The toolbar button, which triggers the same function, works well. In the past few years I always used the button and forgot about the existence of the command to do the same, so it's been likely broken for a long time.

The console command deletes n characters after the cursor, where n is the number of characters that were typed to execute the command. `>cursor` deletes 7 characters after the cursor. Pressing the equivalent toolbar button restores the text (executing via the console again doesn't).

I tried and failed to write a unit test for this also. All the console command does is call the BookView function. In the console command tests, the BookView is mocked. It would have to be an integration test I guess because the BookView function works, the console command works, the problem only happens when you combine them. Currently I don't have any integration tests, I've never done it before, and it's complicated.

## Try clearing before
I don't really understand the behaviour but it seems to me that simply clearing the console before calling the functions the console commands are attached to should resolve the bugs, as then it would be the same in theory as toolbar buttons or any other method of triggering a function call. The only situation I can imagine where it wouldn't work, is if clearing the console is async or takes some time before it takes effect, but I don't think that that's the case.

Confirmed. Well, that turned out to be a bit of a storm in a teacup.

{% include fin.html %}
