---
layout: post
title: 13 — Microphone threshold notification
date: 2026-04-05 12:03
modified_date: 2026-04-07 19:57
categories: dotfiles audio sounddevice numpy micnot
lang: en
redirect_from: /devlog/13
---

WIP - this devlog is unfinished

## Microphone to detect sound level
Living in a region under bombardments, at times of trouble there is a sound that is sufficiently loud to wake you from sleep, but somehow in the loo I hear nothing. I have been caught out enough times that I now have a certain anxiety and try to rush when I do my business!

Where I leave my computer the sound is audible, so I'm thinking a simple solution to the problem is connect a microphone and emit an audio notification over wireless headphones when the sound is over a threshold.

I searched for how to do things with the microphone in a Python script, and saw `pyaudio` and `sounddevice`, both bindings for the C library [portaudio](https://github.com/PortAudio/portaudio). I chose `sounddevice` because it seems nicer to use, especially swayed by bastibe's comment on [this 2015 Reddit thread](https://www.reddit.com/r/Python/comments/3k11g5/whats_a_good_sound_recording_library) (this must be none other than Bastian Bechtold, which the author of `sounddevice` thanks in the initial release).

[The docs](https://python-sounddevice.readthedocs.io/en/latest/usage.html) are good.

{% include note.html content='
> [!NOTE]
> The author is skilled at technical writing. He also has tutorials on [mg.readthedocs.io](https://mg.readthedocs.io/) and [Jupyter notebooks about working with audio in Python](https://nbviewer.org/github/mgeier/python-audio/blob/master/index.ipynb).
' %}

For reading from the microphone we could use [InputStream](https://python-sounddevice.readthedocs.io/en/latest/api/streams.html#sounddevice.InputStream) or [RawInputStream](https://python-sounddevice.readthedocs.io/en/latest/api/raw-streams.html#sounddevice.RawInputStream), the difference being that the first one passes `numpy.ndarray` (and thus requires `numpy`), and the latter "plain Python buffer objects".

InputStream arguments that are relevant to us:
- `samplerate` ?
- `blocksize`
- `channels` : "By default, the maximum possible number of channels for the selected device is used". All we want is to establish the volume of the input; we only need 1. This affects the number of columns in the data `ndarray` ("one column per channel").
- `latency` : "high" by default. "For reference, by default Audacity specifies a desired latency of 0.1 seconds and typically achieves robust performance."
- `callback` : Function that should take `indata: ndarray, frames: int, time: CData, status: CallbackFlags` and return nothing. The first argument (`indata`) is the data, the second argument is equivalent to `len(indata)`, and the rest are irrelevant to us.

In practice, higher `blocksize` and `latency` reduce the frequency at which our callback is called. Passing or not passing `samplerate` doesn't seem to make a difference here (I don't know what the default value for it is, seemingly `None` but surely not?)

Example indata:
```python
[[-0.00686818]
 [ 0.00214948]
 [ 0.00148432]
 [-0.00763792]
 [-0.01246719]
 [-0.01476933]
 [-0.00019834]
 [-0.01359193]
 [-0.01242189]
 [ 0.00367865]
 # [..]
 [ 0.02786395]
 [ 0.02375757]
 [ 0.01853095]
 [ 0.03426521]
 [ 0.02395786]
 [ 0.01751399]
 [ 0.01335307]
 [ 0.01133552]
 [ 0.01475426]
 [ 0.01101278]]
```

To calculate the volume of a signal, one typically uses [root mean square](https://en.wikipedia.org/wiki/Root_mean_square) (RMS), "the square root of the arithmetic mean of the squares of the values"
```python
#!/usr/bin/env python3
import sounddevice as sd
import numpy as np

def callback(indata, *_):
    print(np.sqrt(np.mean(indata**2)))

with sd.InputStream(channels=1, latency=.5, callback=callback):
    while True:
        sd.sleep(1000)
```
Notes:
- First set the microphone you want to use as the default input device. I am on ArchLinux and used `pavucontrol` to do this. You could alternatively tell sounddevice to use a particular device, see [Usage: Device Selection](https://python-sounddevice.readthedocs.io/en/latest/usage.html#device-selection) (note `default.device` can be a tuple).
- I'm not sure if the while loop is the right thing to do here, but if I don't have it it quits after two callback calls. We need to keep the script going so that the callback can continue to be called. The sleep isn't the polling rate, it continues to call the callback at the same rate regardless of how long we sleep for (despite this, we do need to sleep, with `pass` there instead it's "paying a heavy price to do nothing"; my CPU usage rises to 10% and temperature to 70°C !). In [the examples](https://python-sounddevice.readthedocs.io/en/latest/examples.html) he never uses it. In "Play a Sine Signal" what he does to keep it open is `input()` and then the user can "press Return to quit", but I plan to have the script in the background, so it will not have stdin. Are there differences in performance between `input` and a sleep loop? Not that it really matters right now... [`input` is better because no loop, but they're both practically no impact]
- `sd.sleep` takes ms, not seconds (unlike `time.sleep`). What's the point of using that over `time.sleep`? No idea (... one less line of code. But I don't know performance-wise)
- I tried latency 1 but it makes it hard to see the "wave" when I clap

My neighbours must not understand why I'm clapping so much.

Example output:
```python
# (background noise)
0.16827115
0.15406585
0.12535669
0.11746144
0.100960195
0.07497487
0.07434347
0.050024685
# (clap)
3.5514598
2.0815926
2.5934682
3.8069906
3.8069906
3.8069904
3.805783
3.7154257
3.1583529
2.69863
2.1975737
1.4657643
1.1886109
0.79398113
0.44642067
0.29354808
0.1775556
```

## Threshold
It is trivial to add a threshold:
```python
#!/usr/bin/env python3
import sounddevice as sd
import numpy as np

THRESHOLD = 1

def callback(indata, *_):
    rms = np.sqrt(np.mean(indata**2))
    if rms > THRESHOLD:
        print(rms)

with sd.InputStream(channels=1, latency=.5, callback=callback):
    while True:
        sd.sleep(1000)
```

I've actually clapped so much now that my hands hurt and I've had to find an alternative way to produce a sound. I am using a steel bottle whose lid has a plastic part that's kind of like a carabiner that you can connect to something to secure it, and I'm hitting that against the side of the bottle to produce a metallic clang, which is apparently louder than even my most vigorous claps.

{% include note.html content='
> [!NOTE]
> I\'m not going to use my vocal cords, god forbid.
' %}

This running in the background is using about 0.1% CPU. Lower sleep times increase this, but higher ones do not reduce this.

## Sound notification
I at first thought to use an existing sound, like "/usr/share/sounds/freedesktop/stereo/audio-test-signal.oga" or something, but to play an audio file we would actually need another library to read it, e.g. soundfile. But since we're already using sounddevice and numpy, we could generate a sound ourselves!!!

This is inspired in part by [dublUayaychtee's thread from 2021](https://www.reddit.com/r/learnpython/comments/n0ophu/play_multiple_sounds_at_once_in_sounddevice/), who was using sounddevice to make a digital piano!

In mgeier's [Generating Simple Audio Signals Jupyter notebook](https://nbviewer.org/github/mgeier/python-audio/blob/master/simple-signals.ipynb), he shows how to make a sine wave and play it. Adapted:
```python
import numpy as np
import sounddevice as sd

# duration (seconds), amplitude +-1.0, frequency (hz), sampling frequency (hz)
def bip(dur=1, amp=.2, freq=440, fs=44100):
    t = np.arange(np.ceil(dur * fs)) / fs
    sig = amp * np.sin(2 * np.pi * freq * t)
    sd.play(sig, fs)

bip()
```

> `numpy.arange([start, ]stop, [step, ]dtype=None, *, device=None, like=None)`  
Return evenly spaced values within a given interval.  
—[docs](https://numpy.org/doc/stable/reference/generated/numpy.arange.html)

There is also [`linspace`](https://numpy.org/doc/stable/reference/generated/numpy.linspace.html) which is sometimes used to generate `t`. The difference is over my head so I will not even try to talk about it.

Playing around with the inputs:
- dur : increases/decreases the length of the sound
- amp : volume [[technically not](https://dsp.stackexchange.com/questions/91569/difference-between-level-signal-strength-amplitude-and-volume)]
  + I made it intentionally low in the sample because I'm worried about blowing someone's ears out whose computer's top volume is high (for me even double max volume I'm fine, so I should be careful when I post audio things online)
- freq : pitch
- fs (sampling freq) : I can't tell any difference, other than if you set it too low or too high sounddevice raises an exception "sounddevice.PortAudioError: Error opening OutputStream: Invalid sample rate [PaErrorCode -9997]"

He also shows how to make a stereo sound. Adapted:
```python
def bip_stereo(dur=1, amp=.2, freq=(500, 600), fs=44100):
    freq = np.array(freq)
    t = np.arange(np.ceil(dur * fs)) / fs
    sig = amp * np.sin(2 * np.pi * freq * t.reshape(-1, 1))
    sd.play(sig, fs)

bip_stereo()
```

If you pass lower frequencies it's positively pleasant
```python
bip_stereo(2, 0.2, (100, 400))
```

Can we modify it to pass more frequencies? I guess not because we only have two ears? Though audio devices can nevertheless have more than two channels, like front centre, front left, front right, rear centre, rear left, rear right, side left, side right (I'm getting this from the `audio-channel-*.oga` files in /usr/share/sounds/freedesktop/stereo).

Well anyway that's a complete distraction. Any sound will do.

It works better with a short sound.

```python
#!/usr/bin/env python3
import sounddevice as sd
import numpy as np

THRESHOLD = 1
SAMPLERATE = 44100

def bip(dur=.1, amp=.2, freq=440, fs=SAMPLERATE):
    t = np.arange(np.ceil(dur * fs)) / fs
    return amp * np.sin(2 * np.pi * freq * t)

sound = bip()

def callback(indata, *_):
    rms = np.sqrt(np.mean(indata**2))
    if rms > THRESHOLD:
        sd.play(sound, SAMPLERATE)
        print(rms)

with sd.InputStream(channels=1, latency=.5, callback=callback):
    while True:
        sd.sleep(1000)
```

{% include note.html content='
> [!NOTE]
> Discovered accidentally: Coughing is also sufficiently loud to trigger it. If you have a cold it\'s like a geiger counter over here
' %}

## Reality
I had to reduce the threshold further and further until it started working (to detect the alert) at 0.3. Is it that the sound I thought was super loud is actually less loud than a cough or clap, technical limitations of the microphone, or technical limitation of this way of calculating volume? It may not actually be that loud mathematically but our perception for how loud a sound is has to do with how annoying it is and its pitch?

Reduced threshold, added timestamps to be able to tell if it beeped when it should have, and changed the sound to be more noticeable (careful, it might be too loud on your machine)
```python
#!/usr/bin/env python3
import sounddevice as sd
import numpy as np
from time import strftime

THRESHOLD = .3
SAMPLERATE = 44100

def bip(dur=0.1, amp=1, freq=(500, 600), fs=SAMPLERATE):
    freq = np.array(freq)
    t = np.arange(np.ceil(dur * fs)) / fs
    return amp * np.sin(2 * np.pi * freq * t.reshape(-1, 1))

sound = bip()

def callback(indata, *_):
    rms = np.sqrt(np.mean(indata**2))
    if rms > THRESHOLD:
        sd.play(sound, SAMPLERATE)
        print(f"{strftime('%F %T')} Volume {rms:.2f}")

with sd.InputStream(channels=1, latency=.5, callback=callback):
    while True:
        sd.sleep(1000)
```
It's still, alas, shit.

## Loopback
Idea to instead of playing a generated sound, play the sound it captured. That way we would (presumably) have a better idea at least for now if it's really an alert or a false positive.

What happens if I simply replace `sd.play(sound, SAMPLERATE)` with `sd.play(indata, SAMPLERATE)`?

That seemed stupid, I thought there would be a problem doing it in the callback or something, or that it wouldn't be a continuous sound. but it does work, if I say something next to the microphone I hear my own voice. Clapping or coughing is not recommended, however.

With a long sound, the `sd.play` calls reset each other, sometimes I get a little excerpt or two and sometimes nothing, even though I see the logs.

Instead of doing `sd.play` to play the sound, we could replace our `InputStream` with a fully-fledged `Stream` (both input and output) and write out either silence or the sound if it's above the threshold. Same arguments except we also take `outdata` and have to write to it each time the callback is called.
```python
#!/usr/bin/env python3
import sounddevice as sd
import numpy as np
from time import strftime

THRESHOLD = .3

def callback(indata, outdata, *_):
    rms = np.sqrt(np.mean(indata**2))
    if rms > THRESHOLD:
        print(f"{strftime('%F %T')} Volume {rms:.2f}")
        outdata[:] = indata
    else:
        outdata.fill(0)

with sd.Stream(channels=1, latency=.5, callback=callback):
    while True:
        sd.sleep(1000)
```
The reason for the `[:]` is better explained by the [docs](https://python-sounddevice.readthedocs.io/en/latest/api/streams.html):
```python
# In Python, assigning to an identifier merely re-binds the identifier to
# another object, so this will not work as expected:
outdata = my_data  # Don't do this!
# To actually assign data to the buffer itself, you can use indexing, e.g.:
outdata[:] = my_data
# … which fills the whole buffer, or:
outdata[:, 1] = my_channel_data
# … which only fills one channel.
```

I don't know if I should have the `channels=1` anymore. It doesn't seem to hurt anything, I'm not recording the sound and don't care about its quality or whether it's mono or stereo, I just want to be alerted when there is a bloody alert. The latency is there as a performance optimisation, it introduces a delay but keeps CPU use at just 0.1%, as before.

## activating/deactivating mic and integration with togglepause
problem is we don't know when it is pause or play and it could lead to deactivating when we mean to activate. could do sound effects:
- at script start have it verify first microphone is emitting some stuff to ensure it is on, and if yes do a success sound
TODO

## micnot
I called my script micnot, and here is it in full (careful running it in case the sound is too loud on your machine):
```sh
```

[version control link where I will put future changes](https://github.com/plu5/dotfiles/blob/main/pm/scripts/micnot)

## Fix/improve
- Lower the false positives
  + Lower threshold for high pitch sounds?
  + Detect more features particular to the sound in question?

*SNEEZES*  
BEEP  
—Thank you.

{% include fin.html %}
