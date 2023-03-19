---
layout: post
title:  "Training Tesseract 4 on Windows"
date:   2021-04-29
modified_date: 2021-07-22 17:13
categories: ocr
redirect_from:
  - /training-tesseract
---

This article covers training Tesseract 4.1.1 for a particular font.

Put a otf or ttf of the font in some folder and make note of the path it is in.

Add the build folder with all the Tesseract executables to your PATH environment variable if it is not there already.

## Training scripts
Tesseract source `src/training` subdirectory has a script [`tesstrain.sh`](https://github.com/tesseract-ocr/tesseract/blob/4.1.1/src/training/tesstrain.sh) that already does most of the steps of the training for us. You can copy it to elsewhere if you wish, but you also need these other scripts it depends on: `tesstrain_utils.sh`, `language-specific.sh`.

## Tesstrain arguments
You can run `tesstrain.sh` on its own to print usage text [if not, you can check the source and it is in a string right at the top after the licence]. The important arguments are:

**1. `--fonts_dir FONTS_PATH`**

A path to a directory that contains the otf or ttf file of the font you want to train for.

**2. `--fontlist FONTS`**

The name or list of names of the font(s) you want to train for. That’s not necessarily the name of the file, nor even the 'Font name' Windows gives when you open it. A way to find out is run:

```bash
text2image --list_available_fonts --fonts_dir=i:/path/to/folder --fontconfig_tmpdir=i:/any/path/doesnt/matter
```

My font is SofiaPro-Black, and this command gives me the output `0: Sofia Pro Heavy`. So 'Sofia Pro Heavy' is the font name to use.

**3. `--lang LANG_CODE`**

The language you are training for, e.g. `eng`.

**4. `--langdata_dir DATADIR`**

From [langdata_lstm](https://github.com/tesseract-ocr/langdata_lstm) repository you need the folder of the language you are training for (e.g. `eng`), and `radical-stroke.txt`. You can download this whole thing, but note it is over 1 GB.

**5. [OPTIONAL] `--tessdata_dir TESSDATADIR`**

This is only needed if you want to refine an existing model, rather than training from scratch. Download the `.traineddata` file for the language you are training for, e.g. `eng.traineddata`. You can get it from [tessdata](https://github.com/tesseract-ocr/tessdata), [tessdata_best](https://github.com/tesseract-ocr/tessdata_best), [tessdata_fast](https://github.com/tesseract-ocr/tessdata_fast), or other places (you may be able to find people who share the models they’ve trained).

**6. [OPTIONAL] `training_text TEXTFILE`**

`text2image` by default will make the training images based on the training text for the language you are training for which comes with `langdata_lstm` (e.g. `eng.training_text`). If you like, you can provide your own text instead, for example if you need only particular characters or only digits. You need quite a lot of text (the default text has almost 200,000 lines), so you’ll probably need to write a simple script to generate some text for you into a text file and provide it with this option.

**7. [OPTIONAL] `--maxpages N`**

As mentioned, the training text tends to be very long, so you can limit the number of pages `text2image` will generate with this option. Pages refers to pages the tif image that is used for training will have. Seems to do 55 lines per page but I am not sure if that is always the case.

**8. `--linedata_only`**

We need to provide this flag in order to train for Tesseract 4 LSTM training rather than the legacy box training which was used in Tesseract 3.

**9. [OPTIONAL] `--save_box_tiff`**

If you provide this flag, it will save the tif image that is used for training in the output folder, so you can see what it was using.

**10. `--output_dir OUTPUTDIR`**

Needless to say, path to the folder it will output stuff to.

## Optional: Customise text2image output

You can modify `tesstrain_utils.sh` to pass different arguments to text2image. I didn’t want it to rotate the images because I will be using my model for screenshots which are never going to be rotated. In line 283 I added to the text2image call the following arguments: `--rotate_image=false --degrade_image=false`.

## Step 1: Initial tesstrain command

You need to preface `tesstrain.sh` with `sh` if you are running the script in cmd or PowerShell.

Example command:

```sh
sh tesstrain.sh --fonts_dir fonts --fontlist 'Sofia Pro Heavy' --lang eng  --linedata_only --langdata_dir langdata_lstm --tessdata_dir tessdata --save_box_tiff --maxpages 10 --output_dir train
```
With the following subfolders in the working directory:
- `fonts` : containing the otf/ttf of the font
- `langdata_lstm` : containing the aforementioned needed data from [langdata_lstm](https://github.com/tesseract-ocr/langdata_lstm)
- `tessdata` : containing the `eng.traineddata` model to refine
- `train` : empty folder for the output

## Step 2: Extract lstm from model

The next step is to extract the lstm portion of `eng.traineddata` (or whatever `.traineddata` file you are refining). You can skip this step if you are not refining an existing model.

```bash
combine_tessdata -e tessdata/eng.traineddata train/eng.lstm
```

The first path needs to link to the `.traineddata` file of the model you are refining. The second path can point anywhere; this is where to output the lstm portion of it. The `-e` stands for 'extract'.

## Step 3: Combine

Again, you can skip this step if you are not refining an existing model.

```bash
lstmtraining --continue_from train/eng.lstm --model_output output/Sofia --traineddata tessdata/eng.traineddata --train_listfile train/eng.training_files.txt --debug_interval -1 --max_iterations 500
```

- the first path (`--continue_from`) is to the `lstm` file we just extracted
- the second path (`--model_output`) can point anywhere; this is where to output your model and checkpoints of it during the training
- the third path (`--traineddata`) is to the model you are refining
- the fourth path (`--train_listfile`) is to a txt file that will have been created in Step 1 in the output folder you specified

Other things to note:
- `--debug interval -1` is [a workaround](https://github.com/tesseract-ocr/tesseract/issues/578)

## Step 4: Finalise

This last step is to stop the training and produce our final `.traineddata` file, which is the new model we’ve produced.

```bash
lstmtraining --stop_training --continue_from output/Sofia_checkpoint --traineddata tessdata/eng.traineddata --model_output output/Sofia.traineddata
```

- the first path (`--continue_from`) is to the latest checkpoint of your model. change `output/Sofia` to whatever you set the second path to in Step 3
- the second path (`--traineddata`) is to the model you are refining (optional)
- the third path (`--model_output`) is where to output your finished model

## Using your new model

Your new model will be in whatever path you provided in Step 4; `output/Sofia.traineddata` in my case. You can also see the checkpoints there and continue the training whenever you wish.

Tesseract uses the `.traineddata` models that are in the path in the `TESSDATA_PREFIX` environment variable, or in the path you provide it with `--tessdata-dir` (argument you can give tesseract when you call it). If you want to use your new model for an existing language, for example English, rename it `eng.traineddata` and place it there. Otherwise you can use it by providing in language whatever your traineddata file is called, for example in my case `-l Sofia`.

You can see the languages it detects by running `tesseract --list-langs`.

-----

## See also
- [Livezingy’s tesstrainsh-win](https://github.com/livezingy/tesstrainsh-win)

{% include fin.html %}
