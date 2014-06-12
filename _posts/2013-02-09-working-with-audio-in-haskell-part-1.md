---
layout: post
title: Working with Audio in Haskell (Part 1)
date: 2013-02-09
comments: false
---

### Introduction

[Digital signal processing](http://en.wikipedia.org/wiki/Digital_signal_processing), [audio processing](http://en.wikipedia.org/wiki/Audio_signal_processing) and the like are all rather complex topics of study. I have a personal interest in these fields as I try to create guitar processing effects from time to time. Today's post is all about taking the first steps in getting our hands on some audio and associated information from within Haskell.

### hsndfile

For today's post, I'll be using the library [hsndfile](http://www.haskell.org/haskellwiki/Hsndfile) to do all of the heavy lifting as far as opening audio files and interpreting information. The files that we'll work with will need to be in [wave format](http://en.wikipedia.org/wiki/WAV). The demonstration in this post will simply open an audio file, read some information about the file and then close the file.

### Project setup

I've created a Haskell project using cabal so that I can manage the hsndfile dependency locally to this application. You may already have this installed globally on your system, but if you follow along here, you should have it installed to your project in not time.

Setup your project as usual:

{% highlight bash %}
$ mkdir sndtest
$ cd sndtest
$ touch LICENSE
$ cabal init
{% endhighlight %}

Just select all of the defaults when setting up your project (well, that's what I did, anyway). We need to add [hsndfile](http://www.haskell.org/haskellwiki/Hsndfile) as a dependency to our project, so we'll specify this in our `sndtest.cabal` file. Open it up and make sure that your `build-depends` reads as follows.

{% highlight text %}
build-depends:         base ==4.5.*,
                       hsndfile ==0.5.3
{% endhighlight %}

Of course, you may have some different version of base, but here's where I was at anyway. Create a new file in your project called `Test.hs`. We'll now fill out this file with the code that will open a file, read its information, close the file and then display the information to screen.

{% highlight haskell %}
module Main where

import Sound.File.Sndfile as SF

main :: IO ()
main = do
  -- open the file that we want to know about
  f <- SF.openFile "test.wav" SF.ReadMode SF.defaultInfo

  -- read the information about the file out
  let info = SF.hInfo f

  -- close the file
  SF.hClose f

  -- display information about the file
  putStrLn $ "format:      " ++ (show $ SF.format info)
  putStrLn $ "sample rate: " ++ (show $ SF.samplerate info)
  putStrLn $ "channels:    " ++ (show $ SF.channels info)
  putStrLn $ "frames:      " ++ (show $ SF.frames info)
{% endhighlight %}

This is pretty straight forward. First up, we import `Sound.File.SndFile` qualified as `SF` so we know when we're using something from this import. Dissecting the main function, firstly we open the file using `openFile`. This function expects the path to the audio file (in this case we're using "test.wav" which by the way you'll have to find something), we're only reading from the file at the moment so we specify `ReadMode` and finally we have the info parameter which is useful to us when we're writing a new file (so we can tell it what format to write in, etc), but for reading we just use `defaultInfo`.

We now read the stream information about the file using `hInfo`, the result of which will give us back a value of type `Info`. This info packet tells us the number of frames in the file, the sample rate, number of channels, header and sample format, number of sections and if the file is seekable or not.

Now that we have the information from the stream, we can close it off. We do this with `hClose`. Now we can interrogate the Info value with a series of print statements. We've got a module ready to run, but we need to tell our project that it's the entry point to run. In the `sndtest.cabal` file, make sure you set your `main-is:` attribute like so.

{% highlight text %}
main-is:           Test.hs
{% endhighlight %}

### Build and Run<

We've created our project and finished our code. Let's build and run the application. The first build is going to take a bit more time as `cabal-dev` will need to resolve all of the dependencies that it doesn't yet have. Get this process moving with the following command.

{% highlight bash %}
$ cabal-dev install
{% endhighlight %}

All going well, you should be able to launch your executable and check out the results:

{% highlight text %}
$ dist/build/sndtest/sndtest
format:      Format {headerFormat = HeaderFormatWav, sampleFormat = SampleForm
atPcm16, endianFormat = EndianFile}
sample rate: 48000
channels:    2
frames:      302712
{% endhighlight %}

That's it for today's post. Hopefully I'll get back to this topic soon so I can write some more about audio processing.