---
layout: post
title: A Primer on Radio Signals
date: 2025-01-30
comments: false
categories: [ "radio", "rf", "hackrf" ]
---

# Introduction

Radio signals surround us every day—whether it’s FM radio in your car, WiFi on your laptop, or Bluetooth connecting 
your phone to wireless headphones. These signals are all based on the same fundamental principles: frequency, 
modulation, and bandwidth.

In today's article, I want to go through some of the fundamentals:

* How do radio signals travel through the air?
* What is the difference between AM and FM?
* How can multiple signals exist without interfering?
* What is digital modulation (FSK & PSK), and why does it matter?
* How can we capture and transmit signals using HackRF?

By the end of this article, we should be running some experiments with our own software defined radio devices.

## Getting Setup

Before getting started in this next section, you'll want to make sure that you have some specific software installed 
on your computer, as well as a software defined radio device.

I'm using a [HackRF One](https://greatscottgadgets.com/hackrf/one/) as my device of choice as it integrates with all of 
the software that I use.

Make sure you have the following packages installed:

* `hackrf`
* `gqrx`
* `gnuradio`

{% highlight shell %}
sudo pacman -S hackrf gqrx gnuradio
{% endhighlight %}

Create yourself a new python project and virtual environment, and install the libraries that will wrap some of these 
tools to give you an easy to use programming environment for your software defined radio.

{% highlight shell %}
# create your virtual environment
python -m venv venv

# activate
source venv/bin/activate

# install
pip install pyhackrf matplotlib
{% endhighlight %}

The output of some of these examples are graphs, so we use `matplotlib` to save them to look at later.

## Be Responsible!

A note before we get going - you will be installing software that will allow you to transmit signals that could 
potentially be dangerous and against the law, so before transmitting:

* **Know the laws** – Unlicensed transmission can interfere with emergency services.
* **Use ISM Bands** – 433 MHz, 915 MHz, 2.4 GHz are allowed for low-power use.
* **Start in Receive Mode** – Learning to capture first avoids accidental interference.

# Basics of Radio Waves

## What is a Radio Signal?

A radio signal is a type of electromagnetic wave that carries information through the air. These waves travel at the 
speed of light and can carry audio, video, or digital data.

Radio waves are defined by:

1. **Frequency (Hz)** – How fast the wave oscillates.
2. **Wavelength (m)** – The distance between peaks.
3. **Amplitude (A)** – The height of the wave (strength of the signal).

A high-frequency signal oscillates faster and has a shorter wavelength. A low-frequency signal oscillates slower and 
has a longer wavelength.

Since **radio waves travel at the speed of light**, their wavelength ($$ \lambda $$) can be calculated using:

$$
\lambda = \frac{c}{f}
$$

Where:
- $$ \lambda $$ = Wavelength in meters
- $$ c $$ = Speed of light ~($$ 3.0 \times 10^8 $$ m/s)
- $$ f $$ = Frequency in Hz

## What is Frequency?

Frequency is measured in Hertz (Hz), meaning cycles per second. You may have heard of kilohertz, megahertz, and 
gigahertz. These are all common frequency units:

| Unit | Hertz         |
|------|---------------|
| 1 kHz (kilohertz) | 1,000 Hz      |
|1 MHz (megahertz) |  1,000,000 Hz |
|1 GHz (gigahertz) | 1,000,000,000 Hz |

Every device that uses radio has a specific frequency range. For example:

* **AM Radio**: 530 kHz – 1.7 MHz
* **FM Radio**: 88 MHz – 108 MHz
* **WiFi (2.4 GHz Band)**: 2.4 GHz – 2.5 GHz
* **Bluetooth**: 2.4 GHz
* **GPS Satellites**: 1.2 GHz – 1.6 GHz

Each of these frequencies belongs to the radio spectrum, which is carefully divided so that signals don’t interfere 
with each other.

## What is Bandwidth?

Bandwidth is the amount of frequency space a signal occupies.

A **narrowband** signal (like AM radio) takes up less space. A **wideband** signal (like WiFi) takes up more space to 
carry more data.

Example:

* **AM Radio Bandwidth**: ~10 kHz per station
* **FM Radio Bandwidth**: ~200 kHz per station
* **WiFi Bandwidth**: 20–80 MHz (much larger, more data)

The more bandwidth a signal has, the better the quality (or more data it can carry).

## How Can Multiple Signals Exist Together?

One analogy you can use is imagining a highway—each lane is a different frequency. Cars (signals) stay in their lanes and don’t interfere unless they overlap (cause interference). This is why:

* FM stations are spaced apart (88.1 MHz, 88.3 MHz, etc.).
* WiFi has channels (1, 6, 11) to avoid congestion.
* TV channels each have a dedicated frequency band.

This method of dividing the spectrum is called Frequency Division Multiplexing (FDM).

Using the following python code, we can visualise FDM in action by sweeping the FM spectrum:

{% highlight python %}
# Basic FM Spectrum Capture

from hackrf import *
import matplotlib.pyplot as plt
import numpy as np
from scipy.signal import welch

with HackRF() as hrf:
    hrf.sample_rate = 20e6  # 20 MHz sample rate
    hrf.center_freq = 104.5e6  # FM Radio

    samples = hrf.read_samples(2e6)  # Capture 2 million samples

    # Compute PSD using Welch’s method (handling complex IQ data)
    freqs, psd_values = welch(samples, fs=hrf.sample_rate, nperseg=1024, return_onesided=False)

    # Convert frequency axis to MHz
    freqs_mhz = (freqs - (hrf.sample_rate / 2)) / 1e6 + (hrf.center_freq / 1e6)

    # Plot Power Spectral Density
    plt.figure(figsize=(10, 5))
    plt.plot(freqs_mhz, 10 * np.log10(psd_values))  # Convert power to dB
    plt.xlabel('Frequency (MHz)')
    plt.ylabel('Power Spectral Density (dB/Hz)')
    plt.title(f'FM Radio Spectrum at {hrf.center_freq/1e6} MHz')
    plt.grid()

    # Save and show
    plt.savefig("fm_spectrum.png")
    plt.show(block=True)
{% endhighlight %}

Running this code gave me the following resulting plot (it will be different for you depending on where you live!):

![FM Spectrum]({{ site.url }}/assets/fm_spectrum.png)

Each **sharp peak** that you see here represents an FM station at a unique frequency. These are the lanes.

# Understanding Modulation

## What is Modulation?

Radio signals don’t carry useful information by themselves. Instead, they use modulation to encode voice, music, or 
data.

There are two main types of modulation:

1. **Analog Modulation** – Used for traditional radio (AM/FM).
2. **Digital Modulation** – Used for WiFi, Bluetooth, GPS, and modern systems.

## AM (Amplitude Modulation)

AM works by **varying the height (amplitude) of the carrier wave** to encode audio.

As an example, the carrier frequency stays the same (e.g., 900 kHz), but the amplitude changes based on the sound wave.

AM is prone to static noise (because any electrical interference changes amplitude).

You can capture a sample of AM signals using the `hackrf_transfer` utility that was installed on your system:

{% highlight shell %}
hackrf_transfer -r am_signal.iq -f 900000 -s 2000000
{% endhighlight %}

This will capture **AM signals** at **900 kHz** into a file for later analysis.

We can write some python to capture an AM signal and plot the samples so we can visualise this information.

{% highlight python %}
# AM Signal Demodulation

from hackrf import *
import matplotlib.pyplot as plt
import numpy as np

with HackRF() as hrf:
    hrf.sample_rate = 10e6  # 10 MHz sample rate
    hrf.center_freq = 693e6  # 693 MHz AM station

    samples = hrf.read_samples(1e6)  # Capture 1M samples

    # AM Demodulation - Extract Magnitude (Envelope Detection)
    demodulated = np.abs(samples)

    # Plot Demodulated Signal
    plt.figure(figsize=(10, 5))
    plt.plot(demodulated[:5000])  # Plot first 5000 samples
    plt.xlabel("Time")
    plt.ylabel("Amplitude")
    plt.title("AM Demodulated Signal")
    plt.grid()

    # Save and show
    plt.savefig("am_demodulated.png")
    plt.show(block=True)
{% endhighlight %}

Running this code should give you a plot of what's happening at 693 MHz:

![AM Demodulated]({{ site.url }}/assets/am_demodulated.png)

The plot above represents the **amplitude envelope** of a real AM radio transmission.

* The X-axis represents **time**, while the Y-axis represents **amplitude**.
* The **variations in amplitude** correspond to the **audio signal** encoded by the AM station.

## FM (Frequency Modulation)

FM works by **varying the frequency of the carrier wave** to encode audio.

As an example, the amplitude stays constant, but the frequency changes based on the audio wave.

FM is clearer than AM because it ignores amplitude noise.

You can capture a sample of FM signals with the following:

{% highlight shell %}
hackrf_transfer -r fm_signal.iq -f 100300000 -s 2000000
{% endhighlight %}

This will capture an **FM station** at **100.3 MHz**.

We can write some python code to capture and demodulate an FM signal as well:

{% highlight python %}
from hackrf import *
import matplotlib.pyplot as plt
import numpy as np

with HackRF() as hrf:
    hrf.sample_rate = 2e6  # 2 MHz sample rate
    hrf.center_freq = 104.5e6  # Example FM station

    samples = hrf.read_samples(1e6)

    # FM Demodulation - Phase Differentiation
    phase = np.angle(samples)  # Extract phase
    fm_demodulated = np.diff(phase)  # Differentiate phase

    # Plot FM Demodulated Signal
    plt.figure(figsize=(10, 5))
    plt.plot(fm_demodulated[:5000])  # Plot first 5000 samples
    plt.xlabel("Time")
    plt.ylabel("Frequency Deviation")
    plt.title("FM Demodulated Signal")
    plt.grid()

    # Save and show
    plt.savefig("fm_demodulated.png")
    plt.show(block=True)
{% endhighlight %}

If you pick a frequency that has a local radio station, you should get a strong signal like this:

![FM Demodulated]({{ site.url }}/assets/fm_demodulated.png)

Unlike AM, where the signal's amplitude changes, FM signals encode audio by varying the frequency of the carrier wave.

The graph above shows the **frequency deviation** over time:
* The X-axis represents **time**, showing how the signal changes.
* The Y-axis represents **frequency deviation**, showing how much the carrier frequency shifts.
* The spikes and variations represent audio modulation, where frequency shifts encode sound.

If your FM demodulation appears too noisy:
1. Try tuning to a stronger station (e.g., 100.3 MHz).
2. Increase the sample rate for a clearer signal.
3. Apply a low-pass filter to reduce noise in post-processing.

## Bandwidth of a Modulated Signal

Modulated signals require bandwidth ($$ B $$), and the amount depends on the modulation type.

### AM

The total bandwidth required for AM signals is:

$$
B = 2f_m
$$

Where:
- $$ B $$ = Bandwidth in Hz
- $$ f_m $$ = Maximum audio modulation frequency in Hz

If an AM station transmits audio up to **5 kHz**, the bandwidth is:

$$
B = 2 \times 5\text{kHz} = 10\text{kHz}
$$

This explains why **AM radio stations typically require ~10 kHz per station**.

### FM

The bandwidth required for an FM signal follows **Carson’s Rule**:

$$
B = 2 (f_d + f_m)
$$

Where:
- $$ f_d $$ = Peak frequency deviation (how much the frequency shifts)
- $$ f_m $$ = Maximum audio frequency in Hz

For an **FM station** with **a deviation of 75 kHz** and **max audio frequency of 15 kHz**, the total bandwidth is:

$$
B = 2 (75 + 15) = 180 \text{kHz}
$$

This explains why **FM radio stations require much more bandwidth (~200 kHz per station).**

# Digital Modulation

For digital signals, it's important to be able to transmit binary data (1's and 0's). These methods of modulation are 
focused on making this process much more optimal than what the analog counterparts could provide.

## What is FSK (Frequency Shift Keying)?

FSK is **digital FM**—instead of smoothly varying frequency like FM radio, it switches between two frequencies for 0's and 
1's. This method of modulation is used in technologies like Bluetooth, LoRa, and old-school modems.

Example:

* A **"0"** might be transmitted as a **lower frequency** (e.g., 915 MHz).
* A **"1"** might be transmitted as a **higher frequency** (e.g., 917 MHz).
* The receiver detects these frequency changes and reconstructs the **binary data**.

## What is PSK (Phase Shift Keying)?

PSK is **digital AM**—instead of changing amplitude, it shifts the phase of the wave. This method of modulation is used 
in technologies like WiFi, GPS, 4G LTE, Satellites.

Example:

* **0° phase shift** = Binary 0
* **180° phase shift** = Binary 1
* More advanced PSK (like **QPSK**) uses **four phase shifts** (0°, 90°, 180°, 270°) to send **two bits per symbol** (faster data transmission).

# Wrapping Up

In this post, we explored the fundamentals of radio signals—what they are, how they work, and how different modulation 
techniques like AM and FM allow signals to carry audio through the air.

This really is only the start of what you can get done with software defined radio. Here are some further resources to 
check out:

* ["The ARRL Handbook for Radio Communications"](https://www.arrl.org/arrl-handbook-2023) – A comprehensive guide to radio theory.
* [Great Scott Gadgets HackRF One](https://greatscottgadgets.com/hackrf/one/) – The official HackRF documentation.
* [RTL-SDR Blog](https://www.rtl-sdr.com/) – A great site for learning SDR with practical tutorials.
* ["SignalsEverywhere"](https://www.youtube.com/channel/UCGvakD8eB8Asnz8ETmQBlVw) – Video tutorials on SDR and radio hacking.

