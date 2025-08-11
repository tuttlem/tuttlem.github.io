---
layout: post
title: Building a Modular Synth in Clojure
date: 2025-08-11
comments: false
categories: [ clojure, modular ]
---

# Introduction

I’ve always liked the idea that a programming language can feel like a musical instrument.  
Last night, I decided to make that idea *very* literal.

The result is **rack** — a little Clojure module that models a modular synthesizer. It doesn’t aim to be a complete 
DAW or polished softsynth — this is more of an experiment: what if we could patch together oscillators and filters 
the way Eurorack folks do, but using s-expressions instead of patch cables?

As always, the code for this article is [up in my Github account](https://github.com/tuttlem/rack) to help you follow 
along.

## Why Clojure Feels Like a Good Fit

Clojure’s s-expressions are perfect for this kind of modeling.  

A synth module is, in some sense, just a little bundle of state and behavior. In OOP we might wrap that up in a class; 
in Clojure, we can capture it as a simple map, plus a few functions that know how to work with it.

The parentheses give us a “patch cable” feel — data and functions connected in readable chains.

## A 30-Second Synth Primer

Before we dive into code, a very quick crash course in some synth lingo:

- **VCO** (*Voltage-Controlled Oscillator*): Produces a periodic waveform — the basic sound source.
- **LFO** (*Low-Frequency Oscillator*): Like a VCO, but slower, used for modulation (wobble, vibrato, etc.).
- **VCA** (*Voltage-Controlled Amplifier*): Controls the amplitude of a signal, usually over time.

That’s enough to make the examples readable. We’re here for the Clojure, not the audio theory.

# Setup Audio

The first thing we need to do is open an audio output line.  

Java’s `javax.sound.sampled` API is low-level but accessible from Clojure with no extra dependencies.

Here’s the start of our code:

{% highlight clojure %}
(def ^:const sample-rate 48000.0)
(def ^:const bits-per-sample 16)
(def ^:const channels 1)
{% endhighlight %}

Three constants — a 48 kHz sample rate (good quality, not too CPU-heavy), 16-bit samples, and mono output.

## Starting the Audio Engine

{% highlight clojure %}
(defn ^SourceDataLine open-line
  ([] (open-line sample-rate))
  ([sr]
   (let [fmt (AudioFormat. (float sr) bits-per-sample channels true false) ; signed, little endian
         ^SourceDataLine line (AudioSystem/getSourceDataLine fmt)]
     (.open line fmt 4096)  ;; important: use (fmt, bufferSize) overload
     (.start line)
     line)))
{% endhighlight %}

Line by line:

1. **Function arity**: With no arguments, `open-line` uses our default sample rate. With one argument, you can pass a custom rate.
2. **`AudioFormat.`**: Creates a format object with:
   - `sr` as a float  
   - `bits-per-sample` bits per sample  
   - `channels` (mono)  
   - `true` for signed samples  
   - `false` for little-endian byte order
3. **`AudioSystem/getSourceDataLine`**: Asks the JVM for a line that matches our format.
4. **`.open`**: Opens the line with a buffer size of 4096 bytes — small enough for low latency, large enough to avoid dropouts.
5. **`.start`**: Starts audio playback.
6. Returns the `SourceDataLine` object so we can write samples to it.

## Finding available outputs

{% highlight clojure %}
(defn list-mixers []
  (doseq [[i m] (map-indexed vector (AudioSystem/getMixerInfo))]
    (println i ":" (.getName m) "-" (.getDescription m))))
{% endhighlight %}

This helper prints out all available audio devices (“mixers”) so you can choose one if your machine has multiple outputs. 
In cases where you're struggling to find the appropriate sound mixer, this function can help you diagnose these problems.

# Starting, Stopping, and Writing Audio

Opening an audio line is one thing — actually feeding it samples in real time is another.  
This is where we start talking about **frames**, **buffers**, and a little bit of number crunching.

## Writing audio frames

{% highlight clojure %}
(defn- write-frames! [^SourceDataLine line ^floats buf nframes]
  (let [^bytes out (byte-array (* 2 nframes))]
    (dotimes [i nframes]
      (let [s (int (Math/round (* 32767.0 (double (aget buf i)))))
            idx (* 2 i)]
        (aset-byte out idx       (unchecked-byte (bit-and s 0xFF)))
        (aset-byte out (inc idx) (unchecked-byte (bit-and (unsigned-bit-shift-right s 8) 0xFF)))))
    (.write line out 0 (alength out))))
{% endhighlight %}

Here’s what’s happening:

1. **Input**:  
   - `buf`: A float array of audio samples, each in the range $$ [-1.0, 1.0] $$.  
   - `nframes`: How many samples we want to send.
2. **Output**:  
   - `out`: A byte array holding the samples in 16-bit **little-endian** PCM format.

### Scaling floats to integers

Most audio hardware expects integers, not floats. In **16-bit PCM**, the range is $$ [-32768, 32767] $$.  

We scale a float $$ x $$ by $$ 32767.0 $$:

$$
s = \operatorname{round}(x \times 32767)
$$

For example:

$$ 
x = 1.0 \Rightarrow s = 32767 
$$ 

$$ 
x = -1.0 \Rightarrow s = -32767 
$$

_(close enough; the exact min value is special-cased in PCM)_

### Breaking into bytes

Because 16 bits = 2 bytes, we split the integer into:

- **Low byte**: $$ s \,\&\, 0xFF $$  
- **High byte**: $$ (s \gg 8) \,\&\, 0xFF $$

We store them in **little-endian** order — low byte first — so the audio hardware interprets them correctly.

<div class="mermaid">
graph LR
  A[Modules] --> B[Mix Function]
  B --> C[Float Buffer]
  C --> D[write-frames!]
  D --> E[16-bit PCM Bytes]
  E --> F[Audio Line]
  F --> G[Speakers / Headphones]
</div>

## Stopping audio cleanly

{% highlight clojure %}
(defn stop-audio! []
  (when (:running? @engine)
    (swap! engine assoc :running? false)
    (when-let [t (:thread @engine)]
      (try (.join t 500) (catch Throwable _))))
  (when-let [^SourceDataLine line (:line @engine)]
    (try (.drain line) (.stop line) (.close line)
         (catch Throwable _)))
  (reset! engine {:running? false :thread nil :line nil})
  :ok)
{% endhighlight %}

Stopping audio isn’t just hitting a “pause” button:

- **`:running?`** tells the audio thread to exit its loop.
- **`.join`** waits briefly for that thread to finish.
- **`.drain`** ensures any remaining samples in the buffer are played before stopping.
- **`.stop`** and **`.close`** free the hardware resources.

## Starting audio in real time

{% highlight clojure %}
(defn start-audio!
  "Start real-time audio. Call stop-audio! to end."
  ([] (start-audio! sample-rate 1024))
  ([sr block-size]
   (stop-audio!)
   (ensure-main-mixer!)
   (let [^SourceDataLine line (open-line sr)
         runner (doto
                  (Thread.
                    (fn []
                      (try
                        (let [ctx (make-ctx sr)]
                          (while (:running? @engine)
                            (let [cache (atom {})
                                  mix   ((:pull ctx) cache ctx "main-mixer" :out block-size)]
                              (write-frames! line mix block-size))))
                        (catch Throwable e
                          (.printStackTrace e))
                        (finally
                          (try (.drain line) (.stop line) (.close line)
                               (catch Throwable _))))))
                  (.setDaemon true))]
     (reset! engine {:running? true :thread runner :line line})
     (.start runner)
     :ok)))
{% endhighlight %}

This is where the magic loop happens:

1. **`block-size`** is how many frames we process at a time — small enough for low latency, large enough to avoid CPU overload.
2. We **open** the line, then spin up a **daemon thread** so it won’t block JVM shutdown.
3. Inside the loop:
   - `make-ctx` builds a context with our sample rate.
   - `(:pull ctx)` asks the “main mixer” module for the next `block-size` frames.
   - We hand those frames to `write-frames!` to push them to the audio hardware.
4. When `:running?` goes false, the loop exits, drains the buffer, and closes the line.

### How block size relates to latency

Audio latency is fundamentally the time between “we computed samples” and “we hear them.” For a block-based engine, 
one irreducible component is the **block latency**:

$$
\text{latency}_{\text{block}} = \frac{\text{block_size}}{\text{sample_rate}} \quad \text{seconds.}
$$

With our defaults:

$$ 
\text{block_size} = 1024\ \text{frames}  
$$

$$ 
\text{sample_rate} = 48000\ \text{Hz}
$$

So:

$$
\text{latency}_{\text{block}} = \frac{1024}{48000} \approx 0.02133\ \text{s} \approx 21.33\ \text{ms}.
$$

That’s the *one-way* block scheduling delay. Real perceived latency also includes:

- **Hardware/driver buffering**. We opened the line with `4096` bytes. At 16-bit mono (2 bytes/sample), that’s
  $$ 4096 \div 2 = 2048 $$ samples, i.e.:

  $$
  \text{latency}_{\text{line}} = \frac{2048}{48000} \approx 42.67\ \text{ms}.
  $$

- **OS and JVM scheduling overhead**, which tends to be small but non-zero.

A rough back-of-the-envelope estimate for output-path latency is:

$$
\text{latency}_{\text{total}} \approx \text{latency}_{\text{block}} + \text{latency}_{\text{line}} \approx 21.33\ \text{ms} + 42.67\ \text{ms} \approx 64\ \text{ms}.
$$

Lowering `block-size` reduces compute-to-play latency but increases CPU overhead (more wakeups, more function calls). 
Similarly, if your device/driver allows a smaller `.open` buffer, you can shave additional milliseconds — at the risk 
of underruns (clicks/pops). The sweet spot depends on your machine.

# Keeping Track of your Patch

A modular synth is basically:

- A set of **modules** (oscillators, filters, VCAs…)
- A set of **connections** between module outputs and inputs
- Some **engine state** for playback

We’ll keep these in atoms so we can mutate them interactively in the REPL.

{% highlight clojure %}
(defonce ^:private registry (atom {})) ; id -> module
(defonce ^:private cables   (atom #{})) ; set of {:from [id port] :to [id port] :gain g}
(defonce ^:private engine   (atom {:running? false :thread nil :line nil}))
{% endhighlight %}

- **registry**: All modules in the patch, keyed by ID.
- **cables**: All connections, each with `from`/`to` module IDs and ports, plus an optional gain.
- **engine**: Tracks whether audio is running, plus the playback thread and output line.

## Resetting the patch

{% highlight clojure %}
(defn reset-patch! []
  (reset! registry {})
  (reset! cables #{}))
{% endhighlight %}

This wipes everything so you can start a new patch. No modules. No cables.

## Adding modules and cables

{% highlight clojure %}
(defn- register! [m] (swap! registry assoc (:id m) m) (:id m))

(defn add-cable
  "Connect module output → input. Optional gain (defaults 1.0)."
  ([from-id from-port to-id to-port] (add-cable from-id from-port to-id to-port 1.0))
  ([from-id from-port to-id to-port gain]
   (swap! cables conj {:from [from-id (keyword from-port)]
                       :to   [to-id   (keyword to-port)]
                       :gain (double gain)})
   :ok))
{% endhighlight %}

`register!` stores a module in the registry and returns its ID.  
`add-cable` creates a connection between two module ports — think of it as digitally plugging in a patch cable.

These functions are basic data structure management.

## Setting parameters generically

{% highlight clojure %}
(defn set-param!
  "Set a module parameter (e.g., (set-param! \"vco1\" :freq 440.0))."
  [id k v]
  (when-let [st (:state (@registry id))]
    (swap! st assoc k v))
  :ok)
{% endhighlight %}

Because each module stores its state in a map, we can update parameters without knowing the module’s internals. This is 
one of the joys of modeling in Clojure — generic operations fall out naturally.

# Pulling Signal

Up to now we can open the device, stream audio, and keep track of a patch.  

But how do modules actually **produce** samples for each block?

We use a **pull-based** model: when the engine needs `N` frames from a module’s output port, it *asks* that module to 
render. If the module depends on other modules (its inputs), it pulls *those* first, mixes/filters them, and returns a 
buffer. 

This naturally walks the patch graph from outputs back to sources and avoids doing work we don’t need.

## Connections into a port

{% highlight clojure %}
(defn- connections-into [to-id to-port]
  (filter (fn [{:keys [to]}] (= to [to-id to-port])) @cables))
{% endhighlight %}

Plain data → simple query:

- We scan `@cables` for any connection whose `:to` is exactly `[to-id to-port]`.
- The result is a (possibly empty) sequence of “incoming patch cables”.

This is intentionally tiny; the interesting part comes when we combine the sources.

## Summing signals into a buffer

{% highlight clojure %}
(defn- sum-into
  "Sum all signals connected to [id port] into a float-array of nframes."
  [cache ctx id port nframes]
  (let [conns (connections-into id port)]
    (if (seq conns)
      (let [acc (float-array nframes)]
        (doseq [{:keys [from gain]} conns
                :let [[src-id src-port] from
                      buf ((:pull ctx) cache ctx src-id src-port nframes)
                      g (float gain)]]
          (dotimes [i nframes]
            (aset-float acc i (+ (aget acc i) (* g (aget ^floats buf i))))))
        acc)
      (float-array nframes))) )
{% endhighlight %}

Conceptually, if $$ \{x_k[i]\} $$ are the input buffers (per-connection) and $$ g_k $$ are the per-cable gains, the 
mixed signal is:

$$
y[i] \;=\; \sum_{k=1}^{K} g_k \, x_k[i], \quad i = 0,1,\dots,n\!-\!1
$$

Where:
- $$ n = $$ `nframes` (the block size),
- $$ K = $$ number of incoming connections into `[id port]`.

Implementation notes:

- We allocate `acc` as our accumulator buffer and initialize it to zeros.
- For each incoming connection:
  - We **pull** from the source `(src-id, src-port)` via `(:pull ctx)`.
  - We convert the cable’s `gain` to a `float` once (keeps the inner loop tight).
  - We add the scaled samples into `acc`.
- If there are **no** connections, we return a zeroed buffer (silence). This is a convenient “ground” for the graph.

Time complexity for this step is $$ O(K \cdot n) $$ per port, which is exactly what you’d expect for mixing $$ K $$ streams.

## Rendering a port with per-block memoization

{% highlight clojure %}
(defn- render-port
  "Render [id port] with memoization for this audio block."
  [cache ctx id port nframes]
  (if-let [cached (get @cache [id port])]
    cached
    (let [m (@registry id)]
      (when-not m (throw (ex-info (str "Unknown module: " id) {})))
      (let [outbuf ((:process m) ctx m (keyword port) nframes)]
        (swap! cache assoc [id port] outbuf)
        outbuf))))
{% endhighlight %}

Why memoize? Consider one VCO feeding two different modules, both ultimately ending at your main mixer. In a naive pull 
model, the VCO would be recomputed twice per block. We avoid that by caching the result buffer for `[id port]` the 
**first** time it’s pulled in a block:

- `cache` is an atom (a per-block memo table).
- If we’ve already computed `[id port]`, return the cached buffer.
- Otherwise, we call the module’s `:process` function, stash the buffer, and return it.

This makes the pull model efficient even when the patch graph has lots of fan-out.

### The context object (`ctx`)

{% highlight clojure %}
;; ctx provides a way for modules to pull inputs
(defn- make-ctx [sr]
  {:sr sr
   :pull (fn [cache ctx id port nframes]
           (render-port cache ctx id port nframes))})
{% endhighlight %}

`ctx` bundles:

- `:sr` — the sample rate (modules often need it for phase increments, envelopes, etc.).
- `:pull` — the function modules call to obtain inputs. This keeps module code simple and testable.

Because `:pull` closes over `render-port`, modules don’t need to know about caching details or registry lookups — 
they just ask the world for “the buffer at `[id port]`”.

<div class="mermaid">
flowchart TD
  subgraph Block["nframes block render"]
    MM["Main Mixer :process"] -->|:pull osc1 :out| VCO
    MM -->|:pull lfo1 :out| LFO
    VCO -->|:pull mod :in| SUM
    LFO -->|:pull mod :in| SUM
    SUM["sum-into"] -->|float buf| MM
  end
  style SUM fill:#eef,stroke:#77a
  style MM fill:#efe,stroke:#7a7
  style VCO fill:#fee,stroke:#a77
  style LFO fill:#fee,stroke:#a77
  Cache["Per-block cache"] --- MM
  Cache --- VCO
  Cache --- LFO
</div>

The cache sits beside the graph for the duration of a single block render. Any subsequent pulls of the 
same `[id port]` return the memoized buffer.

## Numerical notes (clipping and headroom)

Mixing is a straight sum. If your sources are near full-scale and you add them, you can exceed \([-1, 1]\) in the 
mixed float domain, which will later clip when we convert to 16-bit in `write-frames!`. Options to consider (later):

- Normalize or soft-clip in the mixer: \( y[i] \leftarrow \tanh(y[i]) \) or a gentle limiter.
- Encourage sub-unity `gain` on cables feeding into mixers.
- Keep VCO defaults conservative (e.g., amplitude \(0.2\) or \(0.5\)) to preserve headroom.

## Modules

With all of the setup finished, we can finally create some modules — the building blocks of a patch.

### The module shape: `mk-*` vs. public constructor

Each module comes in two layers:

- A **maker** (`mk-vco`, `mk-lfo`, `mk-vca`, …): returns a plain Clojure **map** that describes the module:
  - `:id`, `:type`, and a mutable `:state` atom
  - `:inputs` / `:outputs` port sets
  - a `:process` function with the signature  
    `(:process m) ctx m requested-port nframes -> float-array`
- A **public constructor** (`vco`, `lfo`, `vca`, …): a thin wrapper that calls the maker and then `register!`s the resulting module into the global `registry`. This pattern keeps the module definition pure/data-first and the side‑effect (registration) explicit.

The engine always drives modules through `:process`. If a module needs other signals, it **pulls** them 
via `sum-into` (which uses the per‑block cache and respects cabling).

## Voltage Controlled Oscillator (VCO)

A **VCO** produces periodic waveforms at audio rates. In this design:

- Base frequency is `:freq` (Hz).
- A control input `:pitch` (typically from an LFO or envelope) modulates the frequency by `:pitch-depth` (Hz per unit CV).
- Phase evolves per sample as  
  $$
  [
  \varphi_{i+1} = \varphi_i + \frac{2\pi}{\text{sr}}\; f_i
  \quad\text{where}\quad
  f_i = \max\!\big(0,\; \text{freq} + \text{pitch_depth}\cdot \text{pitch}[i]\big).
  ]
  $$
- We render four classic shapes from the same phase accumulator: sine, square, saw, and reverse‑saw, each scaled by `:amp`.

Note on outputs: this VCO exposes **`:sine-out`**, **`:square-out`**, **`:saw-out`**, and **`:rev-saw-out`**. When 
cabling, target one of those (e.g., `:sine-out`), not `:out`.

{% highlight clojure %}
(defn- mk-vco
  [id {:keys [freq amp pitch-depth]
       :or   {freq 220.0 amp 0.2 pitch-depth 50.0}}]
  (let [state (atom {:phase 0.0
                     :freq (double freq)
                     :amp (double amp)
                     :pitch-depth (double pitch-depth)})]
    {:id id
     :type :vco
     :state state
     :outputs #{:sine-out :square-out :saw-out :rev-saw-out}
     :inputs #{:pitch}
     :process
     (fn [ctx m port nframes]
       (let [{:keys [phase freq amp pitch-depth]} @(:state m)
             sr (:sr ctx)
             pitch-buf (sum-into (atom {}) ctx (:id m) :pitch nframes)
             two-pi (* 2.0 Math/PI)
             ;; output buffers
             sine-buf (float-array nframes)
             square-buf (float-array nframes)
             saw-buf (float-array nframes)
             rev-saw-buf (float-array nframes)]
         ;; run the block, capture final phase
         (let [final-ph
               (loop [i 0, ph phase]
                 (if (< i nframes)
                   (let [hz (max 0.0 (+ freq (* pitch-depth (aget ^floats pitch-buf i))))
                         ph2 (let [p (+ ph (/ (* two-pi hz) sr))]
                               (if (>= p two-pi) (- p two-pi) p))
                         norm-phase (/ ph two-pi) ; 0..1 based on current phase
                         sine (Math/sin ph)
                         square (if (< ph Math/PI) 1.0 -1.0)
                         saw (- (* 2.0 norm-phase) 1.0)
                         rev-saw (- 1.0 (* 2.0 norm-phase))]
                     (aset-float sine-buf i (float (* amp sine)))
                     (aset-float square-buf i (float (* amp square)))
                     (aset-float saw-buf i (float (* amp saw)))
                     (aset-float rev-saw-buf i (float (* amp rev-saw)))
                     (recur (inc i) ph2))
                   ph))]
           ;; persist the advanced phase
           (swap! (:state m) assoc :phase (double final-ph)))
         ;; return the requested port
         (case port
           :sine-out sine-buf
           :square-out square-buf
           :saw-out saw-buf
           :rev-saw-out rev-saw-buf
           (float-array nframes))))}))

(defn vco
  "Create and register a Voltage Controlled Oscillator (VCO) module.

  The VCO generates multiple waveforms and supports pitch modulation via the :pitch input.

  Inputs:
    :pitch — control signal in [-1.0 .. +1.0] range, multiplied by :pitch-depth (Hz)
             and added to :freq.

  Outputs:
    :sine-out, :square-out, :saw-out, :rev-saw-out

  Parameters:
    :freq         — base frequency in Hz (default = 220.0).
    :amp          — peak amplitude (default = 0.2).
    :pitch-depth  — Hz per unit of :pitch CV (default = 50.0).

  Example:
    (vco \"osc1\" {:freq 440.0 :amp 0.25 :pitch-depth 20.0})
    (lfo \"mod1\" {:freq 5.0 :amp 1.0})
    (add-cable \"mod1\" \"sine-out\" \"osc1\" \"pitch\")
    (add-cable \"osc1\" \"sine-out\" \"main-mixer\" \"in\")"
  ([id] (vco id {}))
  ([id params] (register! (mk-vco id params))))
{% endhighlight %}

## Low Frequency Oscillator (LFO)

An **LFO** is just an oscillator that runs at **control** rates (typically < 20 Hz). We use it to modulate other 
parameters (pitch, amplitude, filter cutoff…). The math is identical to the VCO’s phase increment, just at a lower 
`:freq`, and the output is usually **not** sent directly to the speakers.

{% highlight clojure %}
(defn- mk-lfo
  [id {:keys [freq amp] :or {freq 2.0 amp 1.0}}]
  (let [state (atom {:phase 0.0 :freq (double freq) :amp (double amp)})]
    {:id id
     :type :lfo
     :state state
     :outputs #{:sine-out}
     :inputs #{}
     :process
     (fn [ctx m port nframes]
       (let [{:keys [phase freq amp]} @(:state m)
             sr (:sr ctx)
             out (float-array nframes)
             two-pi (* 2.0 Math/PI)]
         (let [final-ph
               (loop [i 0, ph phase]
                 (if (< i nframes)
                   (let [ph2 (let [p (+ ph (/ (* two-pi freq) sr))]
                               (if (>= p two-pi) (- p two-pi) p))
                         s (* amp (Math/sin ph))]
                     (aset-float out i (float s))
                     (recur (inc i) ph2))
                   ph))]
           (swap! (:state m) assoc :phase (double final-ph)))
         out))}))

(defn lfo
  "Create and register a Low Frequency Oscillator (LFO) module.

  Outputs:
    :sine-out — control-rate sine in [-amp .. +amp].

  Parameters:
    :freq — Hz (default = 2.0).
    :amp  — peak amplitude (default = 1.0).

  Example:
    (lfo \"mod1\" {:freq 5.0 :amp 1.0})
    (vco \"osc1\" {:freq 220.0 :amp 0.2})
    (add-cable \"mod1\" \"sine-out\" \"osc1\" \"pitch\")"
  ([id] (lfo id {}))
  ([id params] (register! (mk-lfo id params))))
{% endhighlight %}

## Voltage Controlled Amplifier (VCA)

A **VCA** scales an audio signal by a **gain** derived from a control voltage (CV). A common musical use is tremolo: 
feed a VCO into `:in`, an LFO into `:cv`, and you’ll hear periodic amplitude variation.

We map CV $$ \in [-1,1] $$ to gain $$ \in [0,1] $$ (plus an optional `:bias`) using:
$$
[
\text{gain}_i = \operatorname{clamp}_{[0,1]}\!\left(\text{bias} + \tfrac{1}{2}(\text{cv}[i] + 1)\right).
]
$$

The output sample is $$ y[i] = \text{gain}_i \cdot x[i] $$.

{% highlight clojure %}
(defn- mk-vca [id {:keys [bias] :or {bias 0.0}}]
  (let [state (atom {:bias (double bias)})]
    {:id id
     :type :vca
     :state state
     :inputs #{:in :cv}        ;; audio in, control voltage in [-1..1]
     :outputs #{:out}
     :process
     (fn [ctx m port nframes]
       (let [in  (sum-into (atom {}) ctx (:id m) :in nframes)
             cv  (sum-into (atom {}) ctx (:id m) :cv nframes)
             out (float-array nframes)
             bias (:bias @(:state m))]
         (dotimes [i nframes]
           ;; gain = max(0, bias + 0.5*(cv+1))  -> maps cv [-1..1] to [0..1]
           (let [gain (max 0.0 (min 1.0 (+ bias (* 0.5 (+ 1.0 (aget ^floats cv i))))))
                 s (* gain (aget ^floats in i))]
             (aset-float out i (float s))))
         out))}))

(defn vca
  "Create and register a Voltage Controlled Amplifier (VCA) module.

  Inputs:
    :in   — audio signal (float samples in [-1.0..1.0]).
    :cv   — control voltage signal in [-1.0..+1.0].

  Output:
    :out  — amplified audio.

  Parameter:
    :bias — DC offset added before clamping gain to [0..1] (default 0.0).

  Example (tremolo):
    (vco \"osc\" {:freq 220 :amp 0.25})
    (lfo \"mod\" {:freq 5.0 :amp 1.0})
    (vca \"amp1\" {:bias 0.5})
    (add-cable \"osc\" \"sine-out\" \"amp1\" \"in\")
    (add-cable \"mod\" \"sine-out\" \"amp1\" \"cv\")
    (add-cable \"amp1\" \"out\" \"main-mixer\" \"in\")"
  ([id] (vca id {}))
  ([id params] (register! (mk-vca id params))))
{% endhighlight %}

# A quick “hello patch”

Tie it together with a gentle vibrato + tremolo:

{% highlight clojure %}
(reset-patch!)
(ensure-main-mixer!)

(lfo "vib" {:freq 6.0 :amp 1.0})
(lfo "trem" {:freq 4.0 :amp 1.0})
(vco "osc" {:freq 220.0 :amp 0.2 :pitch-depth 8.0})
(vca "amp" {:bias 0.3})

(add-cable "vib"  :sine-out "osc" :pitch)   ;; vibrato
(add-cable "osc"  :sine-out "amp" :in)
(add-cable "trem" :sine-out "amp" :cv)      ;; tremolo
(add-cable "amp"  :out      "main-mixer" :in)

(start-audio!)
;; tweak live:
;; (set-param! "osc"  :freq 330.0)
;; (set-param! "vib"  :freq 5.0)
;; (set-param! "trem" :freq 8.0)
;; (set-param! "amp"  :bias 0.5)
;; ...
(stop-audio!)
{% endhighlight %}

With these three modules you can already explore a surprising amount of sonic territory, and the pattern for adding 
more is clear: define a small `:state`, specify ports, and implement `:process` that uses `sum-into` for inputs and 
writes a block-sized buffer for outputs.

