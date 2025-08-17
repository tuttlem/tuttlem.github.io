---
layout: post
title: Getting Started with ClojureScript
date: 2025-08-17
comments: false
categories: [clojure, clojurescript, beginner, cljsbuild, tutorial]
---

# Introduction

I recently decided to dip my toes into ClojureScript. As someone who enjoys exploring different language ecosystems, I 
figured getting a basic "Hello, World!" running in the browser would be a fun starting point. It turns out that even 
this small journey taught me quite a bit about how ClojureScript projects are wired together.

This post captures my first successful setup: a minimal ClojureScript app compiled with `lein-cljsbuild`, rendering 
output in the browser console.

# A Rough Start

I began with the following command to create a new, blank project:

{% highlight shell %}
lein new cljtest
{% endhighlight %}

First job from here is to organise dependencies, and configure the build system for the project.

## `project.clj`

There's a few things to understand in the configuration of the project:

* We add `org.clojure/clojurescript "1.11.132"` as a dependency
* To assist with our builds, we add the plugin `lein-cljsbuild "1.1.8"`
* The source path is normally `src`, but we change this for ClojureScript to `src-cljs`
* The output will be javascript output for a website, and all of our web assets go into `resources/public`

{% highlight clojure %}
(defproject cljtest "0.1.0-SNAPSHOT"
  :min-lein-version "2.9.1"
  :description "Minimal ClojureScript Hello World"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/clojurescript "1.11.132"]]
  :plugins [[lein-cljsbuild "1.1.8"]]
  :source-paths ["src-cljs"]
  :clean-targets ^{:protect false} ["resources/public/js" "target"]

  :cljsbuild
  {:builds
   {:dev
    {:source-paths ["src-cljs"]
     :compiler {:main cljtest.core
                :output-to "resources/public/js/main.js"
                :output-dir "resources/public/js/out"
                :asset-path "js/out"
                :optimizations :none
                :source-map true
                :pretty-print true}}

    :prod
    {:source-paths ["src-cljs"]
     :compiler {:main cljtest.core
                :output-to "resources/public/js/main.js"
                :optimizations :advanced
                :pretty-print false}}}})
{% endhighlight %}

We have two different build configurations here: `dev` and `prod`.

The `dev` configuration focuses on being much quicker to build so that the change / update cycle during development is 
quicker. Source maps, pretty printing, and no optimisations provide the verbose output appropriate for debugging.

The `prod` configuration applies all the optimisations. This build is slower, but produces one single output file: 
`main.js`. This is the configuration that you use to "ship" your application.

## Your First ClojureScript File

Place this in `src-cljs/cljtest/core.cljs`:

{% highlight clojurescript %}
(ns cljtest.core)

(enable-console-print!)
(println "Hello from ClojureScript!")
{% endhighlight %}

## HTML Page to Load It

Create a file at `resources/public/index.html`:

{% highlight html %}
<!doctype html>
<html>
  <head><meta charset="utf-8"><title>cljtest</title></head>
  <body>
    <h1>cljtest</h1>
    <script src="js/out/goog/base.js"></script>
    <script src="js/main.js"></script>
    <script>goog.require('cljtest.core');</script>
  </body>
</html>
{% endhighlight %}

## Build & Run

Compile your dev build:

{% highlight shell %}
lein clean
lein cljsbuild once dev
{% endhighlight %}

Then open `resources/public/index.html` in your browser, and check the developer console — you should see your message.

If you want to iterate while coding:

{% highlight shell %}
lein cljsbuild auto dev
{% endhighlight %}

When you're ready to build a production bundle:

{% highlight shell %}
lein cljsbuild once prod
{% endhighlight %}

Then you can simplify the HTML:

{% highlight html %}
<script src="js/main.js"></script>
{% endhighlight %}

No `goog.require` needed — it all gets bundled.

# Step it up

Next, we'll step up to something a little more useful. We'll put together a table of names that we can add, edit, 
delete, etc. Just a really simple CRUD style application.

In order to do this, we're going to rely on a pretty cool library called [reagent](https://reagent-project.github.io/).

We add the following dependency to `project.clj`:

{% highlight clojure %}
[reagent "1.0.0"]
{% endhighlight %}

## State

Our little application requires some state:

{% highlight clojure %}
(defonce names (r/atom [{:id 1 :name "Alice"}
                        {:id 2 :name "Bob"}]))

(defonce next-id (r/atom 3))
(defonce editing-id (r/atom nil))
(defonce edit-text (r/atom ""))
{% endhighlight %}

`names` is the currentl list of names. `next-id` gives us the next value that we'll use an ID when adding a new 
record. `editing-id` and `edit-text` manage the state for updates.

## Table

We can now render our table using a simple function:

{% highlight clojure %}
(defn name-table []
  [:div
   [:h2 "Name Table"]
   [:table
    [:thead
     [:tr [:th "Name"] [:th "Edit"] [:th "Delete"]]]
    [:tbody
     (for [n @names]
       ^{:key (:id n)} [name-row n])]]
   [:div
    [:input {:placeholder "New name"
             :value @edit-text
             :on-change #(reset! edit-text (.. % -target -value))}]
    [:button {:on-click
              #(when-not (clojure.string/blank? @edit-text)
                 (swap! names conj {:id @next-id :name @edit-text})
                 (swap! next-id inc)
                 (reset! edit-text ""))}
     "Add"]]])
{% endhighlight %}

The table renders all of the names, as well and handles the create case. The edit case is a little more complex and 
requires a function of its own. The `name-row` function manages this complexity for us.

{% highlight clojure %}
(defn name-row [{:keys [id name]}]
  [:tr
   [:td name]
   [:td
    (if (= id @editing-id)
      [:<>
       [:input {:value @edit-text
                :on-change #(reset! edit-text (.. % -target -value))}]
       [:button {:on-click
                 (fn []
                   (swap! names (fn [ns]
                                  (mapv (fn [n]
                                          (if (= (:id n) id)
                                            (assoc n :name @edit-text)
                                            n))
                                        ns)))
                   (reset! editing-id nil))}
        "Save"]]
      [:<>
       [:button {:on-click #(do (reset! editing-id id)
                                (reset! edit-text name))}
        "Edit"]])]
   [:td
    [:button {:on-click
              (fn []
                (swap! names (fn [ns]
                               (vec (remove (fn [n] (= (:id n) id)) ns)))))} ;; FIX
     "Delete"]]])
{% endhighlight %}

## Mounting!

Now we're going to make sure that these functions end up on our web page.

{% highlight clojure %}
(defn mount-root []
  (dom/render [name-table] (.getElementById js/document "app")))

(defn init []
  (enable-console-print!)
  (mount-root))
{% endhighlight %}

We need an `app` element in our HTML page.

{% highlight html %}
<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <title>cljtest</title>
  </head>
  <body>
    <h1>cljtest</h1>

    <!-- This is our new element! -->
    <div id="app"></div>

    <script src="js/out/goog/base.js"></script>
    <script src="js/main.js"></script>
    <script>goog.require('cljtest.core'); cljtest.core.init();</script>

  </body>
</html>
{% endhighlight %}

# Conclusion

This journey started with a humble goal: get a simple ClojureScript app running in the browser. Along the way, I 
tripped over version mismatches, namespace assumptions, and nested anonymous functions — but I also discovered the 
elegance of Reagent and the power of functional UIs in ClojureScript.

While the setup using `lein-cljsbuild` and Reagent 1.0.0 may feel a bit dated, it’s still a solid way to learn the 
fundamentals. From here, I’m looking forward to exploring more advanced tooling like Shadow CLJS, integrating external 
JavaScript libraries, and building more interactive UIs.

This was my first real toe-dip into ClojureScript, and already I’m hooked. Stay tuned — there's more to come.