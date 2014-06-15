---
layout: post
title: ConfigFile Basics in Haskell
date: 2013-07-04
comments: false
categories: [ "Haskell", "Programming", "ConfigFile" ]
---

### Introduction

One of the first things that I reach for when writing an application that will be used outside the context of my development sandbox is a configuration library. Not having statically compiled values for variables is quite a valuable position to be in once your application has been deployed. Today's post will take you from 0 to up and running with a package called [ConfigFile](http://hackage.haskell.org/packages/archive/ConfigFile/1.0.5/doc/html/Data-ConfigFile.html) that is specifically designed to serve your applications with configuration data. Most of the content in this post is lifted directly from the documentation, so you'll be better off reading through those to gain a deeper understanding of the library. This post is more of a short-cut to get up and running.

### Configuration format

If you've had much experience with administering windows back in the day when INI files ruled the earth, you'll be right at home with [ConfigFile](http://hackage.haskell.org/packages/archive/ConfigFile/1.0.5/doc/html/Data-ConfigFile.html). For those of you who have never seen it before, it's really easy and you can read up on it [here](http://en.wikipedia.org/wiki/INI_file). Files are broken into sections which contain a list of key/value pairs - done!

For today's example, the configuration file will look as follows:

{% highlight ini %}
[Location]
path=/tmp
filename=blah
{% endhighlight %}

We'll end up with two keys, `path` and `filename` that have corresponding values in the `Location` section.

### Structure

The thing I like most about the part of the application is that we can make a nice record based data structure in Haskell that will marry up to how our configuration file looks. The simple file that we've defined above, would look like this:

{% highlight haskell %}
data ConfigInfo = ConfigInfo { path :: String
                             , fileName :: String
                             }
{% endhighlight %}

Once we fill one of these up, you can see that it'll be pretty natural to access these details.

### Reading and Building

Finally - we need to read the values out of the config file and get them into our structure. The following block of code will do that for us.

{% highlight haskell %}
readConfig :: String -> IO ConfigInfo
readConfig f = do
   rv <- runErrorT $ do

      -- open the configuration file
      cp <- join $ liftIO $ readfile emptyCP f
      let x = cp

      -- read out the attributes
      pv <- get x "Location" "path"
      fv <- get x "Location" "filename"

      -- build the config value
      return (ConfigInfo { path = pv
                         , fileName = fv
                         })

   -- in the instance that configuration reading failed we'll
   -- fail the application here, otherwise send out the config
   -- value that we've built
   either (\x -> error (snd x)) (\x -> return x) rv
{% endhighlight %}

There's a few interesting points in here to note. The Error Monad is being used here to keep track of any failures during the config read process. `runErrorT` kicks this off for us. We then use `readfile` to open the config file with a sane parser that knows how to speak INI. Pulling the actual strings from the config is done by using `get`. From here, it's just wrapping the values up ready to send out. The final call is to `either`. Leaving the Error Monad, we're given an `Either` (left being the error, right being the value). I've used `either` here so I can provide an implementation for either scenario. If an error occurs (the first lambda) then I just toast-out of the application. If we get a config value back (the second lambda), that's what gets returned.

### Conclusion

That's all there is to that. Remember, you won't escape from the `IO` Monad which is why the read function's return type has IO. When you want to use these values, it'll need to be within `do` constructs:

{% highlight haskell %}
main :: IO ()
main = do
   config <- readConfig "test.cfg"
   putStrLn $ "The path value is: " ++ (path config)
   putStrLn $ "The filename value is: " ++ (fileName config)
{% endhighlight %}

Cheers.

