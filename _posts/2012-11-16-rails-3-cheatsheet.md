---
layout: post
title: Rails 3 Cheatsheet
date: 2012-11-16
comments: false
---

A cheatsheet of Rails 3 commands once all of the software infrastructure has been setup and is running.

### Creating and destroying things

{% highlight bash %}
# Create a new application
rails new ApplicationName

# Create a new model
rails generate/g model ModelName

# Create a new controller
rails generate/g controller ControllerName

# Create a new migration
rails generate/g migration MigrationName

# Create a scaffolded controller, model and view 
rails generate/g scaffold ModelName ControllerName

# Destroy a new model
rails destroy model ModelName

# Destroy a new controller
rails destroy controller ControllerName

# Destroy a new migration
rails destroy migration MigrationName
{% endhighlight %}

### Controlling the application

{% highlight bash %}
# Start the rails server
rails server/s

# Install a plugin
rails plugin install PluginName

# Start the rails console
rails console/c

# Start the database console
rails dbconsole/db

# Benchmark/Profile the application
rails performance
{% endhighlight %}

### Manage the database

{% highlight bash %}
# Create the database
rake db:create

# Drop the database  
rake db:drop

# Migrate the database
rake db:migrate

# Drop and recreate database from schema
rake db:reset

# Rollback the latest migration
rake db:rollback

# Create the schema file
rake db:schema:dump

# Create a sessions migration
rake db:sessions:create
{% endhighlight %}

### Utility tasks

{% highlight bash %}
# Build the RDOC html files
rake doc:app

# List all available tasks
rake –tasks

# Truncate log files
rake log:clear

# Clear session, cache and socket files
rake tmp:clear

# Print out all defined application routes
rake routes
{% endhighlight %}

### Testing your application

{% highlight bash %}
# Run all tests
rake test

# Run functional tests
rake test:functionals

# Run integration tests
rake test:integration

# Run unit tests
rake test:units
{% endhighlight %}

### Working with Gems

{% highlight bash %}
# Lock this application's gem set
rake rails:freeze:gems

# List dependent gems
rake gems

# Install all required gems
rake gems:install

# List all installed gems 
gem list

# Install a gem
gem install GemName

# Uninstall a gem
gem uninstall GemName

# Start the gem server
gem server
{% endhighlight %}