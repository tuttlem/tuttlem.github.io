---
layout: post
title: information_schema with PostgreSQL
date: 2016-01-10
comments: false
categories: [ "postgres", "sql", "information_schema" ]
---

The `information_schema` in PostgreSQL holds a lot of really handy views with information about the current database. Very useful in investigation and discovery scenarios.

In today's post, we'll go through the tables that sit in this schema and how they can help. The `information_schema` documentation can be found [here](http://www.postgresql.org/docs/current/static/information-schema.html) and is what this article has been based on.

## Meta and Context

{% highlight sql %}
-- get the current database name
SELECT * FROM information_schema.information_schema_catalog_name;

-- what are the roles does the current user have that the admin option for?
SELECT * FROM information_schema.administrable_role_authorizations;

-- what roles are applicabl to the current user?
SELECT * FROM information_schema.applicable_roles;

-- attributes on composite data types that the current user has access to
SELECT * FROM information_schema.attributes;

{% endhighlight %}

## Server 

{% highlight sql %}
-- available character sets
SELECT * FROM information_schema.character_sets;

-- list all collations available to this database
SELECT * FROM information_schema.collations;

-- lists the available character sets that apply to the collations
SELECT * FROM information_schema.collation_character_set_applicability;

-- list all of the options defined for foreign-data wrappers
SELECT * FROM information_schema.foreign_data_wrapper_options;

-- list all foreign-data wrappers defined in the database
SELECT * FROM information_schema.foreign_data_wrappers;

-- lists all of the options defined for foreign servers in this database
SELECT * FROM information_schema.foreign_server_options

-- lists all of the standard sql features supported
SELECT * FROM information_schema.sql_features;

-- lists features that are implementation defined
SELECT * FROM information_schema.sql_implementation_info;

-- lists all of the sql languages supported
SELECT * FROM information_schema.sql_languages;

-- lists all of the sql defined feature packages are supported
SELECT * FROM information_schema.sql_packages;

-- lists all of the supported parts of the sql standard
SELECT * FROM information_schema.sql_parts;

-- lists the size limits in the database
SELECT * FROM information_schema.sql_sizing;

-- lists sizing profile information
SELECT * FROM information_schema.sql_sizing_profiles;

-- lists all of the foreign servers defined in this database
SELECT * FROM information_schema.foreign_servers;

-- lists all of the options defined for foreign tables in this database
SELECT * FROM information_schema.foreign_table_options;

-- lists all of the foreign tables 
SELECT * FROM information_schema.foreign_tables;

-- list all settings for user mappings
SELECT * FROM information_schema.user_mapping_options;

-- list all user mappings 
SELECT * FROM information_schema.user_mappings;

{% endhighlight %}

## Catalog

{% highlight sql %}
-- list all check constraints
SELECT * FROM information_schema.check_constraints;

-- lists all of the parameters to functions in the database
SELECT * FROM information_schema.parameters;

-- lists all foreign keys in the database
SELECT * FROM information_schema.referential_constraints;

-- lists all of the functions in the database
SELECT * FROM information_schema.routines;

-- lists all of the sequences
SELECT * FROM information_schema.sequences;

-- lists all constraints from tables in this database
SELECT * FROM information_schema.table_constraints;

-- list all tables
SELECT * FROM information_schema.tables;

-- list all triggers
SELECT * FROM information_schema.triggers;

-- list all composite types
SELECT * FROM information_schema.user_defined_types;

-- lists all views in the database
SELECT * FROM information_schema.views;

-- list all transforms (9.5 ONLY)
SELECT * FROM information_schema.transforms;

{% endhighlight %}

## Security and Privileges

{% highlight sql %}
-- list all columns and their priviledges
SELECT * FROM information_schema.column_privileges;

-- lists all privileges on columns
SELECT * FROM information_schema.role_column_grants;

-- lists all privileges on functions
SELECT * FROM information_schema.role_routine_grants;

-- lists all privileges on tables
SELECT * FROM information_schema.role_table_grants;

-- lists all privileges on udfs
SELECT * FROM information_schema.role_udt_grants;

-- lists all privileges on various objects 
SELECT * FROM information_schema.role_usage_grants;

-- lists all privileges on functions
SELECT * FROM information_schema.routine_privileges;

-- lists all of the table privileges 
SELECT * FROM information_schema.table_privileges;

-- list all udt privileges
SELECT * FROM information_schema.udt_privileges;

-- list privileges on various objects
SELECT * FROM information_schema.usage_privileges;

-- list all data types that the user has access to
SELECT * FROM information_schema.data_type_privileges;

-- list all enabled roles
SELECT * FROM information_schema.enabled_roles;

{% endhighlight %}

## Explore

{% highlight sql %}
-- list all routines that are used by a check constraint
SELECT * FROM information_schema.check_constraint_routine_usage;

-- list columns using a domain defined inside of this database
SELECT * FROM information_schema.column_domain_usage;

-- list all columns that use types owned by the current user
SELECT * FROM information_schema.column_udt_usage;

-- list all columns used by constraints
SELECT * FROM information_schema.constraint_column_usage;

-- list all tables used by constraints
SELECT * FROM information_schema.constraint_table_usage;

-- list all domains based on data types owned by the current user
SELECT * FROM information_schema.domain_udt_usage;

-- lists all columns in the database restricted by primary,unique, foreign or check constraint
SELECT * FROM information_schema.key_column_usage;

-- list all columns that are used in views
SELECT * FROM information_schema.view_column_usage;

-- list all routines that are used in views
SELECT * FROM information_schema.view_routine_usage;

-- lists all tables that are used in views
SELECT * FROM information_schema.view_table_usage;

-- list all of the columns in the database
SELECT * FROM information_schema.columns;

-- list all triggers that specify update columns
SELECT * FROM information_schema.triggered_update_columns;

-- list options for any foreign table columns
SELECT * FROM information_schema.column_options;

-- list all constraints that belong to domains in the current database
SELECT * FROM information_schema.domain_constraints;

-- list all domains defined in the database
SELECT * FROM information_schema.domains

-- list all of the data types inside of array elements
SELECT * FROM information_schema.element_types;

-- lists all of the schemas 
SELECT * FROM information_schema.schemata;

{% endhighlight %}


