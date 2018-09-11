---
layout: post
title: Binary dependencies with AWS Lambda
date: 2018-09-11
comments: false
categories: [ "python", "aws", "lambda" ]
---

When you're developing an [AWS Lambda](https://aws.amazon.com/lambda/), sometimes you're going to need to install binary package dependencies. Today's article will take you through the construction of a project that can be deployed into [AWS Lambda](https://aws.amazon.com/lambda/) including your binary dependencies.

### Structure

The whole idea here is based on [AWS Lambda](https://aws.amazon.com/lambda/) using [Docker](https://www.docker.com/) to facilite package, deployment, and execution of your function. The standard `python:3.6` image available in the standard library is compatible with what we'll end up deploying.

The structure of your project should have a `requirements.txt` file holding your dependencies, a standard `Dockerfile` and of course, your code.

{% highlight text %}
.
├── Dockerfile
├── requirements.txt
└── src
    └── __init__.py
{% endhighlight %}

Any depeendencies are listed out by the `requirements.txt` file.

### Docker

We can now bundle our application up, so that it can be used by [AWS Lambda](https://aws.amazon.com/lambda/).

{% highlight text %}
FROM python:3.6
RUN apt-get update && apt-get install -y zip
WORKDIR /lambda

# add the requirements and perform any installations
ADD requirements.txt /tmp
RUN pip install --quiet -t /lambda -r /tmp/requirements.txt && \
    find /lambda -type d | xargs chmod ugo+rx && \
    find /lambda -type f | xargs chmod ugo+r

# the application source code is added to the container
ADD src/ /lambda/
RUN find /lambda -type d | xargs chmod ugo+rx && \
    find /lambda -type f | xargs chmod ugo+r

# pre-compilation into the container
RUN python -m compileall -q /lambda

RUN zip --quiet -9r /lambda.zip .

FROM scratch
COPY --from=0 /lambda.zip /
{% endhighlight %}

The docker container is then built with the following:

{% highlight bash %}
docker build -t my-lambda .
ID=$(docker create my-lambda /bin/true)
docker cp $ID:/ .
{% endhighlight %}

The retrieves the `zip` file that we built through the process, that's readily deployable to [AWS Lambda](https://aws.amazon.com/lambda/).

