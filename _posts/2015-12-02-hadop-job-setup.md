---
layout: post
title: Hadoop job setup
date: 2015-12-02
comments: false
categories: [ "java", "hadoop", "mapreduce", "setup", "maven" ]
---

In today's post, I'm going to refresh the information in some [previous]({% post_url 2014-01-30-create-a-mapreduce-job-using-java-and-maven %}) [articles]({% post_url 2014-01-09-hadoop-2-2-2-0-setup-on-debian %}) that I have written to bring them up to date.

### Job, Mapper and Reducer

It's pretty easy to bundle your job, mapper and reducer together. If they're small enough, it makes sense to do so.

{% highlight java %}
public class MyJobJob 
  extends Configured implements Tool {

  public static class MyMapper 
    extends Mapper<LongWritable, Text, Text, Text> {

    @Override 
    public void setup(Context context) {
      /* setup any configs from the command line */
      this.val = context.getConfiguration().get("some.value");
    }    

    public void map(LongWritable key, Text value, Context context) 
      throws IOException, InterruptedException {      
        /* data selection and source filtering here */
    }
    
  }
  
  public static class MyReducer 
    extends Reducer<Text, Text, NullWritable, Text> {
    
    public void reduce(Text key, Iterable<Text> values, Context context) 
      throws IOException, InterruptedException {
        /* data aggregators here */
    }
    
  }
}
{% endhighlight %}

There isn't much that has changed here. Reading the type annotations can be a little hairy. You can always lookup the documentation for [Mapper](https://hadoop.apache.org/docs/r2.6.2/api/org/apache/hadoop/mapreduce/Mapper.html) and for [Reducer](https://hadoop.apache.org/docs/r2.6.2/api/org/apache/hadoop/mapreduce/Reducer.html). The type definitions are uniform:

{% highlight java %}
class Mapper<KEYIN,VALUEIN,KEYOUT,VALUEOUT>
class Reducer<KEYIN,VALUEIN,KEYOUT,VALUEOUT>
{% endhighlight %}

### ToolRunner

The [ToolRunner](https://hadoop.apache.org/docs/r2.6.2/api/org/apache/hadoop/util/ToolRunner.html) class simplifies the execution management of a MapReduce job using the interface, [Tool](http://hadoop.apache.org/docs/r2.6.2/api/org/apache/hadoop/util/Tool.html). [Tool](http://hadoop.apache.org/docs/r2.6.2/api/org/apache/hadoop/util/Tool.html) is a very simple interface, only providing implementing classes with a contract to `run`. The `run` method looks like this:

{% highlight java %}
public abstract int run(java.lang.String[] args) 
  throws java.lang.Exception;
{% endhighlight %} 

`args` are supplied as usual from the `main` method of the job. A typical implementation of the `run` method will retrieve configuration information, setup the job and execute.

{% highlight java %}
public int run(String[] allArgs) throws Exception {
  
  Job job = Job.getInstance(getConf());

  job.setJarByClass(MyJob.class);

  // basic I/O shape setup 
  job.setInputFormatClass(TextInputFormat.class);
  job.setOutputFormatClass(TextOutputFormat.class);
  job.setOutputKeyClass(NullWritable.class);
  job.setOutputValueClass(Text.class);

  // map, combine, partition, reduce setup 
  job.setMapperClass(MyMapper.class);
  job.setCombinerClass(MyCombiner.class);
  job.setReducerClass(MyReducer.class);
  job.setNumReduceTasks(1);
  
  // parse options passed to the job      
  String[] args = new GenericOptionsParser(
    getConf(), allArgs
  ).getRemainingArgs();
  
  // set the files (from arguments)
  FileInputFormat.setInputPaths(job, new Path(args[0]));
  FileOutputFormat.setOutputPath(job, new Path(args[1]));
  
  // wait for the jobs to finish
  boolean status = job.waitForCompletion(true);
  return status ? 0 : 1;
}

{% endhighlight %}

A special part of the magic here is wrapped up in the [GenericOptionsParser](https://hadoop.apache.org/docs/r1.2.1/api/index.html?org/apache/hadoop/util/GenericOptionsParser.html) which takes in the standard set of command line parameters and plumbs them directly into the job's configuration.

### Finishing up

So there are a couple of features that are provided for you with this wrapper around the run function. Your `main` method ends up very simple:

{% highlight java %}
public static void main(String[] args) throws Exception {
  Configuration conf = new Configuration();
  ToolRunner.run(new MyJob(), args);
}
{% endhighlight %}

Your job is then invoked from the command line using the `hadoop` command:

{% highlight bash %}
$HADOOP_PREFIX/bin/hadoop jar my-jobs-0.0.1-SNAPSHOT.jar \
  org.example.MyMapReduceJob \
  -D arbitrary.config.value=xyz \
  /user/root/input-file.csv \
  /user/root/output-dir
{% endhighlight %}
