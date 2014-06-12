---
layout: post
title: Sending email with GMail and Ruby
date: 2013-01-06
comments: false
---

Here's a quick little ruby snippet to get you sending email through your gmail account. I had to implement this recently, so I thought I'd make note of it here. You will need the [tlsmail](http://rubygems.org/gems/tlsmail) gem installed to make this happen. Here's the code.

{% highlight ruby %}
require 'tlsmail'                                                              
require 'time'                                                                                                  
                                                                               
from = "me@email.com"                                                     
to   = "someone@email.com"                                                     
pwd  = "SECRET PASSWORD"                                                        
                                                                               
content = <<EOF                                                                
From: #{from}                                                                  
To: #{to}                                                                      
MIME-Version: 1.0                                                              
Content-type: text/html                                                        
Subject: An email for you                                                                
Date: #{Time.now.rfc2822}                                                      
                                                                               
<p>Hello to you!</p>                                                                        
EOF                                                                            

# start up a TLS session (required by GMail)
Net::SMTP.enable_tls(OpenSSL::SSL::VERIFY_NONE)

# send the email
Net::SMTP.start('smtp.gmail.com', 587, 'gmail.com', from, pwd, :login) do |smtp|                                                                              
   smtp.send_message(content, from, to)                                        
end                                                                            
{% endhighlight %}