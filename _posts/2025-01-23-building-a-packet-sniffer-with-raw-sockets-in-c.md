---
layout: post
title: Building a Packet Sniffer with Raw Sockets in C
date: 2025-01-23
comments: false
categories: [ "c", "sockets", "network" ]
---

# Introduction

Network packet sniffing is an essential skill in the toolbox of any systems programmer or network engineer. It enables 
us to inspect network traffic, debug communication issues, and even learn how various networking protocols function 
under the hood. 

In this article, we will walk through the process of building a simple network packet sniffer in C using raw sockets.

Before we begin, it might help to run through a quick networking primer.

## OSI and Networking Layers

Before diving into the code, let's briefly revisit the OSI model—a conceptual framework that standardizes network 
communication into seven distinct layers:

1. **Physical Layer**: Deals with the physical connection and transmission of raw data bits.
2. **Data Link Layer**: Responsible for framing and MAC addressing. Ethernet operates at this layer.
3. **Network Layer**: Handles logical addressing (IP addresses) and routing. This layer is where IP packets are structured.
4. **Transport Layer**: Ensures reliable data transfer with protocols like TCP and UDP.
5. **Session Layer**: Manages sessions between applications.
6. **Presentation Layer**: Transforms data formats (e.g., encryption, compression).
7. **Application Layer**: Interfaces directly with the user (e.g., HTTP, FTP).

Our packet sniffer focuses on Layers 2 through 4. By analyzing Ethernet, IP, TCP, UDP, and ICMP headers, we gain 
insights into packet structure and how data travels across a network.

# The Code

In this section, we'll run through the functions that are needed to implement our packet sniffer. The layers that we'll 
focus on are:

* **Layer 2 (Data Link)**: Capturing raw Ethernet frames and extracting MAC addresses.
* **Layer 3 (Network)**: Parsing IP headers for source and destination IPs.
* **Layer 4 (Transport)**: Inspecting TCP, UDP, and ICMP protocols to understand port-level communication and message types.

## Layer 2 (Data Link)

The Data Link Layer is responsible for the physical addressing of devices on a network. It includes the Ethernet 
header, which contains the source and destination MAC addresses. In this section, we analyze and print the Ethernet 
header.

{% highlight c %}
void print_eth_header(unsigned char *buffer, int size) { 
    struct ethhdr *eth = (struct ethhdr *)buffer;

    printf("\nEthernet Header\n");
    printf("   |-Source Address      : %.2X-%.2X-%.2X-%.2X-%.2X-%.2X \n",
           eth->h_source[0], eth->h_source[1], eth->h_source[2], eth->h_source[3], eth->h_source[4], eth->h_source[5]);
    printf("   |-Destination Address : %.2X-%.2X-%.2X-%.2X-%.2X-%.2X \n",
           eth->h_dest[0], eth->h_dest[1], eth->h_dest[2], eth->h_dest[3], eth->h_dest[4], eth->h_dest[5]);
    printf("   |-Protocol            : %u \n", (unsigned short)eth->h_proto);
}
{% endhighlight %}

## Layer 3 (Network)

The Network Layer handles logical addressing and routing. In our code, this corresponds to the IP header, where we 
extract source and destination IP addresses.

{% highlight c %}
void print_ip_header(unsigned char *buffer, int size) { 
    struct iphdr *ip = (struct iphdr *)(buffer + sizeof(struct ethhdr));

    printf("\nIP Header\n");
    printf("   |-Source IP        : %s\n", inet_ntoa(*(struct in_addr *)&ip->saddr));
    printf("   |-Destination IP   : %s\n", inet_ntoa(*(struct in_addr *)&ip->daddr));
    printf("   |-Protocol         : %d\n", ip->protocol);
}
{% endhighlight %}

Here, we use the `iphdr` structure to parse the IP header. The `inet_ntoa` function converts the source and destination 
IP addresses from binary format to a human-readable string.

## Layer 4 (Transport)

The Transport Layer ensures reliable data transfer and includes protocols like [TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol), 
[UDP](https://en.wikipedia.org/wiki/User_Datagram_Protocol), and [ICMP](https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol). 
We have specific functions to parse and display these packets:

The [TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol) version of this function has a source and destination for the packet, but also has a sequence and 
acknowledgement which are key features for this protocol.

{% highlight c %}
void print_tcp_packet(unsigned char *buffer, int size) {
    struct iphdr *ip = (struct iphdr *)(buffer + sizeof(struct ethhdr));
    struct tcphdr *tcp = (struct tcphdr *)(buffer + sizeof(struct ethhdr) + ip->ihl * 4);

    printf("\nTCP Packet\n");
    print_ip_header(buffer, size);
    printf("\n   |-Source Port      : %u\n", ntohs(tcp->source));
    printf("   |-Destination Port : %u\n", ntohs(tcp->dest));
    printf("   |-Sequence Number  : %u\n", ntohl(tcp->seq));
    printf("   |-Acknowledgement  : %u\n", ntohl(tcp->ack_seq));
}
{% endhighlight %}

The [UDP](https://en.wikipedia.org/wiki/User_Datagram_Protocol) counterpart doesn't have the sequencing or acknowledgement as it's a general broadcast protocol.

{% highlight c %}
void print_udp_packet(unsigned char *buffer, int size) {
    struct iphdr *ip = (struct iphdr *)(buffer + sizeof(struct ethhdr));
    struct udphdr *udp = (struct udphdr *)(buffer + sizeof(struct ethhdr) + ip->ihl * 4);

    printf("\nUDP Packet\n");
    print_ip_header(buffer, size);
    printf("\n   |-Source Port      : %u\n", ntohs(udp->source));
    printf("   |-Destination Port : %u\n", ntohs(udp->dest));
    printf("   |-Length           : %u\n", ntohs(udp->len));
}
{% endhighlight %}

[ICMP](https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol)'s `type`, `code`, and `checksum` are used in the verification process of this protocol.

{% highlight c %}
void print_icmp_packet(unsigned char *buffer, int size) {
    struct iphdr *ip = (struct iphdr *)(buffer + sizeof(struct ethhdr));
    struct icmphdr *icmp = (struct icmphdr *)(buffer + sizeof(struct ethhdr) + ip->ihl * 4);

    printf("\nICMP Packet\n");
    print_ip_header(buffer, size);
    printf("\n   |-Type : %d\n", icmp->type);
    printf("   |-Code : %d\n", icmp->code);
    printf("   |-Checksum : %d\n", ntohs(icmp->checksum));
}
{% endhighlight %}

## Tying it all together

The architecture of this code is fairly simple. The `main` function sets up a loop which will continually receive raw 
information from the socket. From there, a determination is made about what level the information is at. Using this 
information we'll call/dispatch to a function that specialises in that layer.

{% highlight c %}
int main() {
    int sock_raw;
    struct sockaddr saddr;
    socklen_t saddr_len = sizeof(saddr);

    unsigned char *buffer = (unsigned char *)malloc(BUFFER_SIZE);
    if (buffer == NULL) {
        perror("Failed to allocate memory");
        return 1;
    }

    sock_raw = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
    if (sock_raw < 0) {
        perror("Socket Error");
        free(buffer);
        return 1;
    }

    printf("Starting packet sniffer...\n");

    while (1) {
        int data_size = recvfrom(sock_raw, buffer, BUFFER_SIZE, 0, &saddr, &saddr_len);
        if (data_size < 0) {
            perror("Failed to receive packets");
            break;
        }
        process_packet(buffer, data_size);
    }

    close(sock_raw);
    free(buffer);
    return 0;
}
{% endhighlight %}

The `recvfrom` receives the raw bytes in from the socket.

The `process_packet` function is responsible for the dispatch of the information. This is really a `switch` statement 
focused on the incoming protocol:

{% highlight c %}
void process_packet(unsigned char *buffer, int size) {
    struct iphdr *ip_header = (struct iphdr *)(buffer + sizeof(struct ethhdr));

    switch (ip_header->protocol) {
        case IPPROTO_TCP:
            print_tcp_packet(buffer, size);
            break;
        case IPPROTO_UDP:
            print_udp_packet(buffer, size);
            break;
        case IPPROTO_ICMP:
            print_icmp_packet(buffer, size);
            break;
        default:
            print_ip_header(buffer, size);
            break;
    }
}
{% endhighlight %}

This then ties all of our functions in together.

# Running

Because of the nature of the information that this application will pull from your system, you will need to run this as 
root. You need that low-level access to your networking stack.

{% highlight shell %}
sudo ./psniff
{% endhighlight %}


# Conclusion

Building a network packet sniffer using raw sockets in C offers valuable insight into how data flows through the 
network stack and how different protocols interact. By breaking down packets layer by layer—from the Data Link Layer 
(Ethernet) to the Transport Layer (TCP, UDP, ICMP)—we gain a deeper understanding of networking concepts and 
system-level programming.

This project demonstrates key topics such as:

* Capturing raw packets using sockets.
* Parsing headers to extract meaningful information.
* Mapping functionality to specific OSI layers.

Packet sniffers like this are not only useful for learning but also serve as foundational tools for network 
diagnostics, debugging, and security monitoring. However, it’s essential to use such tools ethically and responsibly, 
adhering to legal and organizational guidelines.

In the future, we could extend this sniffer by writing packet payloads to a file, adding packet filtering (e.g., only 
capturing HTTP or DNS traffic), or even integrating with libraries like libpcap for more advanced use cases.

A [full gist](https://gist.github.com/tuttlem/83db927330af4a1ca1bf2ed0c448c535) of this code is available to check out.
