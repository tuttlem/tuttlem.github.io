---
layout: post
title: Targetting the RISC-V Core of the RP2350
date: 2025-07-28
comments: false
categories: [ rust, risc-v, rp2350]
---

# Introduction

In our [previous post]({% post_url 2025-07-22-getting-started-with-the-rp2350 %}), we got a basic "blinky" app running 
on the [Arm Cortex-M33](https://developer.arm.com/Processors/Cortex-M33) side of the RP2350 using Embassy and `embassy-rp`. This time, we're reworking the same 
application to target the RP2350's **RISC-V** core instead—highlighting how to boot the 
[RISC-V Hazard 3](https://www.raspberrypi.com/news/risc-v-on-raspberry-pi-pico-2/) with Rust and control peripherals 
using the `rp-hal` ecosystem.

This post walks through the key differences and required changes to adapt the project.

Most of this code is available in the examples section of the [rp-hal](https://github.com/rp-rs/rp-hal) repository.

# What is RISC-V?

[RISC-V](https://riscv.org/) (pronounced "risk-five") is an open standard instruction set architecture (ISA) that 
emerged from the University of California, Berkeley in 2010. Unlike proprietary ISAs such as x86 or Arm, RISC-V is 
open and extensible—allowing anyone to design, implement, and manufacture RISC-V chips without licensing fees.

This openness has led to rapid adoption across academia, startups, and even large chipmakers. RISC-V cores can now be 
found in everything from tiny embedded microcontrollers to Linux-capable SoCs and even experimental high-performance CPUs.

In the RP2350, RISC-V comes in the form of the Hazard3 core—a lightweight, open-source 3-stage RV32IMAC processor 
developed by Raspberry Pi. It sits alongside the more familiar Arm Cortex-M33, making the RP2350 one of the first 
widely accessible dual-ISA microcontrollers.

For embedded developers used to the Arm world, RISC-V introduces a slightly different toolchain and runtime, but the 
basic concepts—GPIO control, clock configuration, memory mapping—remain very familiar.

In this post, we explore how to bring up a basic RISC-V application targeting the RP2350 Hazard3 core using Rust.

## Switching to RISC-V: Overview

The RP2350's second core is a Hazard3 RISC-V processor. To target it:

- We switch toolchains from `thumbv8m.main-none-eabihf` to `riscv32imac-unknown-none-elf`
- We drop the Embassy stack and use the `rp235x-hal` directly
- We write or reuse suitable linker scripts and memory definitions
- We adjust runtime startup, including clock and GPIO initialization

## `.cargo/config.toml` Changes

We swap the build target and customize linker flags:

{% highlight toml %}
[build]
target = "riscv32imac-unknown-none-elf"

[target.riscv32imac-unknown-none-elf]
rustflags = [
    "-C", "link-arg=--nmagic",
    "-C", "link-arg=-Trp235x_riscv.x",
    "-C", "link-arg=-Tdefmt.x",
]
runner = "sudo picotool load -u -v -x -t elf"
{% endhighlight %}

Note how we invert the typical linker script behavior: `rp235x_riscv.x` now includes `link.x` instead of the other way 
around.

The Rust target `riscv32imac-unknown-none-elf` tells the compiler to generate code for a 32-bit RISC-V architecture 
(riscv32) that supports the *I* (integer), *M* (multiply/divide), *A* (atomic), and *C* (compressed) instruction set 
extensions. 

The `unknown-none-elf` part indicates a bare-metal environment with no OS (none) and output in the standard ELF binary 
format. This target is a common choice for embedded RISC-V development.

## Updating the `Cargo.toml`

Out goes Embassy, in comes `rp235x-hal`:

{% highlight toml %}
[dependencies]
embedded-hal = "1.0.0"
rp235x-hal = { git = "https://github.com/rp-rs/rp-hal", version = "0.3.0", features = [
    "binary-info",
    "critical-section-impl",
    "rt",
    "defmt",
] }
panic-halt = "1.0.0"
rp-binary-info = "0.1.0"
{% endhighlight %}

## Main Application Rewrite

The runtime is simpler—no executor or async. We explicitly set up clocks, GPIO, and enter a polling loop.

{% highlight rust %}
#[hal::entry]
fn main() -> ! {
    let mut pac = hal::pac::Peripherals::take().unwrap();
    let mut watchdog = hal::Watchdog::new(pac.WATCHDOG);
    let clocks = hal::clocks::init_clocks_and_plls(...).unwrap();
    let mut timer = hal::Timer::new_timer0(pac.TIMER0, ...);
    let pins = hal::gpio::Pins::new(...);
    let mut led = pins.gpio25.into_push_pull_output();

    loop {
        led.set_high().unwrap();
        timer.delay_ms(500);
        led.set_low().unwrap();
        timer.delay_ms(500);
    }
}
{% endhighlight %}

## Linker and Memory Layout

We swapped in a dedicated `rp235x_riscv.x` linker script to reflect RISC-V memory layout. This script takes care of 
startup alignment, section placement, and stack/heap boundaries.

The `build.rs` file was also extended to emit both `memory.x` and `rp235x_riscv.x` so that tooling remains consistent 
across platforms.

## Observations and Gotchas

- **Clock setup** is still necessary, even though the RISC-V HAL avoids some of the abstractions of Embassy.
- **Runtime** and exception handling differ between Arm and RISC-V: for example, default handlers like `DefaultInterruptHandler` and `DefaultExceptionHandler` must be provided.
- The **boot block** and `.bi_entries` sections are still necessary for picotool metadata.

# Conclusion

Today's article was only a brief follow up on the first article. All of these changes are available in a [risc-v branch](https://github.com/tuttlem/rp2350_blink/tree/risc-v) 
that I've added to the original repository.
