# Probing toolsets for Encore

These probing tools allows the user to gain insight into how many actors and futures are created, how work stealing occurs, succeeds or fails, and how many messages are sent and more, in a running Encore program.

## Table of contents

* [Installation](#installation)
	* [Prerequisites](#prerequisites)
	* [Setup](#setup)
		* [MacOS 10.11+](#macos-1011)
		* [Parser](#parser)
* [Usage](#usage)
	* [Dtrace](#dtrace)
	* [SystemTap](#systemtap)
	* [Parsing](#parsing)
* [Further work](#further-work)
* [Authors](#authors)

## Installation

The toolset works only on MacOS, BSD or Linux systems.

### Prerequisites

* DTrace / SystemTap
* NodeJS (**above version 8**)

### Setup

In order for the probing tools to function, the Encore compiler must first be built with DTrace probes enabled, which can be done with the following command:

```bash
$ make config=release use=dtrace
```

#### MacOS 10.11+

Systems running MacOS 10.11 or above, must first disable  **System Integrity Protection** (SIP) for DTrace to work. This can be done by booting into recovery mode, and running the following commands in the Terminal:

```bash
$ csrutil clear
$ csrutil enable --without dtrace
```

If it still does not work, you may disable SIP entirely, with the command ``csrutil disable`` (but do this at your own risk).

#### Parser

To setup the parser, just run ``npm install`` in the subdirectory ``probing-tools/parser``.

## Usage

Although the two scripts give the same output, depending on whether you have DTrace or SystemTap on your system, the usage will differ somewhat.



### DTrace

To probe a program using DTrace, run the following command:

```
$ dtrace -o ../output.xml -Cs dtrace.d -c ./[target binary] XML
```

This assumes that the working directory is ``probing-tools/dtrace``, and will produce an XML-file in the parent directory with the output from the DTrace script.

**Note:** If you want the output to be in a human readable format, you can ommit the ``-o`` flag, and replace the ``XML`` argument with an arbitrary argument (you can not leave it empty, though).

### SystemTap

To probe the program using SystemTap, run the following command:

```
$ stap systemTapXML.stp -c [target binary] -o ../output.XML
```

This assumes that the working directory is ``probing-tools/systemtap``, and will produce an XML-file in the parent directory with the output from the SystemTap script.

**Note:** If you want the output to be in a human readable format, you may use the script ``systemTap.stp`` instead.

### Parsing

A simple parser is available to parse the XML output produced by the DTrace / SystemTap scripts.

To parse the XML output, simply run the following command:

```
node parser/index.js /path/to/output.xml
```

This assumes that the working directory ``probing-tools``, and will give an organized overview of the data gathered:

```bash
┌───────────────────────┬────────┐
│        (index)        │ Values │
├───────────────────────┼────────┤
│     core-switches     │   2    │
│  work-steal-failure   │   2    │
│ work-steal-successful │   3    │
│  work-steal-attempt   │   5    │
│    actor-msg-send     │   27   │
└───────────────────────┴────────┘
--------------------------- ACTORS ---------------------------
┌────────────┬──────────────┬──────────────┬──────────────────────┬─────────────────────┐
│  (index)   │      id      │ numberOfGets │ numberOfTimesBlocked │ numberOfTimesStolen │
├────────────┼──────────────┼──────────────┼──────────────────────┼─────────────────────┤
│ 4430339584 │ '4430339584' │      0       │          0           │          1          │
│ 4933481472 │ '4933481472' │      0       │          0           │          2          │
└────────────┴──────────────┴──────────────┴──────────────────────┴─────────────────────┘
--------------------------- Schedulers ---------------------------
┌────────────┬──────────────┬──────────────────┬──────────────┐
│  (index)   │      id      │ successfulSteals │ failedSteals │
├────────────┼──────────────┼──────────────────┼──────────────┤
│ 4430338432 │ '4430338432' │        1         │      1       │
│ 4430338048 │ '4430338048' │        2         │      1       │
└────────────┴──────────────┴──────────────────┴──────────────┘
--------------------------- Work steal success ---------------------------
┌─────────┬──────────────┬──────────────┬───────┐
│ (index) │  scheduler   │    victim    │ count │
├─────────┼──────────────┼──────────────┼───────┤
│    0    │ '4430338432' │ '4430338048' │   1   │
│    1    │ '4430338048' │ '4430338432' │   2   │
└─────────┴──────────────┴──────────────┴───────┘
--------------------------- Work steal failure ---------------------------
┌─────────┬──────────────┬──────────────┬───────┐
│ (index) │  scheduler   │    victim    │ count │
├─────────┼──────────────┼──────────────┼───────┤
│    0    │ '4430338048' │     '0'      │   1   │
│    1    │ '4430338432' │ '4430338048' │   1   │
└─────────┴──────────────┴──────────────┴───────┘
```

## Further work

While the parser allows for an automated analysis of an Encore program, currently there is no script that does anything fancy with the data.

The data will be collected and accessible through a few properties of the parser, as seen below:

```js
// index.js
const parser = new Parser(nodes);
parser.start(); // Start parsing

const counts       = parser.counts; // Holds a bunch of enumerations
const futures      = parser.futures; // Holds info on all futures created
const futureGets   = parser.futureGets; // Holds info on all future gets
const futureBlocks = parser.blocks; // Holds info on all blocks made
const actors       = parser.actors; // Holds info on all actors
const schedulers   = parser.schedulers; // Holds info on all schedulers
const workStealSuccess = parser.workStealSuccess; // Holds info on all successful work steals
const workStealFailure = parser.workStealFailure; // Holds info on all failed work steals
```

For example, to get the ratio between number of messages sent and number of futures created, you can do the following:

```js
const counts = parser.counts;
const ratio  = parser.counts['actor-msg-send'] / parser.counts['future-create'];

console.log(ratio);
```


## Authors

The toolset was created by [Ardalan Samimi](https://github.com/pkrll), [Ulf Sigvardsson](https://github.com/ulfsigvardsson) and [Joy van den Eijkhof](https://github.com/elieoaks).
