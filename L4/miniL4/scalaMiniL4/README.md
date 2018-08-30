*This is very early work. If you don't work for Legalese, you should probably ignore it.*

Ping Dustin on Legalese Slack if you want to install on Windows, and he'll write the documentation to do so with your help.

## Installing (on Unix systems)

First clone this repo, because Scala gets installed inside the project directory. 

Then install Scala, preferably via the IntelliJ IDE option here: https://www.scala-lang.org/download/. 

## Building and Running

From the root (L4/miniL4/scalaMiniL4):
> sbt

After sbt loads:
> ~compile

The ~ starts indicates to watch for changes.

To run the tests:
> ~test

To build and run scala native (which won't actually do anything useful, but it's nice to see that it works):

> run

