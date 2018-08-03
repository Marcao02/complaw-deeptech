*This is very early work. If you don't work for Legalese, _ignore it_.*

Ping Dustin on Legalese Slack if you want to install on Windows, and he'll write the documentation to do so with your help.

## Installing (on Unix systems)

First clone this repo, because Scala gets installed inside the project directory. 

Then install Scala, preferably via the IntelliJ IDE option here: https://www.scala-lang.org/download/. 

Then install Ammonite-REPL (installs to `/usr/local/bin`) for Scala 2.11 (until Scala-native supports Scala 2.12, which the developer(s?) intend by the end of Aug 2018). 

> sudo sh -c '(echo "#!/usr/bin/env sh" && curl -L https://github.com/lihaoyi/Ammonite/releases/download/1.1.2/2.11-1.1.2) > /usr/local/bin/amm && chmod +x /usr/local/bin/amm' && amm



## Runnning



## Building

From the root (L4/miniL4/scalaMiniL4):
> sbt

After sbt loads:

- To build and run scala native:
> ~run
- I don't know how to do the normal JVM build yet.
   

