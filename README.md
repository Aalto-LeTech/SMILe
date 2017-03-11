# The Scala Media Computation Library (SMCL) | Public Interfaces

This project provides the public interfaces, through which external software, such as integrated development environments, can communicate with SMCL.

<br />

## Project Structure

...


## Tools & Dependencies

...


## Repository Content

The repository contains only the files needed to create a fresh development setup of the project: source code and other resources as well as the initial settings for the build tool.


## Making a Local Copy

One possibility to clone this repository onto your local mass storage is to use [Git](https://git-scm.com/) itself. From command line this can be done like below:
```
git clone git@version.aalto.fi:lukkark1/aalto-cs-scala-smcl-public-interfaces.git
```
This will create a `aalto-cs-scala-smcl-public-interfaces` folder into the folder in which it is executed. Other possibilities to clone the repository include e.g. IDE plugins and other graphical user interfaces developed to make using of Git easier.


## Building the Library

This library uses [sbt](http://www.scala-sbt.org/) as its build tool. One way to test and package the library is to issue the following command in its root folder:
```
sbt clean test doc package
```
Build products---including the packaged Java archives (.jar files)---are located under the generated `js/target` and `jvm/target` folders.


## Developing with IntelliJ IDEA

...
