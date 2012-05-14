#!/bin/bash

scalac -classpath automaton.jar -optimize ../src/main/scala/scala/virtualization/lms/regexp/Lib.scala Out.scala Main.scala
scala -classpath automaton.jar Main
