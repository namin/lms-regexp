package scala.virtualization.lms.regexp

case class Automaton[@specialized(Char) I, @specialized(Boolean,Byte) O](out: O, next: I => Automaton[I,O])
