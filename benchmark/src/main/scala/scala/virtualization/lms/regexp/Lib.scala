package scala.virtualization.lms.regexp

// careful with @specialized blowup
case class Automaton[@specialized(Boolean,Char,Int) I, @specialized(Boolean,Char,Int) O](out: O, next: I => Automaton[I,O])
