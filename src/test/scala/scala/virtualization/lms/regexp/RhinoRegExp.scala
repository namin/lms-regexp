package scala.virtualization.lms.regexp

/* -*- Mode: java; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import java.io.Serializable;

class Context

object Kit {
  /**
   * If character <tt>c</tt> is a hexadecimal digit, return
   * <tt>accumulator</tt> * 16 plus corresponding
   * number. Otherise return -1.
   */
  def xDigitToInt(c: Int, accumulator: Int): Int = {
      // Use 0..9 < A..Z < a..z
      if (c <= '9') {
          val d = c - '0';
          if (0 <= d) { 
            (accumulator << 4) | d;
          }
      } else if (c <= 'F') {
          if ('A' <= c) {
              val d = c - ('A' - 10);
              (accumulator << 4) | d;
          }
      } else if (c <= 'f') {
          if ('a' <= c) {
              val d = c - ('a' - 10);
              (accumulator << 4) | d;
          }
      }
      return -1;
  }

}

object ScriptRuntime {
  def isJSLineTerminator(c: Int): Boolean = {
    // Optimization for faster check for eol character:
    // they do not have 0xDFD0 bits set
    if ((c & 0xDFD0) != 0) {
        return false;
    }
    return c == '\n' || c == '\r' || c == 0x2028 || c == 0x2029;
  }

  def isStrWhiteSpaceChar(c: Int): Boolean = c match {
  		case ' ' // <SP>
  		| '\n' // <LF>
  		| '\r' // <CR>
  		| '\t' // <TAB>
  		| '\u00A0' // <NBSP>
  		| '\u000C' // <FF>
  		| '\u000B' // <VT>
  		| '\u2028' // <LS>
  		| '\u2029' // <PS>
      | '\uFEFF' => // <BOM>
  			true;
  		case _ =>
  			Character.getType(c) == Character.SPACE_SEPARATOR;
  }

  def isJSWhitespaceOrLineTerminator(c: Int) = {
    (isStrWhiteSpaceChar(c) || isJSLineTerminator(c));
  }
}


/**
 * This class implements the RegExp native object.
 *
 * Revision History:
 * Implementation in C by Brendan Eich
 * Initial port to Java by Norris Boyd from jsregexp.c version 1.36
 * Merged up to version 1.38, which included Unicode support.
 * Merged bug fixes in version 1.39.
 * Merged JSFUN13_BRANCH changes up to 1.32.2.13
 *
 * @author Brendan Eich
 * @author Norris Boyd
 */



object Rhino {
    def break = assert(false) //TR FIXME
    def continue = assert(false) //TR FIXME
  
  
  

    final val JSREG_GLOB = 0x1;       // 'g' flag: global
    final val JSREG_FOLD = 0x2;       // 'i' flag: fold
    final val JSREG_MULTILINE = 0x4;  // 'm' flag: multiline

    //type of match to perform
    final val TEST = 0;
    final val MATCH = 1;
    final val PREFIX = 2;

    final var debug = false;

    final val REOP_SIMPLE_START  : Byte = 1;  /* start of 'simple opcodes' */
    final val REOP_EMPTY         : Byte = 1;  /* match rest of input against rest of r.e. */
    final val REOP_BOL           : Byte = 2;  /* beginning of input (or line if multiline) */
    final val REOP_EOL           : Byte = 3;  /* end of input (or line if multiline) */
    final val REOP_WBDRY         : Byte = 4;  /* match "" at word boundary */
    final val REOP_WNONBDRY      : Byte = 5;  /* match "" at word non-boundary */
    final val REOP_DOT           : Byte = 6;  /* stands for any character */
    final val REOP_DIGIT         : Byte = 7;  /* match a digit char: [0-9] */
    final val REOP_NONDIGIT      : Byte = 8;  /* match a non-digit char: [^0-9] */
    final val REOP_ALNUM         : Byte = 9;  /* match an alphanumeric char: [0-9a-z_A-Z] */
    final val REOP_NONALNUM      : Byte = 10; /* match a non-alphanumeric char: [^0-9a-z_A-Z] */
    final val REOP_SPACE         : Byte = 11; /* match a whitespace char */
    final val REOP_NONSPACE      : Byte = 12; /* match a non-whitespace char */
    final val REOP_BACKREF       : Byte = 13; /* back-reference (e.g., \1) to a parenthetical */
    final val REOP_FLAT          : Byte = 14; /* match a flat string */
    final val REOP_FLAT1         : Byte = 15; /* match a single char */
    final val REOP_FLATi         : Byte = 16; /* case-independent REOP_FLAT */
    final val REOP_FLAT1i        : Byte = 17; /* case-independent REOP_FLAT1 */
    final val REOP_UCFLAT1       : Byte = 18; /* single Unicode char */
    final val REOP_UCFLAT1i      : Byte = 19; /* case-independent REOP_UCFLAT1 */
//    private static final byte REOP_UCFLAT        = 20; /* flat Unicode string; len immediate counts chars */
//    private static final byte REOP_UCFLATi       = 21; /* case-independent REOP_UCFLAT */
    final val REOP_CLASS         : Byte = 22; /* character class with index */
    final val REOP_NCLASS        : Byte = 23; /* negated character class with index */
    final val REOP_SIMPLE_END    : Byte = 23; /* end of 'simple opcodes' */
    final val REOP_QUANT         : Byte = 25; /* quantified atom: atom{1,2} */
    final val REOP_STAR          : Byte = 26; /* zero or more occurrences of kid */
    final val REOP_PLUS          : Byte = 27; /* one or more occurrences of kid */
    final val REOP_OPT           : Byte = 28; /* optional subexpression in kid */
    final val REOP_LPAREN        : Byte = 29; /* left paren bytecode: kid is u.num'th sub-regexp */
    final val REOP_RPAREN        : Byte = 30; /* right paren bytecode */
    final val REOP_ALT           : Byte = 31; /* alternative subexpressions in kid and next */
    final val REOP_JUMP          : Byte = 32; /* for deoptimized closure loops */
//    private static final byte REOP_DOTSTAR       = 33; /* optimize .* to use a single opcode */
//    private static final byte REOP_ANCHOR        = 34; /* like .* but skips left context to unanchored r.e. */
//    private static final byte REOP_EOLONLY       = 35; /* $ not preceded by any pattern */
//    private static final byte REOP_BACKREFi      = 37; /* case-independent REOP_BACKREF */
//    private static final byte REOP_LPARENNON     = 40; /* non-capturing version of REOP_LPAREN */
    final val REOP_ASSERT        : Byte = 41; /* zero width positive lookahead assertion */
    final val REOP_ASSERT_NOT    : Byte = 42; /* zero width negative lookahead assertion */
    final val REOP_ASSERTTEST    : Byte = 43; /* sentinel at end of assertion child */
    final val REOP_ASSERTNOTTEST : Byte = 44; /* sentinel at end of !assertion child */
    final val REOP_MINIMALSTAR   : Byte = 45; /* non-greedy version of * */
    final val REOP_MINIMALPLUS   : Byte = 46; /* non-greedy version of + */
    final val REOP_MINIMALOPT    : Byte = 47; /* non-greedy version of ? */
    final val REOP_MINIMALQUANT  : Byte = 48; /* non-greedy version of {} */
    final val REOP_ENDCHILD      : Byte = 49; /* sentinel at end of quantifier child */
    final val REOP_REPEAT        : Byte = 51; /* directs execution of greedy quantifier */
    final val REOP_MINIMALREPEAT : Byte = 52; /* directs execution of non-greedy quantifier */
    final val REOP_ALTPREREQ     : Byte = 53; /* prerequisite for ALT, either of two chars */
    final val REOP_ALTPREREQi    : Byte = 54; /* case-independent REOP_ALTPREREQ */
    final val REOP_ALTPREREQ2    : Byte = 55; /* prerequisite for ALT, a char or a class */
//    private static final byte R: Byte EOP_ENDALT        = 56; /* end of final alternate */
    final val REOP_END           : Byte = 57;

    final val ANCHOR_BOL         : Byte = -2;

    def op2string(op: Byte) = op match {

          //case 1  => "REOP_SIMPLE_START   /* start of 'simple opcodes' */                    "
          case 1  => "REOP_EMPTY          /* match rest of input against rest of r.e. */     "
          case 2  => "REOP_BOL            /* beginning of input (or line if multiline) */    "
          case 3  => "REOP_EOL            /* end of input (or line if multiline) */          "
          case 4  => "REOP_WBDRY          /* match \"\" at word boundary */                    "
          case 5  => "REOP_WNONBDRY       /* match \"\" at word non-boundary */                "
          case 6  => "REOP_DOT            /* stands for any character */                     "
          case 7  => "REOP_DIGIT          /* match a digit char: [0-9] */                    "
          case 8  => "REOP_NONDIGIT       /* match a non-digit char: [^0-9] */               "
          case 9  => "REOP_ALNUM          /* match an alphanumeric char: [0-9a-z_A-Z] */     "
          case 10 => "REOP_NONALNUM       /* match a non-alphanumeric char: [^0-9a-z_A-Z] */ "
          case 11 => "REOP_SPACE          /* match a whitespace char */                      "
          case 12 => "REOP_NONSPACE       /* match a non-whitespace char */                  "
          case 13 => "REOP_BACKREF        /* back-reference (e.g., \1) to a parenthetical */ "
          case 14 => "REOP_FLAT           /* match a flat string */                          "
          case 15 => "REOP_FLAT1          /* match a single char */                          "
          case 16 => "REOP_FLATi          /* case-independent REOP_FLAT */                   "
          case 17 => "REOP_FLAT1i         /* case-independent REOP_FLAT1 */                  "
          case 18 => "REOP_UCFLAT1        /* single Unicode char */                          "
          case 19 => "REOP_UCFLAT1i       /* case-independent REOP_UCFLAT1 */                "
      //    private static final byte REOP_UCFLAT        = 20; /* flat Unicode string; len immediate counts chars */
      //    private static final byte REOP_UCFLATi       = 21; /* case-independent REOP_UCFLAT */
          case 22 => "REOP_CLASS          /* character class with index */                      "
          case 23 => "REOP_NCLASS         /* negated character class with index */              "
          //case 23 => "REOP_SIMPLE_END     /* end of 'simple opcodes' */                         "
          case 25 => "REOP_QUANT          /* quantified atom: atom{1,2} */                      "
          case 26 => "REOP_STAR           /* zero or more occurrences of kid */                 "
          case 27 => "REOP_PLUS           /* one or more occurrences of kid */                  "
          case 28 => "REOP_OPT            /* optional subexpression in kid */                   "
          case 29 => "REOP_LPAREN         /* left paren bytecode: kid is u.num'th sub-regexp */ "
          case 30 => "REOP_RPAREN         /* right paren bytecode */                            "
          case 31 => "REOP_ALT            /* alternative subexpressions in kid and next */      "
          case 32 => "REOP_JUMP           /* for deoptimized closure loops */                   "
      //    private static final byte REOP_DOTSTAR       = 33; /* optimize .* to use a single opcode */
      //    private static final byte REOP_ANCHOR        = 34; /* like .* but skips left context to unanchored r.e. */
      //    private static final byte REOP_EOLONLY       = 35; /* $ not preceded by any pattern */
      //    private static final byte REOP_BACKREFi      = 37; /* case-independent REOP_BACKREF */
      //    private static final byte REOP_LPARENNON     = 40; /* non-capturing version of REOP_LPAREN */
          case 41 => "REOP_ASSERT         /* zero width positive lookahead assertion */         "
          case 42 => "REOP_ASSERT_NOT     /* zero width negative lookahead assertion */         "
          case 43 => "REOP_ASSERTTEST     /* sentinel at end of assertion child */              "
          case 44 => "REOP_ASSERTNOTTEST  /* sentinel at end of !assertion child */             "
          case 45 => "REOP_MINIMALSTAR    /* non-greedy version of * */                         "
          case 46 => "REOP_MINIMALPLUS    /* non-greedy version of + */                         "
          case 47 => "REOP_MINIMALOPT     /* non-greedy version of ? */                         "
          case 48 => "REOP_MINIMALQUANT   /* non-greedy version of {} */                        "
          case 49 => "REOP_ENDCHILD       /* sentinel at end of quantifier child */             "
          case 51 => "REOP_REPEAT         /* directs execution of greedy quantifier */          "
          case 52 => "REOP_MINIMALREPEAT  /* directs execution of non-greedy quantifier */      "
          case 53 => "REOP_ALTPREREQ      /* prerequisite for ALT, either of two chars */       "
          case 54 => "REOP_ALTPREREQi     /* case-independent REOP_ALTPREREQ */                 "
          case 55 => "REOP_ALTPREREQ2     /* prerequisite for ALT, a char or a class */         "
      //    private static final byte R: Byte EOP_ENDALT        = 56; /* end of final alternate */
          case 57 => "REOP_END           "

      case _ => op.toString
    }



// TR compile entry point

    def compileRE(cx: Context, str: String, global: String, flat: Boolean): RECompiled =
    {
        val regexp = new RECompiled(str);
        val length = str.length();
        var flags = 0;
        if (global != null) {
            for (i <- 0 until global.length()) {
                var c = global.charAt(i);
                if (c == 'g') {
                    flags |= JSREG_GLOB;
                } else if (c == 'i') {
                    flags |= JSREG_FOLD;
                } else if (c == 'm') {
                    flags |= JSREG_MULTILINE;
                } else {
                    reportError("msg.invalid.re.flag", String.valueOf(c));
                }
            }
        }
        regexp.flags = flags;

        val state = new CompilerState(cx, regexp.source, length, flags);
        if (flat && length > 0) {
            if (debug) {
                System.out.println("flat = \"" + str + "\"");
            }
            val result = new RESeqNode(REOP_FLAT);
            result.chr = state.cpbegin(0);
            result.length = length;
            result.flatIndex = 0;
            state.result = result
            state.progLength += 5;
        }
        else
            if (!parseDisjunction(state))
                return null;
        
        regexp.startNode = state.result

        regexp.program = new Array[Byte](state.progLength + 1);
        if (state.classCount != 0) {
            regexp.classList = new Array[RECharSet](state.classCount);
            regexp.classCount = state.classCount;
        }
        var endPC = emitREBytecode(state, regexp, 0, state.result);
        regexp.program(endPC) = REOP_END;
        endPC += 1

        if (debug) {
            System.out.println("Prog. length = " + endPC);
            for (i <- 0 until endPC) {
                System.out.print(regexp.program(i));
                if (i < (endPC - 1)) System.out.print(", ");
            }
            System.out.println();
        }
        regexp.parenCount = state.parenCount;

        // If re starts with literal, init anchorCh accordingly
        (regexp.program(0)) match {
            case REOP_UCFLAT1 | REOP_UCFLAT1i =>
                regexp.anchorCh = getIndex(regexp.program, 1).toChar;
            case REOP_FLAT1 | REOP_FLAT1i =>
                regexp.anchorCh = (regexp.program(1) & 0xFF).toChar;
            case REOP_FLAT | REOP_FLATi =>
                val k = getIndex(regexp.program, 1);
                regexp.anchorCh = regexp.source(k);
            case REOP_BOL =>
                regexp.anchorCh = ANCHOR_BOL;
            case REOP_ALT =>
                val n = state.result;
                if (n.kid.op == REOP_BOL && n.kid2.op == REOP_BOL) {
                    regexp.anchorCh = ANCHOR_BOL;
                }
            case _ =>
              //default
        }

        if (debug) {
            if (regexp.anchorCh >= 0) {
                System.out.println("Anchor ch = '" + regexp.anchorCh.toChar + "'");
            }
        }
        return regexp;
    }

    // TR stub
    def emitREBytecodeStub(state: CompilerState, re: RECompiled, pc: Int, t0: RENode): Int = {
      var t = t0
      while (t ne null) {
        println(t)
        t = t.next
      }
      
      0
    }


    def isDigit(c: Char): Boolean =
    {
        return '0' <= c && c <= '9';
    }

    def isWord(c: Char): Boolean =
    {
        return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || isDigit(c) || c == '_';
    }

    def isControlLetter(c: Char): Boolean =
    {
        return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
    }

    def isLineTerm(c: Char): Boolean =
    {
        return ScriptRuntime.isJSLineTerminator(c);
    }

    def isREWhiteSpace(c: Int): Boolean =
    {
        return ScriptRuntime.isJSWhitespaceOrLineTerminator(c);
    }

    /*
     *
     * 1. If IgnoreCase is false, return ch.
     * 2. Let u be ch converted to upper case as if by calling
     *    String.prototype.toUpperCase on the one-character string ch.
     * 3. If u does not consist of a single character, return ch.
     * 4. Let cu be u's character.
     * 5. If ch's code point value is greater than or equal to decimal 128 and cu's
     *    code point value is less than decimal 128, then return ch.
     * 6. Return cu.
     */
    def upcase(ch: Char): Char =
    {
        if (ch < 128) {
            if ('a' <= ch && ch <= 'z') {
                return (ch + ('A' - 'a')).toChar;
            }
            return ch;
        }
        val cu = Character.toUpperCase(ch);
        return if (cu < 128) ch else cu;
    }

    def downcase(ch: Char): Char =
    {
        if (ch < 128) {
            if ('A' <= ch && ch <= 'Z') {
                return (ch + ('a' - 'A')).toChar;
            }
            return ch;
        }
        val cl = Character.toLowerCase(ch);
        return if (cl < 128) ch else cl;
    }

/*
 * Validates and converts hex ascii value.
 */
    def toASCIIHexDigit(c0: Int): Int =
    {
        var c = c0
        if (c < '0')
            return -1;
        if (c <= '9') {
            return c - '0';
        }
        c |= 0x20;
        if ('a' <= c && c <= 'f') {
            return c - 'a' + 10;
        }
        return -1;
    }

/*
 * Top-down regular expression grammar, based closely on Perl4.
 *
 *  regexp:     altern                  A regular expression is one or more
 *              altern '|' regexp       alternatives separated by vertical bar.
 */
    def parseDisjunction(state: CompilerState): Boolean =
    {
        if (!parseAlternative(state))
            return false;
        val source = state.cpbegin;
        var index = state.cp;
        if (index != source.length && source(index) == '|') {
            state.cp += 1;
            val result = new RESeqNode(REOP_ALT);
            result.kid = state.result;
            if (!parseDisjunction(state))
                return false;
            result.kid2 = state.result;
            state.result = result;
            /*
             * Look at both alternates to see if there's a FLAT or a CLASS at
             * the start of each. If so, use a prerequisite match.
             */
            /*if (result.kid.op == REOP_FLAT && result.kid2.op == REOP_FLAT) {
                result.op = if ((state.flags & JSREG_FOLD) == 0)
                        REOP_ALTPREREQ else REOP_ALTPREREQi;
                result.chr = result.kid.asInstanceOf[RESeqNode].chr;
                result.index = result.kid2.asInstanceOf[RESeqNode].chr;
                /* ALTPREREQ, uch1, uch2, <next>, ...,
                                            JUMP, <end> ... JUMP, <end> */
                state.progLength += 13;
            } else if (result.kid.op == REOP_CLASS && result.kid.asInstanceOf[REClassNode].index < 256
                    && result.kid2.op == REOP_FLAT && (state.flags & JSREG_FOLD) == 0) {
                result.op = REOP_ALTPREREQ2;
                result.chr = result.kid2.asInstanceOf[RESeqNode].chr;
                result.index = result.kid.asInstanceOf[REClassNode].index;
                /* ALTPREREQ2, uch1, uch2, <next>, ...,
                                            JUMP, <end> ... JUMP, <end> */
                state.progLength += 13;
            } else if (result.kid.op == REOP_FLAT && result.kid2.op == REOP_CLASS
                    && result.kid2.asInstanceOf[REClassNode].index < 256 && (state.flags & JSREG_FOLD) == 0) {
                result.op = REOP_ALTPREREQ2;
                result.chr = result.kid.asInstanceOf[RESeqNode].chr;
                result.index = result.kid2.asInstanceOf[REClassNode].index;
                /* ALTPREREQ2, uch1, uch2, <next>, ...,
                                            JUMP, <end> ... JUMP, <end> */
                state.progLength += 13;
            } else*/ {
                /* ALT, <next>, ..., JUMP, <end> ... JUMP, <end> */
                state.progLength += 9;
            }
        }
        return true;
    }

/*
 *  altern:     item                    An alternative is one or more items,
 *              item altern             concatenated together.
 */
    def parseAlternative(state: CompilerState): Boolean =
    {
        var headTerm: RENode = null;
        var tailTerm: RENode = null;
        var source = state.cpbegin;
        while (true) {
            if (state.cp == state.cpend || source(state.cp) == '|'
                || (state.parenNesting != 0 && source(state.cp) == ')'))
            {
                if (headTerm == null) {
                    state.result = new RENode(REOP_EMPTY);
                }
                else
                    state.result = headTerm;
                return true;
            }
            if (!parseTerm(state))
                return false;
            if (headTerm == null) {
                headTerm = state.result;
                tailTerm = headTerm;
            }
            else
                tailTerm.next = state.result;
            while (tailTerm.next != null) tailTerm = tailTerm.next;
        }
        false // never reached
    }

    /* calculate the total size of the bitmap required for a class expression */
    def calculateBitmapSize(state: CompilerState, target: REClassNode, src: Array[Char],
                        index0: Int, end: Int): Boolean =
    {
        var index = index0
        
        var rangeStart: Char = 0;
        var c: Char = 0;
        var n: Int = 0;
        var nDigits: Int = 0;
        var i: Int = 0;
        var max: Int = 0;
        var inRange: Boolean = false;

        target.bmsize = 0;
        target.sense = true;

        if (index == end)
            return true;

        if (src(index) == '^') {
            index += 1;
            target.sense = false;
        }

        while (index != end) {
            var localMax = 0;
            nDigits = 2;
            src(index) match {
            case '\\' =>
                index += 1;
                c = src(index);
                index += 1;
                c match {
                case 'b' =>
                    localMax = 0x8;
                case 'f' =>
                    localMax = 0xC;
                case 'n' =>
                    localMax = 0xA;
                case 'r' =>
                    localMax = 0xD;
                case 't' =>
                    localMax = 0x9;
                case 'v' =>
                    localMax = 0xB;
                case 'c' =>
                    if ((index < end) && isControlLetter(src(index))) {
                        localMax = (src(index) & 0x1F).toChar
                        index += 1;
                    } else
                        index -= 1;
                        localMax = '\\'; //TR indentation or bug?!?
                case 'u'|'x' =>
                    if (c == 'u') {
                      nDigits += 2;
                      // fall thru...
                    }
                    n = 0;
                    i = 0
                    var break = false
                    while ((i < nDigits) && (index < end) && !break) {
                        c = src(index)
                        index += 1
                        n = Kit.xDigitToInt(c, n);
                        if (n < 0) {
                            // Back off to accepting the original
                            // '\' as a literal
                            index -= (i + 1);
                            n = '\\';
                            break = true;
                        }
                        i += 1
                    }
                    localMax = n;
                case 'd' =>
                    if (inRange) {
                        reportError("msg.bad.range", "");
                        return false;
                    }
                    localMax = '9';
                case 'D'|'s'|'S'|'w'|'W' =>
                    if (inRange) {
                        reportError("msg.bad.range", "");
                        return false;
                    }
                    target.bmsize = 65536;
                    return true;
                case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7' =>
                    /*
                     *  This is a non-ECMA extension - decimal escapes (in this
                     *  case, octal!) are supposed to be an error inside class
                     *  ranges, but supported here for backwards compatibility.
                     *
                     */
                    n = (c - '0');
                    c = src(index);
                    if ('0' <= c && c <= '7') {
                        index+=1;
                        n = 8 * n + (c - '0');
                        c = src(index);
                        if ('0' <= c && c <= '7') {
                            index+=1;
                            i = 8 * n + (c - '0');
                            if (i <= 0377)
                                n = i;
                            else
                                index-=1;
                        }
                    }
                    localMax = n;

                case _ =>
                    localMax = c;
                }
            case _ =>
                localMax = src(index)
                index += 1
            }
            var continue = false
            if (inRange) {
                if (rangeStart > localMax) {
                    reportError("msg.bad.range", "");
                    return false;
                }
                inRange = false;
            }
            else {
                if (index < (end - 1)) {
                    if (src(index) == '-') {
                        index += 1;
                        inRange = true;
                        rangeStart = localMax.toChar;
                        continue = true;
                    }
                }
            }
            if (!continue) {
            if ((state.flags & JSREG_FOLD) != 0){
                val cu = upcase(localMax.toChar);
                val cd = downcase(localMax.toChar);
                localMax = if (cu >= cd) cu else cd;
            }
            if (localMax > max)
                max = localMax;
            }
        }
        target.bmsize = max + 1;
        return true;
    }

    /*
     *  item:       assertion               An item is either an assertion or
     *              quantatom               a quantified atom.
     *
     *  assertion:  '^'                     Assertions match beginning of string
     *                                      (or line if the class static property
     *                                      RegExp.multiline is true).
     *              '$'                     End of string (or line if the class
     *                                      static property RegExp.multiline is
     *                                      true).
     *              '\b'                    Word boundary (between \w and \W).
     *              '\B'                    Word non-boundary.
     *
     *  quantatom:  atom                    An unquantified atom.
     *              quantatom '{' n ',' m '}'
     *                                      Atom must occur between n and m times.
     *              quantatom '{' n ',' '}' Atom must occur at least n times.
     *              quantatom '{' n '}'     Atom must occur exactly n times.
     *              quantatom '*'           Zero or more times (same as {0,}).
     *              quantatom '+'           One or more times (same as {1,}).
     *              quantatom '?'           Zero or one time (same as {0,1}).
     *
     *              any of which can be optionally followed by '?' for ungreedy
     *
     *  atom:       '(' regexp ')'          A parenthesized regexp (what matched
     *                                      can be addressed using a backreference,
     *                                      see '\' n below).
     *              '.'                     Matches any char except '\n'.
     *              '[' classlist ']'       A character class.
     *              '[' '^' classlist ']'   A negated character class.
     *              '\f'                    Form Feed.
     *              '\n'                    Newline (Line Feed).
     *              '\r'                    Carriage Return.
     *              '\t'                    Horizontal Tab.
     *              '\v'                    Vertical Tab.
     *              '\d'                    A digit (same as [0-9]).
     *              '\D'                    A non-digit.
     *              '\w'                    A word character, [0-9a-z_A-Z].
     *              '\W'                    A non-word character.
     *              '\s'                    A whitespace character, [ \b\f\n\r\t\v].
     *              '\S'                    A non-whitespace character.
     *              '\' n                   A backreference to the nth (n decimal
     *                                      and positive) parenthesized expression.
     *              '\' octal               An octal escape sequence (octal must be
     *                                      two or three digits long, unless it is
     *                                      0 for the null character).
     *              '\x' hex                A hex escape (hex must be two digits).
     *              '\c' ctrl               A control character, ctrl is a letter.
     *              '\' literalatomchar     Any character except one of the above
     *                                      that follow '\' in an atom.
     *              otheratomchar           Any character not first among the other
     *                                      atom right-hand sides.
     */

    def doFlat(state: CompilerState, c: Char): Unit =
    {
        val result = new RESeqNode(REOP_FLAT);
        result.chr = c;
        result.length = 1;
        result.flatIndex = -1;
        state.result = result
        state.progLength += 3;
    }

    def getDecimalValue(c0: Char, state: CompilerState, maxValue: Int,
                    overflowMessageId: String): Int =
    {
        var c = c0
        var overflow = false;
        val start = state.cp;
        var src = state.cpbegin;
        var value = c - '0';
        var break = false
        while (state.cp != state.cpend && !break) {
            c = src(state.cp);
            if (!isDigit(c)) {
                break = true;
            }
            if (!break) {
            if (!overflow) {
                val digit = c - '0';
                if (value < (maxValue - digit) / 10) {
                    value = value * 10 + digit;
                } else {
                    overflow = true;
                    value = maxValue;
                }
            }
            state.cp += 1 //TR was for (;; ++state.cp)
            }
        }
        if (overflow) {
            reportError(overflowMessageId,
                        String.valueOf(src, start, state.cp - start));
        }
        return value;
    }

    def parseTerm(state: CompilerState): Boolean =
    {
        val src = state.cpbegin;
        var c = src(state.cp);
        state.cp += 1
        var nDigits = 2;
        var parenBaseCount = state.parenCount;
        var num, tmp: Int = 0;
        var term: RENode = null;
        var termStart: Int = 0;

        c match {
        /* assertions and atoms */
        case '^' =>
            state.result = new RENode(REOP_BOL);
            state.progLength += 1;
            return true;
        case '$' =>
            state.result = new RENode(REOP_EOL);
            state.progLength += 1;
            return true;
        case '\\' =>
            if (state.cp < state.cpend) {
                c = src(state.cp);
                state.cp += 1
                c match {
                /* assertion escapes */
                case 'b' =>
                    state.result = new RENode(REOP_WBDRY);
                    state.progLength += 1
                    return true;
                case 'B' =>
                    state.result = new RENode(REOP_WNONBDRY);
                    state.progLength += 1
                    return true;
                /* Decimal escape */
                case '0' =>
/*
 * Under 'strict' ECMA 3, we interpret \0 as NUL and don't accept octal.
 * However, (XXX and since Rhino doesn't have a 'strict' mode) we'll just
 * behave the old way for compatibility reasons.
 * (see http://bugzilla.mozilla.org/show_bug.cgi?id=141078)
 *
 */
                    reportWarning(state.cx, "msg.bad.backref", "");
                    /* octal escape */
                    num = 0;
                    var break = false
                    while (state.cp < state.cpend && !break) {
                        c = src(state.cp);
                        if ((c >= '0') && (c <= '7')) {
                            state.cp += 1
                            tmp = 8 * num + (c - '0');
                            if (tmp > 0377)
                                break = true;
                            if (!break)
                              num = tmp;
                        }
                        else
                            break = true;
                    }
                    c = (num).toChar;
                    doFlat(state, c);
                case '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' =>
                    termStart = state.cp - 1;
                    num = getDecimalValue(c, state, 0xFFFF,
                                          "msg.overlarge.backref");
                    if (num > state.parenCount)
                        reportWarning(state.cx, "msg.bad.backref", "");
                    /*
                     * n > 9 or > count of parentheses,
                     * then treat as octal instead.
                     */
                    if ((num > 9) && (num > state.parenCount)) {
                        state.cp = termStart;
                        num = 0;
                        var break = false
                        while (state.cp < state.cpend && !break) {
                            c = src(state.cp);
                            if ((c >= '0') && (c <= '7')) {
                                state.cp += 1;
                                tmp = 8 * num + (c - '0');
                                if (tmp > 0377)
                                    break = true;
                                if (!break)
                                  num = tmp;
                            }
                            else
                                break = true;
                        }
                        c = (num).toChar;
                        doFlat(state, c);
                    } else {
                    /* otherwise, it's a back-reference */
                    state.result = new RENode(REOP_BACKREF);
                    state.result.parenIndex = num - 1;
                    state.progLength += 3;
                    }
                /* Control escape */
                case 'f' =>
                    c = 0xC;
                    doFlat(state, c);
                case 'n' =>
                    c = 0xA;
                    doFlat(state, c);
                case 'r' =>
                    c = 0xD;
                    doFlat(state, c);
                case 't' =>
                    c = 0x9;
                    doFlat(state, c);
                case 'v' =>
                    c = 0xB;
                    doFlat(state, c);
                /* Control letter */
                case 'c' =>
                    if ((state.cp < state.cpend) &&
                                        isControlLetter(src(state.cp))) {
                        c = (src(state.cp) & 0x1F).toChar;
                        state.cp += 1
                    } else {
                        /* back off to accepting the original '\' as a literal */
                        state.cp -= 1;
                        c = '\\';
                    }
                    doFlat(state, c);
                /* UnicodeEscapeSequence */
                /* HexEscapeSequence */
                case 'u'|'x' =>
                    if (c == 'u')
                      nDigits += 2;
                      // fall thru...
                    {
                        var n = 0;
                        var i = 0;
                        var break = false
                        while ((i < nDigits)
                                && (state.cp < state.cpend) && !break) {
                            c = src(state.cp)
                            state.cp += 1
                            n = Kit.xDigitToInt(c, n);
                            if (n < 0) {
                                // Back off to accepting the original
                                // 'u' or 'x' as a literal
                                state.cp -= (i + 2);
                                n = src(state.cp)
                                state.cp += 1
                                break = true;
                            }
                            i += 1
                        }
                        c = n.toChar
                    }
                    doFlat(state, c);
                /* Character class escapes */
                case 'd' =>
                    state.result = new RENode(REOP_DIGIT);
                    state.progLength += 1;
                case 'D' =>
                    state.result = new RENode(REOP_NONDIGIT);
                    state.progLength += 1;
                case 's' =>
                    state.result = new RENode(REOP_SPACE);
                    state.progLength += 1;
                case 'S' =>
                    state.result = new RENode(REOP_NONSPACE);
                    state.progLength += 1;
                case 'w' =>
                    state.result = new RENode(REOP_ALNUM);
                    state.progLength += 1;
                case 'W' =>
                    state.result = new RENode(REOP_NONALNUM);
                    state.progLength += 1;
                /* IdentityEscape */
                case _ =>
                    val result = new RESeqNode(REOP_FLAT);
                    result.chr = c;
                    result.length = 1;
                    result.flatIndex = state.cp - 1;
                    state.result = result
                    state.progLength += 3;
                }
            }
            else {
                /* a trailing '\' is an error */
                reportError("msg.trail.backslash", "");
                return false;
            }
        case '(' => {
            var result: RENode = null;
            termStart = state.cp;
            if (state.cp + 1 < state.cpend && src(state.cp) == '?'
                && (({ c = src(state.cp + 1); c}) == '=' || c == '!' || c == ':'))
            {
                state.cp += 2;
                if (c == '=') {
                    result = new RENode(REOP_ASSERT);
                    /* ASSERT, <next>, ... ASSERTTEST */
                    state.progLength += 4;
                } else if (c == '!') {
                    result = new RENode(REOP_ASSERT_NOT);
                    /* ASSERTNOT, <next>, ... ASSERTNOTTEST */
                    state.progLength += 4;
                }
            } else {
                result = new RENode(REOP_LPAREN);
                /* LPAREN, <index>, ... RPAREN, <index> */
                state.progLength += 6;
                result.parenIndex = state.parenCount;
                state.parenCount += 1;
            }
            state.parenNesting += 1;
            if (!parseDisjunction(state))
                return false;
            if (state.cp == state.cpend || src(state.cp) != ')') {
                reportError("msg.unterm.paren", "");
                return false;
            }
            state.cp += 1;
            state.parenNesting -= 1;
            if (result != null) {
                result.kid = state.result;
                state.result = result;
            }
        }
        case ')' =>
          reportError("msg.re.unmatched.right.paren", "");
          return false;
        case '[' =>
            val result = new REClassNode(REOP_CLASS);
            state.result = result
            termStart = state.cp;
            result.startIndex = termStart;
            var break = false
            while (!break) {
                if (state.cp == state.cpend) {
                    reportError("msg.unterm.class", "");
                    return false;
                }
                if (src(state.cp) == '\\')
                    state.cp+=1;
                else {
                    if (src(state.cp) == ']') {
                        result.kidlen = state.cp - termStart;
                        break = true;
                    }
                }
                if (!break)
                  state.cp+=1;
            }
            result.index = state.classCount;
            state.classCount+=1;
            /*
             * Call calculateBitmapSize now as we want any errors it finds
             * to be reported during the parse phase, not at execution.
             */
            if (!calculateBitmapSize(state, result, src, termStart, state.cp)) {
                state.cp += 1 //TR necessary?
                return false;
            }
            state.cp += 1
            state.progLength += 3; /* CLASS, <index> */

        case '.' =>
            state.result = new RENode(REOP_DOT);
            state.progLength += 1;
        case '*'|'+'|'?' =>
            reportError("msg.bad.quant", String.valueOf(src(state.cp - 1)));
            return false;
        case _ =>
            val result = new RESeqNode(REOP_FLAT);
            state.result = result
            result.chr = c;
            result.length = 1;
            result.flatIndex = state.cp - 1;
            state.progLength += 3;
        }

        term = state.result;
        if (state.cp == state.cpend) {
            return true;
        }
        var hasQ = false;
        src(state.cp) match {
            case '+' =>
                val result = new RERangeNode(REOP_QUANT);
                state.result = result
                result.min = 1;
                result.max = -1;
                /* <PLUS>, <parencount>, <parenindex>, <next> ... <ENDCHILD> */
                state.progLength += 8;
                hasQ = true;
            case '*' =>
                val result = new RERangeNode(REOP_QUANT);
                state.result = result
                result.min = 0;
                result.max = -1;
                /* <STAR>, <parencount>, <parenindex>, <next> ... <ENDCHILD> */
                state.progLength += 8;
                hasQ = true;
            case '?' =>
                val result = new RERangeNode(REOP_QUANT);
                state.result = result
                result.min = 0;
                result.max = 1;
                /* <OPT>, <parencount>, <parenindex>, <next> ... <ENDCHILD> */
                state.progLength += 8;
                hasQ = true;
            case '{' =>  /* balance '}' */
            {
                var min = 0;
                var max = -1;
                var leftCurl = state.cp;

               /* For Perl etc. compatibility, if quntifier does not match
                * \{\d+(,\d*)?\} exactly back off from it
                * being a quantifier, and chew it up as a literal
                * atom next time instead.
                */
                
                state.cp += 1
                if (state.cp < src.length && { c = src(state.cp); isDigit(c) } ) {
                    state.cp += 1
                    min = getDecimalValue(c, state, 0xFFFF,
                                          "msg.overlarge.min");
                    c = src(state.cp);
                    if (c == ',') {
                        state.cp += 1
                        c = src(state.cp);
                        if (isDigit(c)) {
                            state.cp += 1
                            max = getDecimalValue(c, state, 0xFFFF,
                                                  "msg.overlarge.max");
                            c = src(state.cp);
                            if (min > max) {
                                reportError("msg.max.lt.min",
                                            String.valueOf(src(state.cp)));
                                return false;
                            }
                        }
                    } else {
                        max = min;
                    }
                    /* balance '{' */
                    if (c == '}') {
                        val result = new RERangeNode(REOP_QUANT);
                        state.result = result
                        result.min = min;
                        result.max = max;
                        // QUANT, <min>, <max>, <parencount>,
                        // <parenindex>, <next> ... <ENDCHILD>
                        state.progLength += 12;
                        hasQ = true;
                    }
                }
                if (!hasQ) {
                    state.cp = leftCurl;
                }
            }
            case c =>
              //println("hit default: " + c + " -- what to do?")
        }
        if (!hasQ)
            return true;

        val result = state.result.asInstanceOf[RERangeNode]

        state.cp += 1;
        result.kid = term;
        result.parenIndex = parenBaseCount;
        result.parenCount = state.parenCount - parenBaseCount;
        if ((state.cp < state.cpend) && (src(state.cp) == '?')) {
            state.cp += 1;
            result.greedy = false;
        }
        else
            result.greedy = true;
        return true;
    }

//TR match naive 


  def matchNaive(re: RECompiled, input: String, inp: Int = 0): REGlobalData = {
    
    debug = false //new String(re.source).contains("\\|") && input.startsWith("m:94")
    if (debug) println("debug on: " + new String(re.source) + " @ " + inp + " @ " + input)
    
    val t = re.startNode
    
    for (i <- inp to input.length) { // need to include last index: may match empty string
      val (gData, res) = matchNaive(re, t, input, i)
      if (res)
        return gData
    }
    return null
  }


  def matchNaive(re: RECompiled, t: RENode, input: String, inp: Int): (REGlobalData, Boolean) = {
    
    var gData = new REGlobalData

    if (re.parenCount != 0) {
        gData.parens = new Array[Long](re.parenCount);
    } else {
        gData.parens = null;
    }

    gData.backTrackStackTop = null;
    gData.stateStackTop = null;

    gData.multiline = (re.flags & JSREG_MULTILINE) != 0;
    gData.regexp = re;

    gData.skipped = inp
    gData.cp = inp

    for (j <- 0 until re.parenCount) {
        gData.parens(j) = -1l;
    }

    var end = input.length
    
  def matchNode(t: RENode)(k: Boolean => Boolean): Boolean = {
    if (t == null) return k(true)
    
    if (debug)
      println("match " + t + " at " + input.substring(gData.cp))
    
    val k1 = (x: Boolean) => if (x) matchNode(t.next)(k) else k(false)
    
    val matching = t.op match {
        case REOP_EMPTY =>
            true
        case REOP_BOL =>
            (gData.cp == 0) || (gData.multiline && isLineTerm(input.charAt(gData.cp - 1)))
        case REOP_EOL =>
            (gData.cp == end)  || (gData.multiline && isLineTerm(input.charAt(gData.cp)))

        case REOP_WBDRY =>
            ((gData.cp == 0 || !isWord(input.charAt(gData.cp - 1)))
                    ^ !((gData.cp < end) && isWord(input.charAt(gData.cp))));
        case REOP_WNONBDRY =>
            ((gData.cp == 0 || !isWord(input.charAt(gData.cp - 1)))
                    ^ ((gData.cp < end) && isWord(input.charAt(gData.cp))));
        case REOP_DOT =>
            (gData.cp != end && !isLineTerm(input.charAt(gData.cp))) && {
                gData.cp+=1; true
            }
        case REOP_DIGIT =>
            (gData.cp != end && isDigit(input.charAt(gData.cp))) && {
                gData.cp+=1; true
            }
        case REOP_NONDIGIT =>
            (gData.cp != end && !isDigit(input.charAt(gData.cp))) && {
                gData.cp+=1; true
            }
        case REOP_ALNUM =>
            (gData.cp != end && isWord(input.charAt(gData.cp))) && {
                gData.cp+=1; true
            }
        case REOP_NONALNUM =>
            (gData.cp != end && !isWord(input.charAt(gData.cp))) && {
                gData.cp+=1; true
            }
        case REOP_SPACE =>
            (gData.cp != end && isREWhiteSpace(input.charAt(gData.cp))) && {
                gData.cp+=1; true
            }
        case REOP_NONSPACE =>
            (gData.cp != end && !isREWhiteSpace(input.charAt(gData.cp))) && {
                gData.cp+=1; true
            }
        case REOP_CLASS =>
            val t2 = t.asInstanceOf[REClassNode]
            //if (!t2.sense)
            //    program(pc - 1) = REOP_NCLASS;
            assert(t2.index < re.classCount, t2.index + " >= " + re.classCount)
            re.classList(t2.index) = new RECharSet(t2.bmsize, t2.startIndex,
                                                  t2.kidlen, t2.sense);

            val index = t2.index
            (gData.cp != end) && (classMatcher(gData, re.classList(index),
                    input.charAt(gData.cp))) && {
                      gData.cp += 1; true
                    }

        case REOP_FLAT =>
            /*
             * Consecutize FLAT's if possible.
             */
            val t2 = t.asInstanceOf[RESeqNode]
            if (t2.flatIndex != -1) {
                while ((t.next != null) && (t.next.op == REOP_FLAT)
                        && ((t2.flatIndex + t2.length)
                                        == t.next.asInstanceOf[RESeqNode].flatIndex)) {
                    t2.length += t.next.asInstanceOf[RESeqNode].length;
                    t.next = t.next.next;
                }
            }
            if ((t2.flatIndex != -1) && (t2.length > 1)) {
                if ((re.flags & JSREG_FOLD) != 0) //REOP_FLATi
                    flatNIMatcher(gData, t2.flatIndex, t2.length, input, end);
                else // REOP_FLAT
                    flatNMatcher(gData, t2.flatIndex, t2.length, input, end);
            }
            else {
                val matchCh = t2.chr
                def xform(c: Char) = if ((re.flags & JSREG_FOLD) != 0) upcase(c) else c
                (gData.cp != end) && {
                    val c = input.charAt(gData.cp);
                    (matchCh == c || xform(matchCh) == xform(c)) && {
                        gData.cp+=1;
                        true
                    }
                }
            }
        
        case REOP_BACKREF =>
            backrefMatcher(gData, t.parenIndex, input, end);
        
            
        case REOP_ALTPREREQ
         | REOP_ALTPREREQi
         | REOP_ALTPREREQ2
         | REOP_ALT =>
            val prefix = if (t.op != REOP_ALT) {
              val t2 = t.asInstanceOf[RESeqNode]

              val ignoreCase = t.op == REOP_ALTPREREQi;

              val matchCh1 = if (ignoreCase) upcase(t2.chr) else t2.chr
              val matchCh2 = if (ignoreCase) upcase(t2.index.toChar) else t2.index.toChar

              (gData.cp != end) && {
                var c = input.charAt(gData.cp);
                if (t.op == REOP_ALTPREREQ2) {
                    ! ((c != matchCh1 &&
                        !classMatcher(gData, gData.regexp.classList(matchCh2), c)))
                } else {
                    if (t.op == REOP_ALTPREREQi)
                        c = upcase(c);
                    ! (c != matchCh1 && c != matchCh2)
                }
              }
            } else true
            
            return prefix && {
              val saveCp = gData.cp
              val saveParens = if (gData.parens == null) null else gData.parens.toList
              matchNode(t.kid)(r => k1(r) || {
                gData.cp = saveCp
                gData.parens = if (saveParens == null) null else saveParens.toArray
                matchNode(t.kid2)(k1) })}
            

        case REOP_LPAREN =>
            gData.setParens(t.parenIndex, gData.cp, 0);
            return matchNode(t.kid)(r => k1(r && {
              val cap_index = gData.parensIndex(t.parenIndex);
              gData.setParens(t.parenIndex, cap_index,
                      gData.cp - cap_index);
              true
            }))
        case REOP_ASSERT =>
            val saveCp = gData.cp
            println("not properly handling assert")
            return matchNode(t.kid)(r => k1(r && {
              // backtrack state
              gData.cp = saveCp
              true
            }))
        case REOP_ASSERT_NOT =>
            val saveCp = gData.cp
            println("not properly handling assert")
            return matchNode(t.kid)(r => k1(!r && {
              // backtrack state
              gData.cp = saveCp
              true
            }))
            
        case REOP_QUANT => // repetition
            val t2 = t.asInstanceOf[RERangeNode]
            
            if (t2.greedy) {
              // (kid.match && this.match) || next.match 
              
              val parenIndex = t2.parenIndex
            
              def loop(i: Int)(k: Boolean => Boolean): Boolean = {
                
                if (debug)
                  println("try loop " + i + " {" + t2.min + ".." + t2.max +  "} of "+ t.kid + " at " + input.substring(gData.cp))
                
                if (t2.max < 0 || i < t2.max) {
                  
                  // save and reset parens
                  val saveParens = if (gData.parens == null) null else gData.parens.toList
                  val saveCp = gData.cp

                  for (k <- parenIndex until re.parenCount) {
                      gData.setParens(k, -1, 0);
                  }

                  matchNode(t.kid) { res => 
                    if (debug)
                      println(res)
                      
                    (res && loop(i+1)(k)) || {
                      gData.parens = if (saveParens == null) null else saveParens.toArray
                      gData.cp = saveCp
                      k(i >= t2.min)
                    }
                  }
                } else
                  k(i >= t2.min)
              }
              return loop(0)(k1)
            
            } else { // not greedy
              if (debug) println("not greedy: " + t + " / " + t.kid + " / " + input.substring(gData.cp))
              assert(t2.max < 0 && t2.min == 0) // TODO: generalize
              // next.match || (kid.match && this.match)

              val saveParens = if (gData.parens == null) null else gData.parens.toList
              val saveCp = gData.cp
              
              // FIXME: this doesn't work, maybe we're inside parens -- we need to run the *full* continuation!
              val res = k1(true);
              return {
                if (debug) println("next: " + res)
                if (res) true else {              
                  gData.parens = if (saveParens == null) null else saveParens.toArray
                  gData.cp = saveCp
              
                  matchNode(t.kid)(r => r && matchNode(t)(k))
                }
              }
            }

        case _ =>
          println("don't know how to match " + t)
          true
    }
   
    k1(matching)
  }
  
  
  
    val res = matchNode(t)(x=>x)
    (gData, res)
  }



//TR emit bytecode

    def resolveForwardJump(array: Array[Byte], from: Int, pc: Int): Unit = 
    {
        if (from > pc) throw new Exception("Kit.codeBug()");
        addIndex(array, from, pc - from);
    }

    def getOffset(array: Array[Byte], pc: Int): Int =
    {
        return getIndex(array, pc);
    }

    def addIndex(array: Array[Byte], pc: Int, index: Int): Int =
    {
        if (index < 0) throw new Exception("Kit.codeBug();")
        if (index > 0xFFFF)
            throw new Exception("Too complex regexp");
        array(pc) = (index >> 8).toByte;
        array(pc + 1) = (index).toByte;
        return pc + 2;
    }

    def getIndex(array: Array[Byte], pc: Int): Int =
    {
        return ((array(pc) & 0xFF) << 8) | (array(pc + 1) & 0xFF);
    }

    final val INDEX_LEN: Int  = 2;

    def emitREBytecode(state: CompilerState, re: RECompiled, pc0: Int, t0: RENode): Int =
    {
      
        var nextAlt: RENode = null;
        var nextAltFixup, nextTermFixup: Int = 0;
        var program = re.program;
        var pc = pc0
        var t = t0

        while (t != null) {
            program(pc) = t.op;
            pc += 1
            t.op match {
            case REOP_EMPTY =>
                pc -= 1;
            case REOP_ALTPREREQ
             | REOP_ALTPREREQi
             | REOP_ALTPREREQ2
             | REOP_ALT =>
                if (t.op != REOP_ALT) {
                  val t2 = t.asInstanceOf[RESeqNode]
                  val ignoreCase = t.op == REOP_ALTPREREQi;
                  addIndex(program, pc, if (ignoreCase) upcase(t2.chr) else t2.chr);
                  pc += INDEX_LEN;
                  addIndex(program, pc, if (ignoreCase) upcase(t2.index.toChar) else t2.index);
                  pc += INDEX_LEN;
                  // fall through to REOP_ALT
                }
                nextAlt = t.kid2;
                nextAltFixup = pc;    /* address of next alternate */
                pc += INDEX_LEN;
                pc = emitREBytecode(state, re, pc, t.kid);
                program(pc) = REOP_JUMP;
                pc += 1
                nextTermFixup = pc;    /* address of following term */
                pc += INDEX_LEN;
                resolveForwardJump(program, nextAltFixup, pc);
                pc = emitREBytecode(state, re, pc, nextAlt);

                program(pc) = REOP_JUMP;
                pc += 1
                nextAltFixup = pc;
                pc += INDEX_LEN;

                resolveForwardJump(program, nextTermFixup, pc);
                resolveForwardJump(program, nextAltFixup, pc);
            case REOP_FLAT =>
                /*
                 * Consecutize FLAT's if possible.
                 */
                val t2 = t.asInstanceOf[RESeqNode]
                if (t2.flatIndex != -1) {
                    while ((t.next != null) && (t.next.op == REOP_FLAT)
                            && ((t2.flatIndex + t2.length)
                                            == t.next.asInstanceOf[RESeqNode].flatIndex)) {
                        t2.length += t.next.asInstanceOf[RESeqNode].length;
                        t.next = t.next.next;
                    }
                }
                if ((t2.flatIndex != -1) && (t2.length > 1)) {
                    if ((state.flags & JSREG_FOLD) != 0)
                        program(pc - 1) = REOP_FLATi;
                    else
                        program(pc - 1) = REOP_FLAT;
                    pc = addIndex(program, pc, t2.flatIndex);
                    pc = addIndex(program, pc, t2.length);
                }
                else {
                    if (t2.chr < 256) {
                        if ((state.flags & JSREG_FOLD) != 0)
                            program(pc - 1) = REOP_FLAT1i;
                        else
                            program(pc - 1) = REOP_FLAT1;
                        program(pc) = (t2.chr).toByte;
                        pc += 1
                    }
                    else {
                        if ((state.flags & JSREG_FOLD) != 0)
                            program(pc - 1) = REOP_UCFLAT1i;
                        else
                            program(pc - 1) = REOP_UCFLAT1;
                        pc = addIndex(program, pc, t2.chr);
                    }
                }
            case REOP_LPAREN =>
                pc = addIndex(program, pc, t.parenIndex);
                pc = emitREBytecode(state, re, pc, t.kid);
                program(pc) = REOP_RPAREN;
                pc += 1
                pc = addIndex(program, pc, t.parenIndex);
            case REOP_BACKREF =>
                pc = addIndex(program, pc, t.parenIndex);
            case REOP_ASSERT =>
                nextTermFixup = pc;
                pc += INDEX_LEN;
                pc = emitREBytecode(state, re, pc, t.kid);
                program(pc) = REOP_ASSERTTEST;
                pc += 1
                resolveForwardJump(program, nextTermFixup, pc);
            case REOP_ASSERT_NOT =>
                nextTermFixup = pc;
                pc += INDEX_LEN;
                pc = emitREBytecode(state, re, pc, t.kid);
                program(pc) = REOP_ASSERTNOTTEST;
                pc += 1
                resolveForwardJump(program, nextTermFixup, pc);
            case REOP_QUANT =>
                val t2 = t.asInstanceOf[RERangeNode]
                if ((t2.min == 0) && (t2.max == -1))
                    program(pc - 1) = if (t2.greedy) REOP_STAR else REOP_MINIMALSTAR;
                else
                if ((t2.min == 0) && (t2.max == 1))
                    program(pc - 1) = if (t2.greedy) REOP_OPT else REOP_MINIMALOPT;
                else
                if ((t2.min == 1) && (t2.max == -1))
                    program(pc - 1) = if (t2.greedy) REOP_PLUS else REOP_MINIMALPLUS;
                else {
                    if (!t2.greedy) program(pc - 1) = REOP_MINIMALQUANT;
                    pc = addIndex(program, pc, t2.min);
                    // max can be -1 which addIndex does not accept
                    pc = addIndex(program, pc, t2.max + 1);
                }
                pc = addIndex(program, pc, t2.parenCount);
                pc = addIndex(program, pc, t2.parenIndex);
                nextTermFixup = pc;
                pc += INDEX_LEN;
                pc = emitREBytecode(state, re, pc, t.kid);
                program(pc) = REOP_ENDCHILD;
                pc += 1
                resolveForwardJump(program, nextTermFixup, pc);
            case REOP_CLASS =>
                val t2 = t.asInstanceOf[REClassNode]
                if (!t2.sense)
                    program(pc - 1) = REOP_NCLASS;
                pc = addIndex(program, pc, t2.index);
                assert(t2.index < re.classCount, t2.index + " >= " + re.classCount)
                re.classList(t2.index) = new RECharSet(t2.bmsize, t2.startIndex,
                                                      t2.kidlen, t2.sense);
            case _ =>
            }
            t = t.next;
        }
        return pc;
    }

    def pushProgState(gData: REGlobalData, min: Int, max: Int, cp: Int,
                   backTrackLastToSave: REBackTrackData,
                  continuationOp: Int, continuationPc: Int) : Unit =
    {
        gData.stateStackTop = new REProgState(gData.stateStackTop, min, max,
                                              cp, backTrackLastToSave,
                                              continuationOp, continuationPc);
    }

    def popProgState(gData: REGlobalData): REProgState =
    {
        val state = gData.stateStackTop;
        gData.stateStackTop = state.previous;
        return state;
    }

    def pushBackTrackState(gData: REGlobalData, op: Byte, pc: Int): Unit = 
    {
        val state = gData.stateStackTop;
        gData.backTrackStackTop = new REBackTrackData(gData, op, pc,
                gData.cp, state.continuationOp, state.continuationPc);
    }


    def pushBackTrackState(gData: REGlobalData, op: Byte, pc: Int,
                       cp: Int, continuationOp: Int, continuationPc: Int): Unit = 
    {
        gData.backTrackStackTop = new REBackTrackData(gData, op, pc,
                cp, continuationOp, continuationPc);
    }

    /*
     *   Consecutive literal characters.
     */
    def flatNMatcher(gData: REGlobalData, matchChars: Int,
                 length: Int, input: String, end: Int): Boolean =
    {
        if ((gData.cp + length) > end)
            return false;
        var i = 0
        while (i < length) {
            if (gData.regexp.source(matchChars + i) != input.charAt(gData.cp + i)) {
                return false;
            }
            i += 1
        }
        gData.cp += length;
        return true;
    }

    def flatNIMatcher(gData: REGlobalData, matchChars: Int,
                  length: Int, input: String, end: Int): Boolean =
    {
        if ((gData.cp + length) > end)
            return false;
        val source = gData.regexp.source;
        var i = 0
        while (i < length) {
            val c1 = source(matchChars + i);
            val c2 = input.charAt(gData.cp + i);
            if (c1 != c2 && upcase(c1) != upcase(c2)) {
                return false;
            }
            i += 1
        }
        gData.cp += length;
        return true;
    }

    /*
    1. Evaluate DecimalEscape to obtain an EscapeValue E.
    2. If E is not a character then go to step 6.
    3. Let ch be E's character.
    4. Let A be a one-element RECharSet containing the character ch.
    5. Call CharacterSetMatcher(A, false) and return its Matcher result.
    6. E must be an integer. Let n be that integer.
    7. If n=0 or n>NCapturingParens then throw a SyntaxError exception.
    8. Return an internal Matcher closure that takes two arguments, a State x
       and a Continuation c, and performs the following:
        1. Let cap be x's captures internal array.
        2. Let s be cap[n].
        3. If s is undefined, then call c(x) and return its result.
        4. Let e be x's endIndex.
        5. Let len be s's length.
        6. Let f be e+len.
        7. If f>InputLength, return failure.
        8. If there exists an integer i between 0 (inclusive) and len (exclusive)
           such that Canonicalize(s[i]) is not the same character as
           Canonicalize(Input [e+i]), then return failure.
        9. Let y be the State (f, cap).
        10. Call c(y) and return its result.
    */
    def backrefMatcher(gData: REGlobalData, parenIndex: Int,
                   input: String, end: Int): Boolean =
    {
        var len: Int = 0;
        var i: Int = 0;
        if (gData.parens == null || parenIndex >= gData.parens.length)
            return false;
        val parenContent = gData.parensIndex(parenIndex);
        if (parenContent == -1)
            return true;

        len = gData.parensLength(parenIndex);
        if ((gData.cp + len) > end)
            return false;

        if ((gData.regexp.flags & JSREG_FOLD) != 0) {
            for (i <- 0 until len) {
                val c1 = input.charAt(parenContent + i);
                val c2 = input.charAt(gData.cp + i);
                if (c1 != c2 && upcase(c1) != upcase(c2))
                    return false;
            }
        }
        else if (!input.regionMatches(parenContent, input, gData.cp, len)) {
            return false;
        }
        gData.cp += len;
        return true;
    }


    /* Add a single character to the RECharSet */
    def addCharacterToCharSet(cs: RECharSet, c: Char): Unit =
    {
        val byteIndex = (c / 8);
        if (c >= cs.length) {
            throw new Exception("SyntaxError "+
                    "invalid range in character class");
        }
        cs.bits(byteIndex) = (cs.bits(byteIndex) | (1 << (c & 0x7))).toByte
    }


    /* Add a character range, c1 to c2 (inclusive) to the RECharSet */
    def addCharacterRangeToCharSet(cs: RECharSet, c1_ : Char, c2_ : Char): Unit = 
    {
        var i: Int = 0;
        var c1 = c1_
        var c2 = c2_

        val byteIndex1 = (c1 / 8);
        val byteIndex2 = (c2 / 8);

        if ((c2 >= cs.length) || (c1 > c2)) {
            throw new Exception("SyntaxError"+
                    "invalid range in character class");
        }

        c1 = (c1 & 0x7).toChar
        c2 = (c2 & 0x7).toChar;

        if (byteIndex1 == byteIndex2) {
            cs.bits(byteIndex1) = (cs.bits(byteIndex1) | (((0xFF) >> (7 - (c2 - c1))) << c1)).toByte;
        }
        else {
            cs.bits(byteIndex1) = (cs.bits(byteIndex1) | (0xFF << c1)).toByte;
            i = byteIndex1 + 1; 
            while(i < byteIndex2) {
                cs.bits(i) = 0xFF.toByte;
                i += 1
            }
            cs.bits(byteIndex2) = (cs.bits(byteIndex2) | ((0xFF) >> (7 - c2))).toByte;
        }
    }

    /* Compile the source of the class into a RECharSet */
    def processCharSet(gData: REGlobalData, charSet: RECharSet): Unit =
    {
        //synchronized (charSet) {
            if (!charSet.converted) {
                processCharSetImpl(gData, charSet);
                charSet.converted = true;
            }
        //}
    }


    def processCharSetImpl(gData: REGlobalData, charSet: RECharSet): Unit =
    {
        var src = charSet.startIndex;
        var end = src + charSet.strlength;

        var rangeStart: Char = 0
        var thisCh: Char = 0;
        var byteLength: Int = 0;
        var c: Char = 0
        var n: Int = 0;
        var nDigits: Int = 0;
        var i: Int = 0;
        var inRange = false;

        byteLength = (charSet.length + 7) / 8;
        charSet.bits = new Array[Byte](byteLength)

        if (src == end)
            return;

        if (gData.regexp.source(src) == '^') {
            assert (!charSet.sense);
            src += 1;
        } else {
            assert (charSet.sense);
        }

        while (src != end) {
            var continue = false
            nDigits = 2;
            (gData.regexp.source(src)) match {
            case '\\' =>
                src += 1;
                c = gData.regexp.source(src);
                src += 1
                c match {
                case 'b' =>
                    thisCh = 0x8;
                case 'f' =>
                    thisCh = 0xC;
                case 'n' =>
                    thisCh = 0xA;
                case 'r' =>
                    thisCh = 0xD;
                case 't' =>
                    thisCh = 0x9;
                case 'v' =>
                    thisCh = 0xB;
                case 'c' =>
                    if ((src < end) && isControlLetter(gData.regexp.source(src))) {
                        thisCh = (gData.regexp.source(src) & 0x1F).toChar;
                        src += 1
                    } else {
                        src -= 1;
                        thisCh = '\\';
                    }
                case 'u'|'x' =>
                    if (c == 'u') {
                      nDigits += 2
                      // fall thru
                    }
                    n = 0;
                    i = 0
                    var break = false
                    while ((i < nDigits) && (src < end) && !break) {
                        c = gData.regexp.source(src);
                        src += 1
                        val digit = toASCIIHexDigit(c);
                        if (digit < 0) {
                            /* back off to accepting the original '\'
                             * as a literal
                             */
                            src -= (i + 1);
                            n = '\\';
                            break = true;
                        }
                        if (!break) {
                          n = (n << 4) | digit;
                          i += 1
                        }
                    }
                    thisCh = (n).toChar;
                case '0'
                | '1'
                | '2'
                | '3'
                | '4'
                | '5'
                | '6'
                | '7' =>
                    /*
                     *  This is a non-ECMA extension - decimal escapes (in this
                     *  case, octal!) are supposed to be an error inside class
                     *  ranges, but supported here for backwards compatibility.
                     *
                     */
                    n = (c - '0');
                    c = gData.regexp.source(src);
                    if ('0' <= c && c <= '7') {
                        src+=1;
                        n = 8 * n + (c - '0');
                        c = gData.regexp.source(src);
                        if ('0' <= c && c <= '7') {
                            src+=1;
                            i = 8 * n + (c - '0');
                            if (i <= 0377)
                                n = i;
                            else
                                src-=1;
                        }
                    }
                    thisCh = (n).toChar;

                case 'd' =>
                    addCharacterRangeToCharSet(charSet, '0', '9');
                    continue = true;   /* don't need range processing */
                case 'D'=>
                    addCharacterRangeToCharSet(charSet, 0.toChar, ('0' - 1).toChar);
                    addCharacterRangeToCharSet(charSet, ('9' + 1).toChar,
                                                (charSet.length - 1).toChar);
                    continue = true;
                case 's'=>
                    i = (charSet.length - 1)
                    while (i >= 0) {
                        if (isREWhiteSpace(i))
                            addCharacterToCharSet(charSet, (i).toChar);
                        i -= 1
                    }
                    continue = true;
                case 'S'=>
                    i = (charSet.length - 1)
                    while (i >= 0) {
                        if (!isREWhiteSpace(i))
                            addCharacterToCharSet(charSet, (i).toChar);
                        i -= 1
                    }
                    continue = true;
                case 'w'=>
                    i = (charSet.length - 1)
                    while (i >= 0) {
                        if (isWord(i.toChar))
                            addCharacterToCharSet(charSet, (i).toChar);
                        i -= 1
                    }
                    continue = true;
                case 'W' =>
                    i = (charSet.length - 1)
                    while (i >= 0) {
                        if (!isWord(i.toChar))
                            addCharacterToCharSet(charSet, (i).toChar);
                        i -= 1
                    }
                    continue = true;
                case _ =>
                    thisCh = c;
                }

            case _ =>
                thisCh = gData.regexp.source(src);
                src += 1
            }
            if (!continue) {
            if (inRange) {
                if ((gData.regexp.flags & JSREG_FOLD) != 0) {
                    assert(rangeStart <= thisCh);
                    var break = false
                    c = rangeStart
                    while (c <= thisCh && !break) {
                        addCharacterToCharSet(charSet, c);
                        val uch = upcase(c);
                        val dch = downcase(c);
                        if (c != uch)
                            addCharacterToCharSet(charSet, uch);
                        if (c != dch)
                            addCharacterToCharSet(charSet, dch);
                        c = (c + 1).toChar
                        if (c == 0)
                            break = true; // overflow
                    }
                } else {
                    addCharacterRangeToCharSet(charSet, rangeStart, thisCh);
                }
                inRange = false;
            }
            else {
                if ((gData.regexp.flags & JSREG_FOLD) != 0) {
                    addCharacterToCharSet(charSet, upcase(thisCh));
                    addCharacterToCharSet(charSet, downcase(thisCh));
                } else {
                    addCharacterToCharSet(charSet, thisCh);
                }
                if (src < (end - 1)) {
                    if (gData.regexp.source(src) == '-') {
                        src += 1;
                        inRange = true;
                        rangeStart = thisCh;
                    }
                }
            }
            }
        }
    }


    /*
     *   Initialize the character set if it this is the first call.
     *   Test the bit - if the ^ flag was specified, non-inclusion is a success
     */
    def classMatcher(gData: REGlobalData, charSet: RECharSet, ch: Char): Boolean =
    {
        if (!charSet.converted) {
            processCharSet(gData, charSet);
        }

        val byteIndex = ch >> 3;
        return (charSet.length == 0 ||
                ch >= charSet.length ||
                (charSet.bits(byteIndex) & (1 << (ch & 0x7))) == 0) ^ charSet.sense;
    }

    def reopIsSimple(op: Int): Boolean =  {
        return op >= REOP_SIMPLE_START && op <= REOP_SIMPLE_END;
    }

    /*
    *   Apply the current op against the given input to see if
    *   it's going to match or fail. Return false if we don't
    *   get a match, true if we do and update the state of the
    *   input and pc if the update flag is true.
    */
    def simpleMatch(gData: REGlobalData, input: String, op: Int,
                                   program: Array[Byte], pc0: Int, end: Int, updatecp: Boolean): Int =
    {
        var pc = pc0
        var result = false;
        var matchCh: Char = 0;
        var parenIndex: Int = 0;
        var offset, length, index: Int = 0;
        var startcp = gData.cp;

        op match {
            case REOP_EMPTY =>
                result = true;
            case REOP_BOL =>
                var break = false
                if (gData.cp != 0) {
                    if (!gData.multiline || !isLineTerm(input.charAt(gData.cp - 1))) {
                        break = true;
                    }
                }
                if (!break)
                  result = true;
            case REOP_EOL =>
                var break = false
                if (gData.cp != end) {
                    if (!gData.multiline || !isLineTerm(input.charAt(gData.cp))) {
                        break = true;
                    }
                }
                if (break)
                  result = true;
            case REOP_WBDRY =>
                result = ((gData.cp == 0 || !isWord(input.charAt(gData.cp - 1)))
                        ^ !((gData.cp < end) && isWord(input.charAt(gData.cp))));
            case REOP_WNONBDRY =>
                result = ((gData.cp == 0 || !isWord(input.charAt(gData.cp - 1)))
                        ^ ((gData.cp < end) && isWord(input.charAt(gData.cp))));
            case REOP_DOT =>
                if (gData.cp != end && !isLineTerm(input.charAt(gData.cp))) {
                    result = true;
                    gData.cp+=1;
                }
            case REOP_DIGIT =>
                if (gData.cp != end && isDigit(input.charAt(gData.cp))) {
                    result = true;
                    gData.cp+=1;
                }
            case REOP_NONDIGIT =>
                if (gData.cp != end && !isDigit(input.charAt(gData.cp))) {
                    result = true;
                    gData.cp+=1;
                }
            case REOP_ALNUM =>
                if (gData.cp != end && isWord(input.charAt(gData.cp))) {
                    result = true;
                    gData.cp+=1;
                }
            case REOP_NONALNUM =>
                if (gData.cp != end && !isWord(input.charAt(gData.cp))) {
                    result = true;
                    gData.cp+=1;
                }
            case REOP_SPACE =>
                if (gData.cp != end && isREWhiteSpace(input.charAt(gData.cp))) {
                    result = true;
                    gData.cp+=1;
                }
            case REOP_NONSPACE =>
                if (gData.cp != end && !isREWhiteSpace(input.charAt(gData.cp))) {
                    result = true;
                    gData.cp+=1;
                }
            case REOP_BACKREF =>
            {
                parenIndex = getIndex(program, pc);
                pc += INDEX_LEN;
                result = backrefMatcher(gData, parenIndex, input, end);
            }
            case REOP_FLAT =>
            {
                offset = getIndex(program, pc);
                pc += INDEX_LEN;
                length = getIndex(program, pc);
                pc += INDEX_LEN;
                result = flatNMatcher(gData, offset, length, input, end);
            }
            case REOP_FLAT1 =>
            {
                matchCh = (program(pc) & 0xFF).toChar;
                pc += 1
                if (gData.cp != end && input.charAt(gData.cp) == matchCh) {
                    result = true;
                    gData.cp+=1;
                }
            }
            case REOP_FLATi =>
            {
                offset = getIndex(program, pc);
                pc += INDEX_LEN;
                length = getIndex(program, pc);
                pc += INDEX_LEN;
                result = flatNIMatcher(gData, offset, length, input, end);
            }
            case REOP_FLAT1i =>
            {
                matchCh = (program(pc) & 0xFF).toChar;
                pc += 1
                if (gData.cp != end) {
                    val c = input.charAt(gData.cp);
                    if (matchCh == c || upcase(matchCh) == upcase(c)) {
                        result = true;
                        gData.cp+=1;
                    }
                }
            }
            case REOP_UCFLAT1 =>
            {
                matchCh = getIndex(program, pc).toChar;
                pc += INDEX_LEN;
                if (gData.cp != end && input.charAt(gData.cp) == matchCh) {
                    result = true;
                    gData.cp+=1;
                }
            }
            case REOP_UCFLAT1i =>
            {
                matchCh = getIndex(program, pc).toChar;
                pc += INDEX_LEN;
                if (gData.cp != end) {
                    val c = input.charAt(gData.cp);
                    if (matchCh == c || upcase(matchCh) == upcase(c)) {
                        result = true;
                        gData.cp+=1;
                    }
                }
            }

            case REOP_CLASS | REOP_NCLASS =>
            {
                index = getIndex(program, pc);
                pc += INDEX_LEN;
                if (gData.cp != end) {
                    if (classMatcher(gData, gData.regexp.classList(index),
                            input.charAt(gData.cp)))
                    {
                        gData.cp+=1;
                        result = true;
                    }
                }
            }

            case _ =>
                throw new Exception("Kit.codeBug()")
        }
        if (result) {
            if (!updatecp)
                gData.cp = startcp;
            return pc;
        }
        gData.cp = startcp;
        return -1;
    }


    def executeREBytecode(gData: REGlobalData, input: String, end: Int): Boolean =
    {
        var pc = 0;
        var program = gData.regexp.program;
        var continuationOp: Int = REOP_END;
        var continuationPc = 0;
        var result = false;

        var op: Int = program(pc)
        pc += 1

        /*
         * If the first node is a simple match, step the index into the string
         * until that match is made, or fail if it can't be found at all.
         */
        if (gData.regexp.anchorCh < 0 && reopIsSimple(op)) {
            var anchor = false;
            var break = false
            while (gData.cp <= end && !break) {
                val matches = simpleMatch(gData, input, op, program, pc, end, true);
                if (matches >= 0) {
                    anchor = true;
                    pc = matches;    /* accept skip to next opcode */
                    op = program(pc)
                    pc += 1
                    break = true;
                }
                if (!break) {
                  gData.skipped+=1;
                  gData.cp+=1;
                }
            }
            if (!anchor)
                return false;
        }

        var break = false
        while(!break) {

            if (reopIsSimple(op)) {
                val matches = simpleMatch(gData, input, op, program, pc, end, true);
                result = matches >= 0;
                if (result)
                    pc = matches;    /* accept skip to next opcode */
            } else {
                val returnOuter = (x: Boolean) => return x
                switchStatement()
                def switchStatement(): Unit = op match {
                    case REOP_ALTPREREQ
                    | REOP_ALTPREREQi
                    | REOP_ALTPREREQ2 =>
                    {
                        val matchCh1 = getIndex(program, pc).toChar;
                        pc += INDEX_LEN;
                        val matchCh2 = getIndex(program, pc).toChar;
                        pc += INDEX_LEN;

                        if (gData.cp == end) {
                            result = false;
                            break;
                        }
                        var c = input.charAt(gData.cp);
                        if (op == REOP_ALTPREREQ2) {
                            if (c != matchCh1 &&
                                !classMatcher(gData, gData.regexp.classList(matchCh2), c)) {
                                result = false;
                                break;
                            }
                        } else {
                            if (op == REOP_ALTPREREQi)
                                c = upcase(c);
                            if (c != matchCh1 && c != matchCh2) {
                                result = false;
                                break;
                            }
                        }
                    }
                    /* else false thru... */
                    case REOP_ALT =>
                    {
                        var nextpc = pc + getOffset(program, pc);
                        pc += INDEX_LEN;
                        op = program(pc)
                        pc += 1
                        var startcp = gData.cp;
                        if (reopIsSimple(op)) {
                            var matches = simpleMatch(gData, input, op, program, pc, end, true);
                            if (matches < 0) {
                                op = program(nextpc)
                                nextpc += 1
                                pc = nextpc;
                                continue;
                            }
                            result = true;
                            pc = matches;
                            op = program(pc)
                            pc += 1
                        }
                        val nextop = program(nextpc)
                        nextpc += 1
                        pushBackTrackState(gData, nextop, nextpc, startcp,
                                continuationOp, continuationPc);
                    }
                    continue;

                    case REOP_JUMP =>
                    {
                        val offset = getOffset(program, pc);
                        pc += offset;
                        op = program(pc)
                        pc += 1
                    }
                    continue;


                    case REOP_LPAREN =>
                    {
                        val parenIndex = getIndex(program, pc);
                        pc += INDEX_LEN;
                        gData.setParens(parenIndex, gData.cp, 0);
                        op = program(pc)
                        pc += 1
                    }
                    continue;
                    case REOP_RPAREN =>
                    {
                        val parenIndex = getIndex(program, pc);
                        pc += INDEX_LEN;
                        val cap_index = gData.parensIndex(parenIndex);
                        gData.setParens(parenIndex, cap_index,
                                gData.cp - cap_index);
                        op = program(pc)
                        pc += 1
                    }
                    continue;

                    case REOP_ASSERT =>
                    {
                        val nextpc = pc + getIndex(program, pc); /* start of term after ASSERT */
                        pc += INDEX_LEN;                         /* start of ASSERT child */
                        op = program(pc)
                        pc += 1
                        if (reopIsSimple(op) && simpleMatch(gData, input, op, program, pc, end, false) < 0) {
                            result = false;
                            break;
                        }
                        pushProgState(gData, 0, 0, gData.cp, gData.backTrackStackTop,
                                continuationOp, continuationPc);
                        pushBackTrackState(gData, REOP_ASSERTTEST, nextpc);
                    }
                    continue;
                    case REOP_ASSERT_NOT =>
                    {
                        val nextpc = pc + getIndex(program, pc); /* start of term after ASSERT */
                        pc += INDEX_LEN;                         /* start of ASSERT child */
                        op = program(pc)
                        pc += 1
                        if (reopIsSimple(op)) {
                            val matches = simpleMatch(gData, input, op, program, pc, end, false);
                            if (matches >= 0 && program(matches) == REOP_ASSERTNOTTEST) {
                                result = false;
                                break;
                            }
                        }
                        pushProgState(gData, 0, 0, gData.cp, gData.backTrackStackTop,
                                continuationOp, continuationPc);
                        pushBackTrackState(gData, REOP_ASSERTNOTTEST, nextpc);
                    }
                    continue;

                    case REOP_ASSERTTEST =>
                    case REOP_ASSERTNOTTEST =>
                    {
                        val state = popProgState(gData);
                        gData.cp = state.index;
                        gData.backTrackStackTop = state.backTrack;
                        continuationPc = state.continuationPc;
                        continuationOp = state.continuationOp;
                        if (op == REOP_ASSERTNOTTEST) {
                            result = !result;
                        }
                    }
                    break;

                    case REOP_STAR
                    | REOP_PLUS
                    | REOP_OPT
                    | REOP_QUANT
                    | REOP_MINIMALSTAR
                    | REOP_MINIMALPLUS
                    | REOP_MINIMALOPT
                    | REOP_MINIMALQUANT =>
                    {
                        var min, max = 0;
                        var greedy = false;
                        op match {
                            case REOP_STAR =>
                                greedy = true;
                                // fallthrough
                            case REOP_MINIMALSTAR =>
                                min = 0;
                                max = -1;
                                break;
                            case REOP_PLUS =>
                                greedy = true;
                                // fallthrough
                            case REOP_MINIMALPLUS =>
                                min = 1;
                                max = -1;
                                break;
                            case REOP_OPT =>
                                greedy = true;
                                // fallthrough
                            case REOP_MINIMALOPT =>
                                min = 0;
                                max = 1;
                                break;
                            case REOP_QUANT =>
                                greedy = true;
                                // fallthrough
                            case REOP_MINIMALQUANT =>
                                min = getOffset(program, pc);
                                pc += INDEX_LEN;
                                // See comments in emitREBytecode for " - 1" reason
                                max = getOffset(program, pc) - 1;
                                pc += INDEX_LEN;
                                break;
                            case _ =>
                                throw new Exception("Kit.codeBug();")
                        }
                        pushProgState(gData, min, max, gData.cp, null,
                                continuationOp, continuationPc);
                        if (greedy) {
                            pushBackTrackState(gData, REOP_REPEAT, pc);
                            continuationOp = REOP_REPEAT;
                            continuationPc = pc;
                            /* Step over <parencount>, <parenindex> & <next> */
                            pc += 3 * INDEX_LEN;
                            op = program(pc)
                            pc += 1
                        } else {
                            if (min != 0) {
                                continuationOp = REOP_MINIMALREPEAT;
                                continuationPc = pc;
                                /* <parencount> <parenindex> & <next> */
                                pc += 3 * INDEX_LEN;
                                op = program(pc)
                                pc += 1
                            } else {
                                pushBackTrackState(gData, REOP_MINIMALREPEAT, pc);
                                popProgState(gData);
                                pc += 2 * INDEX_LEN;  // <parencount> & <parenindex>
                                pc = pc + getOffset(program, pc);
                                op = program(pc)
                                pc += 1
                            }
                        }
                    }
                    continue;

                    case REOP_ENDCHILD => /* marks the end of a quantifier child */
                        // If we have not gotten a result here, it is because of an
                        // empty match.  Do the same thing REOP_EMPTY would do.
                        result = true;
                        // Use the current continuation.
                        pc = continuationPc;
                        op = continuationOp;
                        continue;

                    case REOP_REPEAT =>
                    {
                        var nextpc, nextop = 0;
                        do {
                            var state = popProgState(gData);
                            if (!result) {
                                // Failed, see if we have enough children.
                                if (state.min == 0)
                                    result = true;
                                continuationPc = state.continuationPc;
                                continuationOp = state.continuationOp;
                                pc += 2 * INDEX_LEN;  /* <parencount> & <parenindex> */
                                pc += getOffset(program, pc);
                                return // switchStatement;
                            }
                            if (state.min == 0 && gData.cp == state.index) {
                                // matched an empty string, that'll get us nowhere
                                result = false;
                                continuationPc = state.continuationPc;
                                continuationOp = state.continuationOp;
                                pc += 2 * INDEX_LEN;
                                pc += getOffset(program, pc);
                                return // switchStatement;
                            }
                            var new_min = state.min
                            var new_max = state.max;
                            if (new_min != 0) new_min-=1;
                            if (new_max != -1) new_max-=1;
                            if (new_max == 0) {
                                result = true;
                                continuationPc = state.continuationPc;
                                continuationOp = state.continuationOp;
                                pc += 2 * INDEX_LEN;
                                pc += getOffset(program, pc);
                                return // switchStatement;
                            }
                            nextpc = pc + 3 * INDEX_LEN;
                            nextop = program(nextpc);
                            var startcp = gData.cp;
                            if (reopIsSimple(nextop)) {
                                nextpc+=1;
                                val matches = simpleMatch(gData, input, nextop, program, nextpc, end, true);
                                if (matches < 0) {
                                    result = (new_min == 0);
                                    continuationPc = state.continuationPc;
                                    continuationOp = state.continuationOp;
                                    pc += 2 * INDEX_LEN;  /* <parencount> & <parenindex> */
                                    pc += getOffset(program, pc);
                                    return // switchStatement;
                                }
                                result = true;
                                nextpc = matches;
                            }
                            continuationOp = REOP_REPEAT;
                            continuationPc = pc;
                            pushProgState(gData, new_min, new_max, startcp, null,
                                    state.continuationOp, state.continuationPc);
                            if (new_min == 0) {
                                pushBackTrackState(gData, REOP_REPEAT, pc, startcp,
                                        state.continuationOp, state.continuationPc);
                                var parenCount = getIndex(program, pc);
                                var parenIndex = getIndex(program, pc + INDEX_LEN);
                                for (k <- 0 until parenCount) {
                                    gData.setParens(parenIndex + k, -1, 0);
                                }
                            }
                        } while (program(nextpc) == REOP_ENDCHILD);

                        pc = nextpc;
                        op = program(pc)
                        pc += 1
                    }
                    continue;

                    case REOP_MINIMALREPEAT =>
                    {
                        var state = popProgState(gData);
                        if (!result) {
                            //
                            // Non-greedy failure - try to consume another child.
                            //
                            if (state.max == -1 || state.max > 0) {
                                pushProgState(gData, state.min, state.max, gData.cp, null,
                                        state.continuationOp, state.continuationPc);
                                continuationOp = REOP_MINIMALREPEAT;
                                continuationPc = pc;
                                var parenCount = getIndex(program, pc);
                                pc += INDEX_LEN;
                                var parenIndex = getIndex(program, pc);
                                pc += 2 * INDEX_LEN;
                                for (k <- 0 until parenCount) {
                                    gData.setParens(parenIndex + k, -1, 0);
                                }
                                op = program(pc)
                                pc += 1
                                continue;
                            } else {
                                // Don't need to adjust pc since we're going to pop.
                                continuationPc = state.continuationPc;
                                continuationOp = state.continuationOp;
                                break;
                            }
                        } else {
                            if (state.min == 0 && gData.cp == state.index) {
                                // Matched an empty string, that'll get us nowhere.
                                result = false;
                                continuationPc = state.continuationPc;
                                continuationOp = state.continuationOp;
                                break;
                            }
                            var new_min = state.min
                            var new_max = state.max;
                            if (new_min != 0) new_min-=1;
                            if (new_max != -1) new_max-=1;
                            pushProgState(gData, new_min, new_max, gData.cp, null,
                                    state.continuationOp, state.continuationPc);
                            if (new_min != 0) {
                                continuationOp = REOP_MINIMALREPEAT;
                                continuationPc = pc;
                                val parenCount = getIndex(program, pc);
                                pc += INDEX_LEN;
                                val parenIndex = getIndex(program, pc);
                                pc += 2 * INDEX_LEN;
                                for (k <- 0 until parenCount) {
                                    gData.setParens(parenIndex + k, -1, 0);
                                }
                                op = program(pc)
                                pc += 1
                            } else {
                                continuationPc = state.continuationPc;
                                continuationOp = state.continuationOp;
                                pushBackTrackState(gData, REOP_MINIMALREPEAT, pc);
                                popProgState(gData);
                                pc += 2 * INDEX_LEN;
                                pc = pc + getOffset(program, pc);
                                op = program(pc)
                                pc += 1
                            }
                            continue;
                        }
                    }

                    case REOP_END =>
                        returnOuter(true); //TR FIXME: return to outer method

                    case _ =>
                        throw new Exception("Kit.codeBug(\"invalid bytecode\")");

                }
            }
            /*
             *  If the match failed and there's a backtrack option, take it.
             *  Otherwise this is a complete and utter failure.
             */
            if (!result) {
                val backTrackData = gData.backTrackStackTop;
                if (backTrackData != null) {
                    gData.backTrackStackTop = backTrackData.previous;
                    gData.parens = backTrackData.parens;
                    gData.cp = backTrackData.cp;
                    gData.stateStackTop = backTrackData.stateStackTop;
                    continuationOp = backTrackData.continuationOp;
                    continuationPc = backTrackData.continuationPc;
                    pc = backTrackData.pc;
                    op = backTrackData.op;
                    continue;
                }
                else
                    return false;
            }

            op = program(pc)
            pc += 1
        }
        result
    }

    def matchRegExp(gData: REGlobalData,  re: RECompiled,
                input: String, start: Int, end: Int,  multiline: Boolean): Boolean = 
    {
        if (re.parenCount != 0) {
            gData.parens = new Array[Long](re.parenCount);
        } else {
            gData.parens = null;
        }

        gData.backTrackStackTop = null;
        gData.stateStackTop = null;

        gData.multiline = multiline || (re.flags & JSREG_MULTILINE) != 0;
        gData.regexp = re;

        val anchorCh = gData.regexp.anchorCh;
        //
        // have to include the position beyond the last character
        //  in order to detect end-of-input/line condition
        //
        var i = start
        while (i <= end) {
            //
            // If the first node is a literal match, step the index into
            // the string until that match is made, or fail if it can't be
            // found at all.
            //
            if (anchorCh >= 0) {
                var break = false
                while (!break) {
                    if (i == end) {
                        return false;
                    }
                    val matchCh = input.charAt(i);
                    if (matchCh == anchorCh ||
                            ((gData.regexp.flags & JSREG_FOLD) != 0
                             && upcase(matchCh) == upcase(anchorCh.toChar)))
                    {
                        break = true;
                    }
                    if (break) i += 1;
                }
            }
            gData.cp = i;
            gData.skipped = i - start;
            for (j <- 0 until re.parenCount) {
                gData.parens(j) = -1l;
            }
            val result = executeREBytecode(gData, input, end);

            gData.backTrackStackTop = null;
            gData.stateStackTop = null;
            if (result) {
                return true;
            }
            if (anchorCh == ANCHOR_BOL && !gData.multiline) {
                gData.skipped = end;
                return false;
            }
            i = start + gData.skipped;
            i += 1 //TR not sure this is correct, was ++i
        }
        return false;
    }


/*
    /*
     * indexp is assumed to be an array of length 1
     */
    Object executeRegExp(Context cx, Scriptable scope, RegExpImpl res,
                         String str, int indexp[], int matchType)
    {
        REGlobalData gData = new REGlobalData();

        int start = indexp[0];
        int end = str.length();
        if (start > end)
            start = end;
        //
        // Call the recursive matcher to do the real work.
        //
        boolean matches = matchRegExp(gData, re, str, start, end,
                                      res.multiline);
        if (!matches) {
            if (matchType != PREFIX) return null;
            return Undefined.instance;
        }
        int index = gData.cp;
        int ep = indexp[0] = index;
        int matchlen = ep - (start + gData.skipped);
        index -= matchlen;
        Object result;
        Scriptable obj;

        if (matchType == TEST) {
            /*
             * Testing for a match and updating cx.regExpImpl: don't allocate
             * an array object, do return true.
             */
            result = Boolean.TRUE;
            obj = null;
        }
        else {
            /*
             * The array returned on match has element 0 bound to the matched
             * string, elements 1 through re.parenCount bound to the paren
             * matches, an index property telling the length of the left context,
             * and an input property referring to the input string.
             */
            result = cx.newArray(scope, 0);
            obj = (Scriptable) result;

            String matchstr = str.substring(index, index + matchlen);
            obj.put(0, obj, matchstr);
        }

        if (re.parenCount == 0) {
            res.parens = null;
            res.lastParen = SubString.emptySubString;
        } else {
            SubString parsub = null;
            int num;
            res.parens = new SubString[re.parenCount];
            for (num = 0; num < re.parenCount; num++) {
                int cap_index = gData.parensIndex(num);
                String parstr;
                if (cap_index != -1) {
                    int cap_length = gData.parensLength(num);
                    parsub = new SubString(str, cap_index, cap_length);
                    res.parens[num] = parsub;
                    if (matchType != TEST)
                        obj.put(num+1, obj, parsub.toString());
                }
                else {
                    if (matchType != TEST)
                        obj.put(num+1, obj, Undefined.instance);
                }
            }
            res.lastParen = parsub;
        }

        if (! (matchType == TEST)) {
            /*
             * Define the index and input properties last for better for/in loop
             * order (so they come after the elements).
             */
            obj.put("index", obj, Integer.valueOf(start + gData.skipped));
            obj.put("input", obj, str);
        }

        if (res.lastMatch == null) {
            res.lastMatch = new SubString();
            res.leftContext = new SubString();
            res.rightContext = new SubString();
        }
        res.lastMatch.str = str;
        res.lastMatch.index = index;
        res.lastMatch.length = matchlen;

        res.leftContext.str = str;
        if (cx.getLanguageVersion() == Context.VERSION_1_2) {
            /*
             * JS1.2 emulated Perl4.0.1.8 (patch level 36) for global regexps used
             * in scalar contexts, and unintentionally for the string.match "list"
             * psuedo-context.  On "hi there bye", the following would result:
             *
             * Language     while(/ /g){print("$`");}   s/ /$`/g
             * perl4.036    "hi", "there"               "hihitherehi therebye"
             * perl5        "hi", "hi there"            "hihitherehi therebye"
             * js1.2        "hi", "there"               "hihitheretherebye"
             *
             * Insofar as JS1.2 always defined $` as "left context from the last
             * match" for global regexps, it was more consistent than perl4.
             */
            res.leftContext.index = start;
            res.leftContext.length = gData.skipped;
        } else {
            /*
             * For JS1.3 and ECMAv2, emulate Perl5 exactly:
             *
             * js1.3        "hi", "hi there"            "hihitherehi therebye"
             */
            res.leftContext.index = 0;
            res.leftContext.length = start + gData.skipped;
        }

        res.rightContext.str = str;
        res.rightContext.index = ep;
        res.rightContext.length = end - ep;

        return result;
    }

*/

    def getFlags(): Int =
    {
        return re.flags;
    }

    def reportWarning(cx: Context, messageId: String, arg: String): Unit =
    {
/*        if (cx.hasFeature(Context.FEATURE_STRICT_MODE)) {
            String msg = ScriptRuntime.getMessage1(messageId, arg);
            Context.reportWarning(msg);
        }
*/        
        println("WARNING: " + messageId + " / " + arg)
    }

    def reportError(messageId: String, arg: String)
    {
/*        String msg = ScriptRuntime.getMessage1(messageId, arg);
        throw ScriptRuntime.constructError("SyntaxError", msg);
*/
        println("ERROR: " + messageId + " / " + arg)
    }


    var re: RECompiled = _
    var lastIndex: Double = _;          /* index after last match, for //g iterator */

}       // class NativeRegExp

class RECompiled(str: String) //extends Serializable
{
    var startNode: RENode = _
  
    val source: Array[Char] = str.toCharArray();    /* locked source string, sans // */
    var parenCount: Int = _;         /* number of parenthesized submatches */
    var flags: Int = _;              /* flags  */
    var program: Array[Byte] = _;         /* regular expression bytecode */
    var classCount: Int = _;         /* count [...] bitmaps */
    var classList: Array[RECharSet] = _;  /* list of [...] bitmaps */
    var anchorCh: Int = -1;      /* if >= 0, then re starts with this literal char */

}

class RENode(var op: Byte) {

    override def toString = op match {
      case Rhino.REOP_ALT => "REOP_ALT(" + kid + "|" + kid2 + ")"
      case _ => Rhino.op2string(op)
    }

    //var op: Byte;         /* r.e. op bytecode */
    var next: RENode = _;       /* next in concatenation order */
    var kid: RENode = _;        /* first operand */

    var kid2: RENode = _;       /* second operand */
    var parenIndex: Int = _; /* or a parenthesis index */



}

class RERangeNode(op: Byte) extends RENode(op) {

                     /* or a range */
  var min: Int = _;
  var max: Int = _;
  var parenCount: Int = _;
  var greedy: Boolean = _;

}

class REClassNode(op: Byte) extends RENode(op) {

                  /* or a character class */
  var startIndex: Int = _;
  var kidlen: Int = _;     /* length of string at kid, in chars */
  var bmsize: Int = _;     /* bitmap size, based on max char code */
  var index: Int = _;      /* index into class list */
  var sense: Boolean = _;

}

class RESeqNode(op: Byte) extends RENode(op) {

  override def toString = op match {
    case Rhino.REOP_FLAT => if (flatIndex == -1) chr.toString else "F" + flatIndex + "+" + length
    case _ => super.toString
  }


                  /* or a literal sequence */
  var chr: Char = _;        /* of one character */
  var length: Int = _;     /* or many (via the index) */
  var index: Int = _;
  var flatIndex: Int = _;  /* which is -1 if not sourced */

}






class CompilerState(var cx: Context, var source: Array[Char], var length: Int, var flags: Int) {

    //var cx: Context;
    var cpbegin: Array[Char] = source
    var cpend: Int = length;
    var cp: Int = 0;
    //var flags: Int;
    var parenCount: Int = 0;
    var parenNesting: Int = _;
    var classCount: Int = 0;   /* number of [] encountered */
    var progLength: Int = 0;   /* estimated bytecode length */
    var result: RENode = _;
}

class REProgState
{
    def this( previous: REProgState, min: Int, max: Int, index: Int,
                 backTrack: REBackTrackData,
                continuationOp: Int,  continuationPc: Int) =
    {
        this()
        this.previous = previous;
        this.min = min;
        this.max = max;
        this.index = index;
        this.continuationOp = continuationOp;
        this.continuationPc = continuationPc;
        this.backTrack = backTrack;
    }

    var  previous: REProgState = _; // previous state in stack

    var min: Int = _;                      /* current quantifier min */
    var max: Int = _;                      /* current quantifier max */
    var index: Int = _;                    /* progress in text */
    var continuationOp: Int = _;
    var continuationPc: Int = _;
    var backTrack: REBackTrackData = _; // used by ASSERT_  to recover state
}

class REBackTrackData {

    def this(gData: REGlobalData, op: Int, pc: Int, cp: Int,
                    continuationOp: Int, continuationPc: Int) =
    {
        this()
        previous = gData.backTrackStackTop;
        this.op = op;
        this.pc = pc;
        this.cp = cp;
        this.continuationOp = continuationOp;
        this.continuationPc = continuationPc;
        parens = gData.parens;
        stateStackTop = gData.stateStackTop;
    }

    var  previous: REBackTrackData = _;

    var op: Int = _;                             /* operator */
    var pc: Int = _;                             /* bytecode pointer */
    var cp: Int = _;                             /* char buffer index */
    var continuationOp: Int = _;                 /* continuation op */
    var continuationPc: Int = _;                 /* continuation pc */
    var parens: Array[Long] = _;                      /* parenthesis captures */
    var stateStackTop: REProgState = _;          /* state of op that backtracked */
}

class REGlobalData {
    var multiline: Boolean = _;
    var regexp: RECompiled = _;              /* the RE in execution */
    var skipped: Int = _;                    /* chars skipped anchoring this r.e. */

    var cp: Int = _;                         /* char buffer index */
    var parens: Array[Long] = _;                  /* parens captures */

    var  stateStackTop: REProgState = _;       /* stack of state of current ancestors */

    var backTrackStackTop : REBackTrackData  = _;  /* last matched-so-far position */


    override def toString = "REGlobalData(" + skipped + "," + cp + " / " + 
      (if (parens ne null) parens.map(l => ((l >>> 32).toInt, (l & 0xFFFFFFFF).toInt)).mkString(",") else null) + ")"

    def groups(input: String) = Array(input.substring(skipped,cp)) ++ (if (parens ne null) parens.map { l => 
        val (len, start) = ((l >>> 32).toInt, (l & 0xFFFFFFFF).toInt)
        if (start >= 0 && len >= 0) input.substring(start, start + len) else ""
    } else Array[String]())


    /**
     * Get start of parenthesis capture contents, -1 for empty.
     */
    def parensIndex(i: Int): Int = 
    {
        return (parens(i)).toInt;
    }

    /**
     * Get length of parenthesis capture contents.
     */
    def parensLength(i: Int): Int =
    {
        return (parens(i) >>> 32).toInt;
    }

    def setParens(i: Int, index: Int, length: Int): Unit = 
    {
        // clone parens array if it is shared with backtrack state
        if (backTrackStackTop != null && backTrackStackTop.parens == parens) {
            parens = parens.clone();
        }
        parens(i) = (index & 0xffffffffL) | (length.toLong << 32);
    }

}


/*
 * This struct holds a bitmap representation of a class from a regexp.
 * There's a list of these referenced by the classList field in the NativeRegExp
 * struct below. The initial state has startIndex set to the offset in the
 * original regexp source of the beginning of the class contents. The first
 * use of the class converts the source representation into a bitmap.
 *
 */
final class RECharSet(val length: Int, val startIndex: Int, val strlength: Int, val sense: Boolean) //extends Serializable
{

    @volatile @transient var converted: Boolean = _;
    @volatile @transient var bits: Array[Byte] = _;
}

