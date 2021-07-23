package io.github.noeppi_noeppi.tools.sourcetransform.inspect.value

import io.github.noeppi_noeppi.tools.sourcetransform.util.CommonParsers

import scala.annotation.tailrec

// Uses BigDecimal as it needs to cover both longs and doubles
case class NumberRange(min: Option[NumberBound], max: Option[NumberBound], inverted: Boolean) {
  
  def isInside(n: Number): Boolean = if (inverted) !isInsideRaw(n) else isInsideRaw(n)
  private def isInsideRaw(n: Number): Boolean = {
    val bd = n match {
      case x: BigDecimal => x
      case x => BigDecimal(x.toString)
    }
    min match {
      case Some(bound) if bound.inclusive && bound.value > bd => return false
      case Some(bound) if !bound.inclusive && bound.value >= bd => return false
      case _ =>
    }
    max match {
      case Some(bound) if bound.inclusive && bound.value < bd => return false
      case Some(bound) if !bound.inclusive && bound.value <= bd => return false
      case _ =>
    }
    true
  }
  
  def string(): String = {
    val minBound = min match {
      case Some(bound) => (if (bound.inclusive) "[" else "(") + bound.value.toString()
      case None => "("
    }
    val maxBound = min match {
      case Some(bound) => bound.value.toString() + (if (bound.inclusive) "]" else ")")
      case None => ")"
    }
    (if (inverted) "!" else "") + minBound + ";" + maxBound
  }
  
  def transform(func: BigDecimal => BigDecimal, fallingFunction: Boolean): NumberRange = {
    if (min.isEmpty && max.isEmpty) {
      this
    } else if (min.isDefined && max.isDefined) {
      val bound1 = NumberBound(func(min.get.value), min.get.inclusive)
      val bound2 = NumberBound(func(max.get.value), max.get.inclusive)
      if (bound1.value <= bound2.value) {
        NumberRange(Some(bound1), Some(bound2), inverted)
      } else {
        NumberRange(Some(bound2), Some(bound1), inverted)
      }
    } else if (min.isDefined) {
      val bound = NumberBound(func(min.get.value), min.get.inclusive)
      if (fallingFunction) {
        NumberRange(None, Some(bound), inverted)
      } else {
        NumberRange(Some(bound), None, inverted)
      }
    } else {
      val bound = NumberBound(func(max.get.value), max.get.inclusive)
      if (fallingFunction) {
        NumberRange(Some(bound), None, inverted)
      } else {
        NumberRange(None, Some(bound), inverted)
      }
    }
  }
}

object NumberRange {
  
  def read(str: String): NumberRange = Parsers.parseIt(Parsers.line, str).getOrElse(throw new IllegalStateException("Number range expected, got empty string."))

  @tailrec
  def overlap(range1: NumberRange, range2: NumberRange): Boolean = {
    if (range1.inverted && range2.inverted) {
      overlap(range2, range1)
    } else if (!range1.inverted && range2.inverted) {
       if (range1.min.isEmpty && range1.max.isEmpty) {
         true
       } else if (range1.min.isDefined && range1.max.isDefined) {
         range2.isInside(range1.min.get.value) || range2.isInside(range1.max.get.value)
       } else if (range1.min.isEmpty) {
         range2.min.isEmpty || range2.isInside(range1.max.get.value)
       } else {
         range2.max.isEmpty || range2.isInside(range1.min.get.value)
       }
    } else if (!range1.inverted && !range2.inverted) {
      if (range1.min.isEmpty && range2.min.isEmpty) {
        true
      } else if (range1.max.isEmpty && range2.max.isEmpty) {
        true
      } else {
        if ((range1.min.isDefined && range2.isInside(range1.min.get.value))
          || (range1.max.isDefined && range2.isInside(range1.max.get.value))) {
          true
        } else if ((range2.min.isDefined && range1.isInside(range2.min.get.value))
          || (range2.max.isDefined && range1.isInside(range2.max.get.value))) {
          true
        } else {
          false
        }
      }
    } else {
      true
    }
  }
  
  
  
  private object Parsers extends CommonParsers {

    def line: Parser[NumberRange] = range | failure("Number range expected")
    
    def range: Parser[NumberRange] = opt("!") ~ raw_range ^^ { case inverted ~ range => if (inverted.isDefined) NumberRange(range.min, range.max, !range.inverted) else range }
    def raw_range: Parser[NumberRange] = simple_true | simple_false | simple_number | full_range
    
    def simple_true: Parser[NumberRange] = "true" ^^ (_ => NumberRange(Some(NumberBound(0, inclusive = true)), Some(NumberBound(0, inclusive = true)), inverted = true))
    def simple_false: Parser[NumberRange] = "false" ^^ (_ => NumberRange(Some(NumberBound(0, inclusive = true)), Some(NumberBound(0, inclusive = true)), inverted = false))
    def simple_number: Parser[NumberRange] = floatingPointNumber ^^ (x => NumberRange(Some(NumberBound(BigDecimal(x), inclusive = true)), Some(NumberBound(BigDecimal(x), inclusive = true)), inverted = false))
    
    def full_range: Parser[NumberRange] = inclusion1 ~ opt(floatingPointNumber) ~ ";" ~ opt(floatingPointNumber) ~ inclusion2 ^^ { case inclusion1 ~ min ~ _ ~ max ~ inclusion2 => NumberRange(min.map(str => NumberBound(BigDecimal(str), inclusion1)), max.map(str => NumberBound(BigDecimal(str), inclusion2)), inverted = false) }
    def inclusion1: Parser[Boolean] = ("[" ^^ (_ => true)) | ("(" ^^ (_ => false))
    def inclusion2: Parser[Boolean] = ("]" ^^ (_ => true)) | (")" ^^ (_ => false))
  }
}

case class NumberBound(value: BigDecimal, inclusive: Boolean)

