package io.github.noeppi_noeppi.tools.sourcetransform.inspect.value

sealed trait NumberContract extends Contract[NumberRange] {

  def testStrict(range: NumberRange): Boolean
  def test(range: NumberRange): Boolean
  
  protected def transform(func: BigDecimal => BigDecimal, fallingFunction: Boolean): NumberContract
}

object NumberContract {
  
  val never: NumberContract = new NumberContract {
    override def testStrict(range: NumberRange): Boolean = false
    override def test(range: NumberRange): Boolean = false
    override protected def transform(func: BigDecimal => BigDecimal, fallingFunction: Boolean): NumberContract = this
  }
  
  def not(contract: NumberContract): NumberContract = new NumberContract {
    override def testStrict(range: NumberRange): Boolean = !contract.testStrict(range)
    override def test(range: NumberRange): Boolean = !contract.test(range)
    override protected def transform(func: BigDecimal => BigDecimal, fallingFunction: Boolean): NumberContract = not(contract.transform(func, fallingFunction))
  }
  
  def and(contracts: NumberContract*): NumberContract = new NumberContract {
    override def testStrict(range: NumberRange): Boolean = contracts.forall(_.testStrict(range))
    override def test(range: NumberRange): Boolean = contracts.forall(_.test(range))
    override protected def transform(func: BigDecimal => BigDecimal, fallingFunction: Boolean): NumberContract = and(contracts.map(_.transform(func, fallingFunction)): _*)
  }
  
  def or(contracts: NumberContract*): NumberContract = new NumberContract {
    override def testStrict(range: NumberRange): Boolean = contracts.exists(_.testStrict(range))
    override def test(range: NumberRange): Boolean = contracts.exists(_.test(range))
    override protected def transform(func: BigDecimal => BigDecimal, fallingFunction: Boolean): NumberContract = or(contracts.map(_.transform(func, fallingFunction)): _*)
  }
  
  def neg(contract: NumberContract): NumberContract = contract.transform(-_, fallingFunction = true)
  def add(contract: NumberContract, value: BigDecimal): NumberContract = contract.transform(_ + value, fallingFunction = false)
  def mul(contract: NumberContract, value: BigDecimal): NumberContract = contract.transform(_ * value, fallingFunction = value < 0)
  
  def number(value: Number): NumberContract = new NumberContract {
    override def testStrict(range: NumberRange): Boolean = range.isInside(value)
    override def test(range: NumberRange): Boolean = range.isInside(value)
    override protected def transform(func: BigDecimal => BigDecimal, fallingFunction: Boolean): NumberContract = number(func(BigDecimal(value.toString)))
  }
  
  def range(value: NumberRange, strict: Boolean): NumberContract = new NumberContract {
    override def testStrict(range: NumberRange): Boolean = strict && test(range)
    override def test(range: NumberRange): Boolean = NumberRange.overlap(value, range)
    override protected def transform(func: BigDecimal => BigDecimal, fallingFunction: Boolean): NumberContract = range(value.transform(func, fallingFunction), strict)
  }
}
