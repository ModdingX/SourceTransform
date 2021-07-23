package io.github.noeppi_noeppi.tools.sourcetransform.inspect.value

trait Contract[T] {

  def testStrict(value: T): Boolean
  def test(value: T): Boolean
}
