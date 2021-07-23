package io.github.noeppi_noeppi.tools.sourcetransform.inspect

import io.github.noeppi_noeppi.tools.sourcetransform.inspect.value.NumberContract
import org.eclipse.jdt.core.dom._

import scala.jdk.CollectionConverters._

object NumberExprUtil {

  def getNumberContract(expr: Expression): NumberContract = {
    getConstantNumber(expr).map(n => NumberContract.number(n)).getOrElse(expr match {
      case e: BooleanLiteral if e.booleanValue() => NumberContract.not(NumberContract.number(0))
      case e: ConditionalExpression => NumberContract.or(getNumberContract(e.getThenExpression), getNumberContract(e.getElseExpression))
      case e: InfixExpression => infixG(e).orElse(infixC(e)).orElse(infixNC(e)).getOrElse(NumberContract.never)
      case e: ParenthesizedExpression => getNumberContract(e.getExpression)
      case e: PrefixExpression => prefix(e)
      case e: SwitchExpression => NumberContract.or(e.statements().asScala.toSeq.flatMap {
        case x: YieldStatement => Some(getNumberContract(x.getExpression))
        case x: ExpressionStatement => Some(getNumberContract(x.getExpression))
        case x: Expression => Some(getNumberContract(x))
        case _ => None
      }: _*)
      case _ => NumberContract.never
    })
  }
  
  def getConstantNumber(expr: Expression): Option[BigDecimal] = expr match {
    case e: BooleanLiteral if !e.booleanValue() => Some(BigDecimal(0))
    case e: CharacterLiteral => Some(BigDecimal(e.charValue()))
    case e: NumberLiteral => Some(BigDecimal(e.getToken))
    case _ => None
  }
  
  def infixG(expr: InfixExpression): Option[NumberContract] = {
    expr.getOperator match {
      case InfixExpression.Operator.AND => Some(NumberContract.and(getNumberContract(expr.getLeftOperand), getNumberContract(expr.getRightOperand)))
      case InfixExpression.Operator.OR => Some(NumberContract.or(getNumberContract(expr.getLeftOperand), getNumberContract(expr.getRightOperand)))
      case _ => None
    }
  }
  
  def infixC(expr: InfixExpression): Option[NumberContract] = {
    val (const, contract) = getConstantNumber(expr.getRightOperand).map(bd => (bd, getNumberContract(expr.getLeftOperand)))
      .orElse(getConstantNumber(expr.getLeftOperand).map(bd => (bd, getNumberContract(expr.getRightOperand))))
      .getOrElse((null, NumberContract.never))
    if (const == null) {
      None
    } else expr.getOperator match {
      case InfixExpression.Operator.PLUS => Some(NumberContract.add(contract, const))
      case InfixExpression.Operator.TIMES => Some(NumberContract.mul(contract, const))
      case _ => None
    }
  }
  
  def infixNC(expr: InfixExpression): Option[NumberContract] = {
    val (const, contract) = getConstantNumber(expr.getRightOperand).map(bd => (bd, getNumberContract(expr.getLeftOperand)))
      .getOrElse((null, NumberContract.never))
    if (const == null) {
      if (expr.getOperator == InfixExpression.Operator.MINUS) {
        val (const2, contract2) = getConstantNumber(expr.getLeftOperand).map(bd => (bd, getNumberContract(expr.getRightOperand)))
          .getOrElse((null, NumberContract.never))
        if (const2 != null && const2 == 0) {
          Some(NumberContract.neg(contract2))
        } else {
          None
        }
      } else {
        None
      }
    } else expr.getOperator match {
      case InfixExpression.Operator.MINUS => Some(NumberContract.add(contract, -const))
      case InfixExpression.Operator.DIVIDE => Some(NumberContract.mul(contract, 1 / const))
      case _ => None
    }
  }
  
  def prefix(expr: PrefixExpression): NumberContract = {
    expr.getOperator match {
      case PrefixExpression.Operator.PLUS => getNumberContract(expr.getOperand)
      case PrefixExpression.Operator.MINUS => NumberContract.neg(getNumberContract(expr.getOperand))
      case PrefixExpression.Operator.NOT => NumberContract.not(getNumberContract(expr.getOperand))
      case _ => NumberContract.never
    }
  }
}
