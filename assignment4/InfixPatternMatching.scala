package com.knoldus.assignment4

/**
  * Created by ashish on 18/1/17.
  */
class InfixPatternMatching {

  private var currentNumber:StringBuilder = new StringBuilder("")

  private var numberStack:List[String] = List()
  private var signStack:List[Char] = List()
  private var expr:String = ""
  private var currentPosition = 0

  def solveInfixExpression(expression : String): Int = {

    expr = expression.trim()
    val exprLength = expr.length
    var char:Char = expr.charAt(0)

    while(currentPosition < exprLength) {

      char match {

          case '*' | '/' | '%' => performHighPrecedOp(char)
          case '+' | '-' => performLowPrecedOp(char)
          case _ => accumulateNumber(currentPosition)

      }
      currentPosition += 1
      if(currentPosition < expr.length)
        char = expr(currentPosition).toChar
    }

    calculateLeftOver()

    numberStack(0).toInt
  }

  private def accumulateNumber(position:Int): Int = {

    val symbol = expr.charAt(position)

    symbol match {

      case '+' | '-' | '*'  | '/' | '%' =>{
        currentPosition -= 1
        currentNumber.toInt
      }
      case _ => {
        currentNumber.append(symbol)
        currentPosition += 1
        if(currentPosition < expr.length)
          accumulateNumber(currentPosition)
        else {
          currentPosition -= 1
          currentNumber.toInt
        }
      }
    }

  }

  private def performHighPrecedOp(sign:Char): Unit = {

    val numberOnTop = currentNumber.toInt

    val nextNumber = accumulateNumber(currentPosition+1)

    println(sign)

    sign match {
      case '*' => numberStack = (numberOnTop * nextNumber).toString :: numberStack
      case '/' => numberStack = (numberOnTop / nextNumber).toString :: numberStack
      case '%' => numberStack = (numberOnTop % nextNumber).toString :: numberStack
    }

    currentNumber = new StringBuilder("")
  }

  private def performLowPrecedOp(sign:Char) : Unit = {
    println(sign)
    numberStack = currentNumber.toString :: numberStack
    println(s"Number stack $numberStack")
    signStack = sign :: signStack
    println(s"Sign stack $signStack")

  }

  private def calculateLeftOver(): Unit = {
    var sign:Char = ' '
    var tempSum:Int = 0
    println(numberStack)

    while (numberStack.length > 1 ) {
      sign = signStack(0)
      println(sign)
      sign match {
        case '+' => tempSum = numberStack(0).toInt + numberStack(1).toInt
        case '-' => tempSum = numberStack(1).toInt - numberStack(0).toInt
      }

      if(numberStack.length > 2)
        numberStack = numberStack.slice(2, numberStack.length -1)
      else if(numberStack.length == 2)
        numberStack = numberStack.slice(1, numberStack.length -1)

      numberStack = tempSum.toString :: numberStack
      signStack = signStack.slice(1, signStack.length)
    }
  }
}

object InfixPatternMatching {
  def main(args: Array[String]) {

    val infixExpression = new InfixPatternMatching()
    val valueOfExpression = infixExpression.solveInfixExpression("7+9*2-1")
    println(valueOfExpression)

  }
}
