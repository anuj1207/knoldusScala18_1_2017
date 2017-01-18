package com.knoldus.assignment4

/**
  * Created by ashish on 18/1/17.
  */
class PaymentMethodProblem {
  def getFinalAmount(method:String,amount:Int):Double= {

    method.toLowerCase match{
      case "paytm" | "freecharge"=> (amount*102)/100
      case  "netbanking"=> amount + 5
      case "card payment" => amount + 1.5
      case _ => amount
    }
  }
}
//companion object

object PaymentMethodProblem {
  def main(args: Array[String]) {
    val paymentMethodProblem = new PaymentMethodProblem()
    println(paymentMethodProblem.getFinalAmount("Paytm",1000))
    println(paymentMethodProblem.getFinalAmount("Card Payment",6000))
    println(paymentMethodProblem.getFinalAmount("NetBanking",40000))
    println(paymentMethodProblem.getFinalAmount("Freecharge",5000))
  }
}