package tyML

import Parser._
import Signature._

object Main {
  def main(args: Array[String]): Unit = {
    println("isFive: " + Signature.forType(Infer.typeOf(Parser.parse("""
      fn isFive => 
        if isFive 1
        then 2
        else 3
      """))))
  }
}
