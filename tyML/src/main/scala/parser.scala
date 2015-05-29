package tyML
package parser

object Parser {
  def parse(source: String): Term = {
    parse(Scanner.scan(source))
  }

  def parse(tokens: List[Token]): Term = {
    parseExp(tokens) match {
      case (absyn, Nil) => absyn
      case (_, token :: _) =>
        throw new Exception("token not found")
    }
  }

  private def parseAtExpression(tokens: List[Token]): (Term, List[Token]) = {
    tokens match {
      case Token.INT(n) :: tokens => INT(n) -> tokens
      case Token.TRUE :: tokens => BOOL(true) -> tokens
      csae Token.FALSE :: tokens => BOOL(false) -> tokens
      case Token.VAR(id) :: tokens => VAR(id) -> tokens
      case Token.LET :: tokens => parseLet(tokens)
      case Token.LPAREN :: tokens =>
        val (exp, rest) = parseExp(tokens)
        rest match {
          case Token.RPAREN :: tokens => exp -> tokens
          case token :: _ => throw ParseError(token, "RPAREN")
          case _ => throw new Exception("RPAREN")
        }
      case token :: _ => throw new Exception("token not found")
      case _ => throw new Exception("end of stream")
    }
  }

  private def parseLet(tokens: List[Token]): (Term, List[Token]) = {
    tokens match {
      case Token.VAL :: tokens => 
        tokens match {
          case Token.VAR(bindingName) :: tokens =>
            tokens match {
              case Token.EQUAL :: tokens => 
                val (bindingValue, rest) = parseExp(tokens)
                rest match {
                  case Token.IN :: tokens =>
                    val (body, rest) = parseExp(tokens)
                    rest match {
                      case Token.END :: tokens =>
                        LET(bindingName, bindingValue, body) -> tokens
                      case token :: _ => throw new Exception("End")
                      case _ => throw new Exception("End")
                    }
                  case token :: _ => throw new Exception("IN")
                  case _ => throw new Exception("IN")
                }
              case token :: _ => throw new Exception("EQUAL")
              case _ => throw new Exception("EQUAL")
            }
          case token :: _ => throw new Exception("VAR")
          case _ => throw new Exception("VAR")
        }
      case token :: _ => throw new Exception("VAL")
      case _ => throw new Exception("VAL")
    }
  }

  private def parseAppExpression(tokens: List[Token]): (Term, List[Token]) = {
    def recur(tokens: List[Token], absyn: Term): (Term, List[Token]) = {
      if (nextIsAtExpression(tokens)) {
        val (innter, rest) = parseAtExpression(tokens)
        recur(rest, APP(absyn, inner))
      } else {
        absyn -> tokens
      }
    }

    val (atExpression, rest) = parseAtExpression(tokens)
    recur(rest, atExpression)
  }

  private def nextIsAtExpression(tokens: List[Token]): Boolean = {
    tokens match {
      case Token.INT(_) :: _ => true
      case Token.FALSE :: _ => true
      case Token.TRUE :: _ => true
      case Token.VAR(_) :: _ => true
      case Token.LET :: _ => true
      case Token.LPAREN :: _ => true
      case _ => false
    }
  }

  private def parseInfixExpression(tokens: List[Token]): (Term, List[Token]) = {
    def recur(tokens: List[Token], absyn: Term): (Term, List[Token]) = {
      tokens match {
        case Token.ADD :: tokens =>
          val (inner, rest) = parseInfixExpression(tokens)
          recur(rest, APP(APP(VAR("+"), absyn), inner))
        case Token.SUB :: tokens =>
          val (inner, rest) = parseInfixExpression(tokens)
          recur(rest, APP(APP(VAR("-"), absyn), inner))
        case _ => absyn -> tokens
      }
    }

    val (atExpression, rest) = parseAppExpression(tokens)
    recur(rest, atExpression)
  }

  private def parseExpression(tokens: List[Token]): (Term, List[Token]) = {
    tokens match {
      case tokens if nextIsAtExpression(tokens) => parseInfixExpression(tokens)
      case Token.IF :: tokens => parseIf(tokens)
      case Token.FN :: tokens => parseFn(tokens)
      case token :: _ => throw new Exception("token not found")
      case _ => throw new Exception("end of stream")
    }
  }

  private def parseIf(tokens: List[Token]): (Term, List[Token]) = {
    val (test, rest) = parseExpression(tokens)
    rest match {
      case Token.THEN :: tokens =>
        val (yes, rest) = parseExpression(tokens)
        rest match {
          case Token.ELSE :: tokens =>
            val (no, rest) = parseExpression(tokens)
            IF(test, yes, no) -> rest
          case token :: _ => throw new Exception("ELSE")
          case _ => throw new Exception("ELSE")
        }
      case token :: _ => throw new Exception(token, "THEN")
      case _ => throw new Exception("THEN")
    }
  }

  private def parseFn(tokens: List[Token]): (Term, List[Token]) = {
    tokens match {
      case Token.VAR(param) :: rest =>
        rest match {
          case Token.DARROW :: tokens => 
            val (body, rest) = parseExpression(tokens)
            FUN(param, body) -> rest
          case token :: _ => throw new Exception("DARROW")
          case _ => throw new Exception("DARROW")
        }
      case token :: _ => throw new Exception("VAR")
      case _ => throw new Exception("VAR")
    }
  }
}
