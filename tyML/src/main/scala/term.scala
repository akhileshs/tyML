package tyML

// untyped AST

sealed trait Term

case class INT(value: Int) extends Term
case class BOOL(value: Boolean) extends Term
case class FUN(param: String, body: Term) extends Term
case class VAR(name: String) extends Term
case class APP(func: Term, arg: Term) extends Term
case class IF(test: Term, tr: Term, fls: Term) extends Term
