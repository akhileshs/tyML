package tyML

// represents the "typed" AST
//
sealed trait TypedTerm {
  def ty: Type
}

object TypedTerm {
  case class Binder(ty: Type, name: String)
  case class INT(ty: Type, value: Int) extends TypedTerm
  case class BOOL(ty: Type, value: Boolean) extends TypedTerm
  case class FUN(ty: Type, arg: TypedTerm.Binder, body: TypedTerm) extends TypedTerm
  case class VAR(ty: Type, name: String) extends TypedTerm
  case class APP(ty: Type, func: TypedTerm, arg: TypedTerm) extends TypedTerm
  case class IF(ty: Type, test: TypedTerm, thenBranch: TypedTerm, elseBranch: TypedTerm) extends TypedTerm
  case class LET(ty: Type, binding: TypedTerm.Binder, value: TypedTerm, body: TypedTerm) extends TypedTerm
}
  
