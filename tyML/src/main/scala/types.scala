package tyML

sealed trait Type

object Type {
  type Var = Int

  case object INT extends Type
  case object BOOL extends Type
  case class FUN(pTy: Type, rTy: Type) extends Type
  case class VAR(tv: Type.Var) extends Type

  private var counter = 0

  def freshVar(): Type = {
    counter += 1
    VAR(counter)
  }
}
