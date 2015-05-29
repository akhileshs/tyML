package tyML

case class Constraint(a: Type, b: Type)

object Constraint {
  import TypedTerm._

  def collect(tyTerm: TypedTerm): Set[Constraint] = {
    tyTerm match {
      case INT(ty, value) => Set(Constraint(ty, Type.INT))
      case BOOL(ty, value) => Set(Constraint(ty, Type.BOOL))
      case FUN(ty, arg, body) =>
        collect(body) ++ Set(Constraint(ty, Type.FUN(arg.ty, body.ty)))
      case VAR(ty, name) => Set.empty
      case APP(ty, func, arg) =>
        collect(func) ++ collect(arg) ++ Set(
          Constraint(func.ty, Type.FUN(arg.ty, ty)))
      case IF(ty, test, thenBranch, elseBranch) =>
        collect(test) ++ collect(thenBranch) ++ collect(elseBranch) ++ Set(
          Constraint(test.ty, Type.BOOL),
          Constraint(thenBranch.ty, ty),
          Constraint(elseBranch.ty, ty))
      case LET(ty, binding, value, body) =>
        collect(value) ++ collect(body) ++ Set(
          Constraint(ty, body.ty),
          Constraint(binding.ty, value.ty))
    }
  }
}
