package tyML

object Annotate {
  def annotate(term: Term, tyEnv: TypeEnv): TypedTerm = {
    term match {
      case INT(value) => TypedTerm.INT(Type.freshVar(), value)
      case BOOL(value) => TypedTerm.BOOL(Type.freshVar(), value)
      case FUN(arg, body) =>
        val argTy = Type.freshVar()
        val argBinder = TypedTerm.Binder(argTy, arg)
        val extendedTEnv = tyEnv.set(arg, argTy)
        TypedTerm.FUN(Type.freshVar(), argBinder, annotate(body, extendedTEnv))
      case Var(name) =>
        tyEnv.get(name) match {
          case None => throw new Exception("unbound identifier")
          case Some(ty) => TypedTerm.VAR(ty, name)
        }
      case APP(func, arg) =>
        TypedTerm.APP(Type.freshVar(), annotate(func, tyEnv), annotate(arg, tyEnv))
      case IF(test, thenBranch, elseBranch) =>
        TypedTerm.IF(
          Type.freshVar(),
          annotate(test, tyEnv),
          annotate(thenBranch, tyEnv),
          annotate(elseBranch, tyEnv))
      case LET(binding, value, body) =>
        val bindingTy = Type.freshVar()
        val extendedEnv = tyEnv.set(binding, bindingTy)
        TypedTerm.LET(
          Type.freshVar(),
          TypedTerm.Binder(bindingTy, binding),
          annotate(value, tyEnv),
          annotate(body, extendedEnv))
    }
  }
}
