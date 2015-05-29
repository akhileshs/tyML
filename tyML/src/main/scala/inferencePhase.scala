package tyML

object Infer {
  val tyenv = TypeEnv(Map(
    "+" -> Type.FUN(Type.INT, Type.FUN(Type.INT, Type.INT)),
    "-" -> Type.FUN(Type.INT, Type.FUN(Type.INT, Type.INT))))

  def typeOf(term: Term): Type = {
    val typedTerm = Annotate.annotate(term, tyenv)
    val constraints = Constraint.collect(typedTerm)
    val subst = Unifier.unify(constraints)
    subst.apply(typedTerm.ty)
  }
}
