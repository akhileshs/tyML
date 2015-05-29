package tyML

object Unifier {
  def unify(constraints: Set[Constraint]): Substitution = {
    if (constraints.isEmpty) {
      Substitution.empty
    }
    else {
      val subst: Substitution = unifyOne(constraints.head)
      val substitutedTail = subst.apply(constraints.tail)
      val substTail: Substitution = unify(substitutedTail)
      subst.compose(substTail)
    }
  }

  def unifyOne(constraint: Constraint): Substitution = {
    (constraint.a, constraint.b) match {
      case (Type.INT, Type.INT) => Substitution.empty
      case (Type.BOOL, Type.BOOL) => Substitution.empty
      case (Type.FUN(arg1, return1), Type.FUN(arg2, return2)) =>
        unify(Set(
          Constraint(arg1, arg2),
          Constraint(return1, return2)))
      case (Type.VAR(tvar), ty) => unifyVar(tvar, ty)
      case (ty, Type.VAR(tvar)) => unifyVar(tvar, ty)
      case (x, y) => throw new Exception(s"can't unify $a with $b")
    }
  }

  def unifyVar(tvar: Type.Var, ty: Type): Substitution = {
    ty match {
      case Type.VAR(tvar2) if tvar == tvar2 => Substitution.empty
      case Type.VAR(_) => Substitution.fromPair(tvar, ty)
      case ty if occurs(tvar, ty) =>
        throw new Exception(s"$tvar already occurs in $ty")
      case ty => Substitution.fromPair(tvar, ty)
    }
  }

  def occurs(tvar: Type.Var, ty: Type): Boolean = {
    ty match {
      case Type.FUN(p, r) => occurs(tvar, p) || occurs(tvar, r)
      case Type.VAR(tvar2) => tvar == tvar2
      case _ => false
    }
  }
}

