package tyML

case class TypeEnv(bindinds: Map[String, Type]) {
  def set(name: String, ty: Type): TypeEnv = {
    TypeEnv(bindinds + (name -> ty))
  }

  def get(name: String): Option[Type] = {
    bindings.get(name)
  }
}

