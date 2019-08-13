object ScalaProblems extends App{

  def getLastElementFunctionally[A](ls: List[A]): A = ls match {
    case h :: Nil => h
    case _ :: tail => getLastElementFunctionally(tail)
    case _ => throw new NoSuchElementException
  }
  
}
