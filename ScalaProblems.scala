object ScalaProblems extends App{

  def getLastElementFunctionally[A](ls: List[A]): A = ls match {
    case h :: Nil => h
    case _ :: tail => getLastElementFunctionally(tail)
    case _ => throw new NoSuchElementException
  }

  def getPenultimateFunctionally[A](ls: List[A]): A = ls match {
    case h :: _ :: Nil => h
    case _ :: tail => getPenultimateFunctionally(tail)
    case _ => throw new NoSuchElementException
  }

  println(getLastElementFunctionally(List(1,2,3,4,5)))
  println(getPenultimateFunctionally(List(1,2,3,4,5)))
}
