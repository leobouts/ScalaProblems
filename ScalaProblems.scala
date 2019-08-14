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

  def kthElementFunctionally[A](n: Int, ls: List[A]): A = (n,ls) match {
    case (0, h :: _ ) => h
    case (n, _ :: tail) => kthElementFunctionally(n - 1,tail)
    case(_, Nil) => throw new NoSuchElementException
  }

  def countElementsInListFunctionally[A](ls: List[A]): Int = ls match {
    case Nil => 0
    case _ :: tail => 1 + countElementsInListFunctionally(tail)
  }

  def reverseListFuntcionally[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case h :: tail => reverseListFuntcionally(tail) ::: List(h)
  }


//  println(getLastElementFunctionally(List(1,2,3,4,5)))
//  println(getPenultimateFunctionally(List(1,2,3,4,5)))
//  println(kthElementFunctionally(3,List(1,2,3,4,5)))
//  println(countElementsInListFunctionally(List(1,2,3,69,4,5)))
  println(reverseListFuntcionally(List(1,2,3,4,56)))
}
