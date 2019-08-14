object ScalaProblems extends App{

  def getLastElement[A](ls: List[A]): A = ls match {
    case h :: Nil => h
    case _ :: tail => getLastElement(tail)
    case _ => throw new NoSuchElementException
  }

  def getPenultimateElement[A](ls: List[A]): A = ls match {
    case h :: _ :: Nil => h
    case _ :: tail => getPenultimateElement(tail)
    case _ => throw new NoSuchElementException
  }

  def FindKthElement[A](n: Int, ls: List[A]): A = (n,ls) match {
    case (0, h :: _ ) => h
    case (n, _ :: tail) => FindKthElement(n - 1,tail)
    case(_, Nil) => throw new NoSuchElementException
  }

  def countElementsInList[A](ls: List[A]): Int = ls match {
    case Nil => 0
    case _ :: tail => 1 + countElementsInList(tail)
  }

  def reverseList[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case h :: tail => reverseList(tail) ::: List(h)
  }

  def isPalindrome[A](ls: List[A]): Boolean = ls == ls.reverse

  def flattenNestedListsIntoOne(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flattenNestedListsIntoOne(ms)
    case e => List(e)
  }

  def eliminateDuplicates[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case h :: tail => h :: eliminateDuplicates(tail.dropWhile(_ == h))
  }

}
