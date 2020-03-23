object problem2 extends App{

  sealed trait Tree[+A]
  case object Nil extends Tree[Nothing]
  case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

  def inOrder[A](tr: Tree[A]): List[A] =
    tr match {
      case Nil => List()
      case Node(v,l,r) => inOrder(l):::List(v):::inOrder(r)
    }

  def preOrder[A](tr: Tree[A]): List[A] =
    tr match {
      case Nil => List()
      case Node(v,l,r) => List(v):::preOrder(l):::preOrder(r)
    }

  def postOrder[A](tr: Tree[A]): List[A] =
    tr match {
      case Nil => List()
      case Node(v,l,r) => postOrder(l):::postOrder(r):::List(v)
    }

  def search[A](tr:Tree[A], k: A): Boolean =
    tr match  {
      case Nil => false
      case Node(v,l,r) =>
        if (v==k)
          true
        else
          search(l,k) || search(r,k)
    }

  def replace[A](tr:Tree[A], bef: A, aft: A):Tree[A] =
    tr match {
      case Nil => Nil
      case Node(v,l,r) =>
        if (v==bef)
          Node(aft, replace(l,bef,aft), replace(r,bef,aft))
        else
          Node(v, replace(l,bef,aft), replace(r,bef,aft))
    }


  val testTree = Node(1, Node(2,Nil,Nil), Node(3,Nil,Nil))

  println("Testing make tree: ", testTree)
  println("inOrder traversal of test tree: ", inOrder(testTree))
  println("preOrder traversal of test tree: ", preOrder(testTree))
  println("postOrder traversal of test tree: ", postOrder(testTree))
  println("Testing replace: ", replace(testTree, 1, 2))
  println("Testing replace: ", replace(testTree, 3, 1))



}
