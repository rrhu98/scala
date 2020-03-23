import java.security.KeyStore.TrustedCertificateEntry

object problem3 extends App {

  def unfold[A, B] (p: A => Boolean, h: A => B, t: A => A) (x: A) : List[B] =
  if (p(x))
    Nil
  else
    h(x) :: unfold(p, h, t) (t(x))

  def int2bin (i: Int): List[Int] = {
    def int2revBin: Int => List[Int] = {
      unfold[Int,Int](_==0,_%2,_/2)
    }
    val l1 = int2revBin(i)
    l1.reverse
  }

  def repHalve[A]: List[A] => List[List[A]] = {
    def takeHalf (l: List[A]): List[A] = {
      l.take(l.length/2+1)
    }
    def dropHalf (l:List[A]): List[A] = {
      l.drop(l.length/2+1)
    }
    unfold[List[A], List[A]](_.isEmpty,takeHalf(_), dropHalf(_))
  }

  val testL1 = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  val testL2 = List.range(1,51)
  val testL3 = List(1,4,6)
  val testL4 = List(1,2,3,4,5,6,7,8)
  println("Integer 20 in Binary is: ", int2bin(20))
  println("Integer 10 in Binary is: ", int2bin(10))
  println("Integer 30 in Binary is: ", int2bin(30))
  println("Integer 57 in Binary is: ", int2bin(57))
  println("Integer 0 in Binary is: ", int2bin(114))
  println("RepHalve Test1 is: ", repHalve(testL1))
  println("RepHalve Test2 is: ", repHalve(testL2))
  println("RepHalve Test3 is: ", repHalve(testL3))
  println("RepHalve Test4 is: ", repHalve(testL4))
}
