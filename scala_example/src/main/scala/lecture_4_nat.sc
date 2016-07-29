/**
  * Created by min on 2016-07-25.
  */
//Peano Numbers
abstract class Nat {
  def isZero : Boolean
  def predecessor : Nat
  def successor = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat{
  def isZero = true
  def predecessor = throw new Error("0.predecessor")
  def + (that:Nat) = that
  def - (that:Nat) = if (that.isZero) this else throw new Error("negative number")
}

class Succ (n:Nat) extends Nat{
  def isZero = false
  def predecessor = n // +1 된 값이니까  n이 전임자
  def +(that:Nat) = new Succ(n + that) //-1을 해야하니까 new Succ로 감
  def -(that:Nat) = if(that.isZero) this else n - that.predecessor
  // new Succ로 묶으면 zero에 닿으면 Exception이 나기 떄문에
  //새로운 방법으로한다. n은 parameter보다 1 작고, that.predecessor도 argument보다 1 작으니까 상쇄
  //
}

//val a = new Succ(Zero)
//println(a.isZero)
//println(a.predecessor)