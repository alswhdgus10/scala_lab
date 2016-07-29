trait Expr
case class Number(n:Int) extends Expr
case class Sum(e1:Expr, e2:Expr) extends Expr
case class Prod(e1:Expr, e2:Expr) extends Expr
case class Var(x:Char) extends Expr

object exprs {
  def show (e:Expr) : String = e match {
    case Number(x) => x.toString
    case Sum(l,r) => show(l) + " + " + show(r)
    case Prod(e1,e2) =>
      val st1 = e1 match {
        case Sum(a,b) => "(" + show(Sum(a,b)) + ")"
        case _ => show(e1)
      }
      val st2 = e2 match {
        case Sum(a,b) => "(" + show(Sum(a,b)) + ")"
        case _ => show(e2)
      }
      st1 + "*" + st2
    case Var(x) => x.toString
  }
}

exprs.show(Sum(Number(1),Number(44)))
exprs.show(Prod(Sum(Number(2),Number(1)),Number(4)))
exprs.show(Prod(Sum(Number(2),Number(1)),Sum(Number(9),Number(6))))


exprs.show(Prod(Sum(Number(2),Var('x')),Var('y')))
exprs.show(Sum(Prod(Number(2),Var('x')),Var('y')))