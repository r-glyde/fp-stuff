package fp.instances

import fp.Monoid

object string extends string

trait string {

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override def zero: String = ""
    override def combine(x: String, y: String): String = x + y
  }

}
