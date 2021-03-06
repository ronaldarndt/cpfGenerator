package generator

import java.io.{File, PrintWriter}

import scala.annotation.tailrec
import scala.util.Random

object Main extends App {
  def isCommonInvalid(n: String) = n.length != 11 || n.sorted == n

  def normalize(n: Int) = if (n == 10) 0 else n

  def random() = Random.between(min, max - maxIt)

  @tailrec
  def applyTransform(n: String, s: Int, until: Int = 2, offset: Int = -1): Int = n.length match {
    case `until` => normalize((s * 10) % 11)
    case _ => applyTransform(
      n.tail,
      n.head.toString.toInt * (n.length + offset) + s,
      until,
      offset
    )
  }

  def validate(n: String): Boolean = {
    if (isCommonInvalid(n)) {
      return false
    }

    val p = applyTransform(n, 0)

    if (p.toString != n.charAt(9).toString) {
      return false
    }

    val s = applyTransform(n, 0, 1, 0)

    s.toString == n.charAt(10).toString
  }

  def generate(n: Long, i: Long = 0): Long = {
    val (it, nn): (Long, Long) = if (i < maxIt) (i, n + i) else (0, random())

    val str = nn.toString
    val correctSum = argsInstance.sum < 0 || str.map(_.asDigit).sum == argsInstance.sum

    if (correctSum && validate(str)) {
      return n
    }

    generate(nn, it + 1)
  }

  def print(v: Long): Unit = {
    println(v)

    if (pw.isDefined)
      pw.value.write(v.toString + "\n")
  }

  case class AppArgs(sum: Int, amount: Int, file: String) {
    def saveToFile() = file != ""
  }

  object AppArgs {
    def default = new AppArgs(-1, 5, "./cpfs.txt")
  }

  val argsInstance = args.sliding(2, 1).toList.foldLeft(AppArgs.default) {
    case (accumArgs, currArgs) => currArgs match {
      case Array("--sum", sum) if sum forall Character.isDigit => accumArgs.copy(sum = sum.toInt)
      case Array("--amount", amount) if amount forall Character.isDigit => accumArgs.copy(amount = amount.toInt)
      case Array("--file", file) => accumArgs.copy(file = file)
      case Array("--help", _) => {
        println("Usage: sbt run [--sum 22] [--amount 5] [--file \"c:/cpfs/vals.txt\"]")

        accumArgs
      }
      case _ => accumArgs
    }
  }

  val min = 100000000L
  val max = 99999999999L
  val maxIt = 1000000

  val f = new File(argsInstance.file)

  if (argsInstance.saveToFile) {
    f.delete()
    f.createNewFile()
  }

  val pw = if (argsInstance.saveToFile) Some(new PrintWriter(f)) else Some(null)

  for (_ <- 1 to argsInstance.amount) {
    val v = generate(random())

    print(v)
  }

  if (pw.isDefined) {
    pw.value.close()
  }
}

