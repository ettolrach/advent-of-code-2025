import io.StdIn.readLine
import util.Try
import util.chaining._

def splitWhen[A](pred: (A) => Boolean, as: List[A]): List[List[A]] = as.dropWhile(pred) match
    case Nil => Nil
    case newas =>
        val (w, l) = newas.span((list) => !pred(list))
            w :: splitWhen(pred, l)

def splitOnEmpties(lArr: List[Array[Char]]): List[List[List[Char]]] =
    val l = lArr.map(_.toList)
    splitWhen((l) => l.forall(_ == ' '), l)

def getInput: String = Iterator
    .continually(readLine)
    .takeWhile(_ != null)
    .mkString("\n")

type Operator = Long => Long => Long

def sToOp(s: String): Operator = s match
    case "+" => (a) => (b) => a + b
    case "*" => (a) => (b) => a * b
    case _ => throw RuntimeException("unknown operator" + s)

def parseCalc(l: List[List[Char]]): (List[Long], Operator) =
    val operatorChar = l(0).last
    val nums = l.map(numList => util.Try(String(numList.toArray.init).trim().toLong).get)
    (nums, sToOp(String(Array(operatorChar))))

def parse(s: String): List[(List[Long], Operator)] =
    s.linesIterator.map(line => line.toCharArray()).toArray.transpose.toList.pipe(splitOnEmpties).map(parseCalc)

def solve(arr: List[(List[Long], Operator)]): Long =
    arr.map((nums, op) => nums.reduce(Function.uncurried(op))).sum

@main def main() = println(getInput.pipe(parse).pipe(solve))
