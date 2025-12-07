import io.StdIn.readLine
import util.Try
import util.chaining._

def getInput: String = Iterator
    .continually(readLine)
    .takeWhile(_ != null)
    .mkString("\n")

type Operator = Long => Long => Long

def sToOp(s: String): Operator = s match
    case "+" => (a) => (b) => a + b
    case "*" => (a) => (b) => a * b
    case _ => throw RuntimeException("unknown operator" + s)

def parse(s: String): Array[(Array[Long], Operator)] =
    s.linesIterator.map(line => line.split(' ').filter(_ != "")).toArray.transpose.map(p =>
        val len = p.length
        val numbers = p.take(len - 1)
        (numbers.map(s => util.Try(s.toLong).get), sToOp(p(len - 1)))
    )

def solve(arr: Array[(Array[Long], Operator)]): Long =
    arr.map((nums, op) => nums.reduce(Function.uncurried(op))).sum

@main def main() = println(getInput.pipe(parse).pipe(solve))
