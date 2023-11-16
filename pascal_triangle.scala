object PascalTriangle {
  def generatePascalTriangle(numRows: Int): List[List[Int]] = {
    var triangle: List[List[Int]] = List.empty
    for (i <- 1 to numRows) {
      var currentRow: List[Int] = List(1)
      if (triangle.nonEmpty) {
        val previousRow: List[Int] = triangle.last
        for (elem <- 0 until previousRow.length - 1) {
          val newElem = previousRow(elem) + previousRow(elem + 1)
          currentRow = currentRow :+ newElem
        }
        currentRow = currentRow :+ 1
      }
      triangle = triangle :+ currentRow
    }
    triangle
  }
}

object PascalTrianglePrinter {
  def printPascalTriangle(triangle: List[List[Int]]): Unit = {
    for (row <- triangle) {
      val rowString = row.mkString(" ")
      val padding = " " * ((triangle.last.length - row.length) * 2)
      println(s"$padding$rowString$padding")
    }
  }

  def main(args: Array[String]): Unit = {
    val triangle = PascalTriangle.generatePascalTriangle(10)
    printPascalTriangle(triangle)
  }
}

object PascalTriangleValue {
  def getPascalValue(triangle: List[List[Int]], row: Int, col: Int): Option[Int] = {
    if (row < 0 || col < 0 || row >= triangle.length || col >= triangle(row).length) {
      None
    } else {
      Some(triangle(row)(col))
    }
  }

  def main(args: Array[String]): Unit = {
    val triangle = PascalTriangle.generatePascalTriangle(5)
    val row = 4
    val col = 2

    getPascalValue(triangle, row, col) match {
      case Some(value) =>
        println(s"Значение в строке №$row, елемент №$col в треугольнике Паскаля: $value")
      case None =>
        println(s"Не существует номера строки или номера элемента.")
    }
  }
}

object PascalTriangleSum {
  def sumRow(triangle: List[List[Int]], row: Int): Option[Int] = {
    if (row < 0 || row >= triangle.length) {
      None
    } else {
      Some(triangle(row).sum)
    }
  }

  def main(args: Array[String]): Unit = {
    val triangle = PascalTriangle.generatePascalTriangle(5)
    val row = 2

    sumRow(triangle, row) match {
      case Some(sum) =>
        println(s"Сумма элементов в строке №$row треугольника Паскаля: $sum")
      case None =>
        println(s"Не существует строки.")
    }
  }
}

object PascalTriangleSymmetry {
  def isSymmetric(triangle: List[List[Int]]): Boolean = {
    
    val lastRow = triangle.last
    if (lastRow.length % 2 != 0) return false
    
    val middleIndex = lastRow.length / 2

    lastRow.take(middleIndex) == lastRow.takeRight(middleIndex).reverse
  }

  def main(args: Array[String]): Unit = {
    val elements = 10
    val symmetricTriangle = PascalTriangle.generatePascalTriangle(elements)

    val isSymmetricResult = isSymmetric(symmetricTriangle)
    val answer = if (isSymmetricResult) "ДА" else "НЕТ"

    println(s"Треугольник Паскаля из $elements симметричен? - $answer")
  }
}
