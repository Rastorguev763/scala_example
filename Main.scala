object HelloWorld {
  def main(args: Array[String]): Unit = {
    println("Привет, мир!")
    println("Привет, Scala!".reverse)
    println("Привет, Scala!".toLowerCase())
    println("Привет, Scala!".stripSuffix("!"))
    println("Привет, Scala!".stripSuffix("!")+" - Python лучше!")
  }
}

object SalaryCalculator extends App {

  // Входные данные для первой задачи
  val annualIncome = 1000000
  val bonusPercentage = 10
  val mealCompensation = 5000

  // Рассчитываем ежемесячный оклад сотрудника после вычета налогов
  val monthlySalary = calculateMonthlySalary(annualIncome, bonusPercentage, mealCompensation)
  println(s"Ежемесячный оклад сотрудника: $monthlySalary руб.")

  // Входные данные для второй задачи
  val salariesList = List(100, 150, 200, 80, 120, 75)

  // Выводим результаты второй задачи
  salaryDeviations(salariesList, bonusPercentage, mealCompensation)

  // Входные данные для третьей задачи
  val goodBehaviorBonus = 5
  val badBehaviorPenalty = -2
  var employeeBehavedWell = true

  // Выводим данные для третьей задачи
  var newSalariesBehavior = updateSalaryBehavior(salariesList, goodBehaviorBonus, badBehaviorPenalty, employeeBehavedWell)
  var monthlySalariesBehavior = newSalariesBehavior.map(salary => calculateMonthlySalary(salary * 1000 * 12, bonusPercentage, mealCompensation))

  // Выводим самую большую и самую маленькую зарплату
  val maxSalary = monthlySalariesBehavior.max
  val minSalary = monthlySalariesBehavior.min
  println(f"Самая высокая зарплата: $maxSalary%.2f")
  println(f"Самая низкая зарплата: $minSalary%.2f")

  // Входные данные для четвертой задачи
  val newSalaryWorker1 = 350
  val newSalaryWorker2 = 90

  // Вывод данных для четвертой задачи
  var newSalaryAdded = addNewSalary(newSalariesBehavior, newSalaryWorker1, newSalaryWorker2)

  var newSalaryAddedMonthly = (newSalaryAdded.map(salary => calculateMonthlySalary(salary * 1000 * 12, bonusPercentage, mealCompensation)))

  newSalaryAddedMonthly.sorted
    .zipWithIndex
    .foreach { case (newSalary, index) =>
      println(f"Зарплата сотрудника ${index + 1}: $newSalary%.2f руб.")
    }

  // Входные данные для пятой задачи
  val newWorkerSalary = 130
  var idx = 1
  val newWorkerSalaryMonthly = calculateMonthlySalary(newWorkerSalary * 1000 * 12, bonusPercentage, mealCompensation)

  // Вывод данных для пятой задачи
  val indexPosition = newSalaryAddedMonthly.sorted.zipWithIndex.map { case (salary, i) =>
    if (newWorkerSalaryMonthly > salary) {
      idx += 1
    }
    i + 1 // Возвращаем порядковый номер элемента
  }

  if (idx <= newSalaryAddedMonthly.length) {
    println(s"Новая зарплата сотрудника находится на позиции $idx.")
  } else {
    println("Новая зарплата сотрудника не найдена в отсортированном списке.")
  }

  // Входные данные для шестой задачи
  val middleSalaryRange = 100000 to 150000
  val newSalaries = (newSalaryAddedMonthly :+ newWorkerSalaryMonthly).sorted

  // Вывод данных
  val middleEmployees = findMiddleEmployees(newSalaries, middleSalaryRange)
  println(s"Номера сотрудников с зарплатой в диапазоне от 100000 руб. до 150000 руб.: $middleEmployees")

  // Входные данные для седьмой задачи
  val inflationRate = 0.07
  val indexedSalaries = indexSalaries(newSalaries, inflationRate)
  indexedSalaries.zipWithIndex
    .foreach { case (newSalary, index) =>
      println(f"Зарплата сотрудника ${index + 1}: $newSalary%.2f руб.")
    }

  // Функции для выполнения задач

  // Задача 1: Рассчет ежемесячного оклада сотрудника после вычета налогов
  def calculateMonthlySalary(annualIncome: Int, bonusPercentage: Int, mealCompensation: Int): Double = {
    val taxableIncome = annualIncome - (annualIncome * 0.13) // Налог в размере 13%
    val bonusAmount = annualIncome * bonusPercentage / 100 // Вычисляем процент
    val totalIncome = taxableIncome + bonusAmount + mealCompensation
    totalIncome / 12 // Ежемесячный оклад
  }

  // Задача 2: Вывод отклонений окладов сотрудников от среднего значения
  def salaryDeviations(salaries: List[Int], bonusPercentage: Int, mealCompensation: Int): Unit = {
    val monthlySalaries = salaries.map(salary => calculateMonthlySalary(salary * 1000 * 12, bonusPercentage, mealCompensation))
    val averageSalary = monthlySalaries.sum / monthlySalaries.length
    val deviations = monthlySalaries.map(salary => (salary - averageSalary) / averageSalary * 100)
    println("Отклонения зарплат сотрудников от среднего значения:")
    deviations.zip(monthlySalaries).foreach { case (deviation, monthlySalary) =>
      println(f"Зарплата сотрудника $monthlySalary%.2f руб. с отклонением от средней запрлаты по отделу $averageSalary в $deviation%.2f%%")
    }
  }

  // Задача 3: Вывод окладов сотрудников с учетом поведения
  def updateSalaryBehavior(salaries: List[Int], goodBehaviorBonus: Int, badBehaviorPenalty: Int, employeeBehavedWell: Boolean): List[Int] = {
    val monthlySalaryBehavior = salaries.map { salary =>
      if (employeeBehavedWell) {
        salary + goodBehaviorBonus
      } else {
        salary + badBehaviorPenalty
      }
    }
    monthlySalaryBehavior
  }

  // Задача 4: В вашу команду пришли два специалиста с окладами 350 и 90 тысяч рублей.
  // Попробуйте отсортировать список сотрудников по уровню оклада от меньшего к большему.
  def addNewSalary(salaries: List[Int], newSalaryWorker1: Int, newSalaryWorker2: Int): List[Int] = {
    salaries :+ newSalaryWorker1 :+ newSalaryWorker2
  }

  // Задача 6: Поиск сотрудников с зарплатой в заданном диапазоне
  def findMiddleEmployees(salaries: List[Double], salaryRange: Range): List[Int] = {
    salaries.zipWithIndex.collect {
      case (salary, index) if salary > salaryRange.start && salary < salaryRange.end => index + 1
    }
  }

  // Задача 7: Индексация зарплат на уровень инфляции
  def indexSalaries(salaries: List[Double], inflationRate: Double): List[Double] = {
    salaries.map(salary => salary * (1 + inflationRate))
  }
}
