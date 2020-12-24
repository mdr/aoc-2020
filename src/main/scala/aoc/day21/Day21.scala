package aoc.day21


import aoc.day21.Day21.{Allergen, Ingredient}
import aoc.util.Util
import aoc.util.Util.{SumByOperation, iterateUntilSteadyState}


case class FoodDescription(ingredients: Seq[Ingredient], allergens: Seq[Allergen]) {
  def constraints: Seq[Constraint] = allergens.map(Constraint(_, ingredients.toSet))
}

object FoodDescription {

  def parse(line: String): FoodDescription = {
    val Array(ingredientsPart, allergensPart) = line.init.split(" \\(contains ")
    val ingredients = ingredientsPart.split(" ").toSeq
    val allergens = allergensPart.split(", ").toSeq
    FoodDescription(ingredients, allergens)
  }

}

case class Constraint(allergen: Allergen, possibleIngredients: Set[Ingredient])

case class SolverState(options: Map[Allergen, Set[Ingredient]], resolved: Map[Allergen, Ingredient] = Map.empty) {

  def solve: SolverState = {
    val newlyResolved =
      for {
        (allergen, ingredients) <- options
        if ingredients.size == 1
        ingredient <- ingredients
      } yield allergen -> ingredient
    val newlyResolvedIngredients = newlyResolved.values.toSet
    val newOptions =
      for {
        (allergen, ingredients) <- options
        newIngredients = ingredients -- newlyResolvedIngredients
        if newIngredients.nonEmpty
      } yield allergen -> newIngredients
    copy(options = newOptions, resolved = resolved ++ newlyResolved)
  }

  def allResolved: Boolean = options.isEmpty

}

object Day21 extends App {

  type Ingredient = String
  type Allergen = String

  def solve(inputPath: String): Unit = {
    val foods = Util.loadLines(inputPath).map(FoodDescription.parse)
    val constraints = foods.flatMap(_.constraints)
    val initialOptions = constraints.groupMapReduce(_.allergen)(_.possibleIngredients)(_ intersect _)

    val initialSolverState = SolverState(initialOptions)
    val finalSolverState = iterateUntilSteadyState(initialSolverState)(_.solve)
    if (!finalSolverState.allResolved) throw new AssertionError("Not all resolved")
    val dangerousIngredients = finalSolverState.resolved.values.toSet
    val allIngredients = foods.flatMap(_.ingredients).toSet
    val safeIngredients = allIngredients -- dangerousIngredients
    val safeOccurrences = foods.sumBy(_.ingredients count safeIngredients)

    println(safeOccurrences)
    println(finalSolverState.resolved.toSeq.sortBy(_._1).map(_._2).mkString(","))
  }

  println("Part One")
  solve("day21/example.txt")
  solve("day21/puzzle.txt")

}