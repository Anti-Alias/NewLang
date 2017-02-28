package newlang

/**
* Some location in a File
*/
case class Loc(lineNum:Int, colNum:Int)

/**
* Represents some Element in an AST
*/
trait Element
{
    /**
    * Location of the Element in the source file.
    */
    def loc:Loc
}

/**
* Represent some operation, be it arithmetic or boolean.
*/
trait Operation extends Element
{
    def left:Element
    def right:Element
}

/**
* Some operation using booleans.
*/
trait Condition extends Element

/**
* Represents an Element that does some sort of work.
*/
trait Execution extends Element

/**
* Some code that evaluates to something else.
*/
trait Evaluation extends Element
{
    def result:Any
}

/**
* Element that stores some literal value
* @param lineNum Line the Element was found on.
* @param colNumber Column the Element was found on.
* @param value Value of the LiteralElement
*/
case class Literal(loc:Loc, lineNum:Int, colNum:Int, result:Any) extends Evaluation

/**
* Represents some variable declaration.
* @param isMutable Mutability status
* @param assignment Assignment made in the Declaration.
*/
case class Declaration(loc:Loc, isMutable:Boolean, assignment:Assignment) extends Execution

/**
* Represent some variable assignment.
* @param varName Name of the variable being assigned.
* @param value Value the variable is being assigned to.
*/
case class Assignment(loc:Loc, varName:String, value:Evaluation) extends Execution

/**
* Represents a while statement.
* @param condition Condition while loop must satisfy to execute value.
* @param code Code to execute
*/
case class While(loc:Loc, condition:Condition, code:Execution) extends Execution

/**
* Represents an If statement.
* @param condition Condition If statement must satisfy in order for result evaluation to be used.
* @param result Result of this If statement.  Left should be used if condition is true.  Otherwise, Right.
*/
case class If(loc:Loc, condition:Condition, result:Either[Evaluation, Evaluation]) extends Evaluation

// Conditionals
case class EqualTo(loc:Loc, left:Element, right:Element) extends Condition
case class LessThan(loc:Loc, left:Element, right:Element) extends Condition
case class GreaterThan(loc:Loc, left:Element, right:Element) extends Condition
case class LessThanEqualTo(loc:Loc, left:Element, right:Element) extends Condition
case class GreaterThanEqualTo(loc:Loc, left:Element, right:Element) extends Condition
case class NotEqualTo(loc:Loc, left:Element, val right:Element) extends Condition
