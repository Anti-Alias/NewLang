/**
* Package object for NewLang
*/
package object newlang
{
    /**
    * Object that stored type names.
    */
    object Types
    {
        val string = "String"
        val bool = "Bool"
        val char = "Char"
        val int = "Int"
        val short = "Short"
        val long = "Long"
        val double = "Double"
        val float = "Float"
        val byte = "Byte"
    }

    /**
    * Some location in a File
    */
    case class Loc(lineNum:Int, colNum:Int)

    /**
    * Represents some existing set of variables.
    */
    case class Context(variables: Set[Variable], maybeParent:Option[Context] = None)
    {
        /**
        * Set of variables converted into a Map of Strings to Evaluations.
        */
        val variableMap:Map[String, Evaluation] = variables
            .map{variable => (variable.name, variable.result)}
            .toMap

        /**
        * Acquires some Evaluation using a given key.
        */
        def apply(key:String):Evaluation = variableMap.get(key) match
        {
            Some(eval:Evaluation) => eval
            None => maybeParent match
            {
                case Some(con:Context) => con.apply(key)
                case None => throw new RuntimeException("Could not access Evaluation ''" + key + "' from Context.")
            }
        }

        
    }

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
    * Some code that evaluates to something else.
    */
    trait Evaluation extends Element
    {
        /**
        * Result of the evaluation.  Retrieved either by accessing a static value,
        * or by executing a function.
        */
        def result:Any

        /**
        * @return expected result type.  Possible types determined by
        * Types object.
        */
        def typ:String
    }

    /**
    * Represents the Member of some object.
    */
    trait Member extends Evaluation
    {
        def name:String
        def result:Evaluation
    }

    /**
    * Type of something.
    * @param name Name of the type
    * @param typeArgs Possible type arguments passed into the Type.
    */
    case class Type(loc: Loc, name:String, typeArgs:Seq[Type] = Seq.empty) extends Element

    /**
    * Some variable. Has a name which is coupled with a type and a value.
    */
    case class Variable(loc:Loc, name:String, typ:Type, value:Evaluation) extends Element

    /**
    * Represents a Declaration of some sort.
    */
    case class Declaration(loc:Loc, isMutable:Boolean, variable:Variable) extends Element

    /**
    * Literal using some scala type.
    */
    case class Literal(loc:Loc, result:Any, typ:String)

    /**
    * Represents some Closure of code.
    */
    case class Closure(loc:Loc, context:Context, statements:Seq[Evaluation]) extends Evaluation
    {
        // Checks requirements.
        require(statements.last.isInstanceOf[Evaluation], "Last statement in a Closure must evalute to something.")

        /**
        * Computes a result based on context variable and statements.
        * @param context Context to consider.
        * @param index Index of statement to execute/evaluate.
        */
        def computeResult(context:Context, index:Int):Any =
        {
            // Gets current evaluation and last index
            val eval:Evaluation = statements(index)
            val lastIndex:Int = statements.length-1

            // If not at end, execute and update context.  Then, recurse
            if(!index == lastIndex)
            {
                val newContext = elem match
                {
                    case assign:Assignment =>
                    {

                    }
                    case ex:Execution =>
                    {

                    }
                }
            }

            // Otherwise, return result of last Evaluation
            else
                eval.result
        }

        /**
        * Computation of the Closure
        */
        override def result:Any = computeResult(context, 0)
    }

    /**
    * Represents a while statement.
    * @param condition Condition while loop must satisfy to execute value.
    * Must evaluate to a Boolean
    * @param code Code to execute
    */
    case class While(loc:Loc, condition:Evaluation, code:Execution) extends Execution
    {
        require(condition.resultType == Types.bool, "")
    }

    /**
    * Represents an If statement.
    * @param condition Condition If statement must satisfy in order for result evaluation to be used.
    * Must evaluate to a Boolean.
    * @param result Result of this If statement.  Left should be used if condition is true.  Otherwise, Right.
    */
    case class If(loc:Loc, condition:Evaluation, result:Either[Evaluation, Evaluation]) extends Evaluation
}
