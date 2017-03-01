import newlang.{Element, Evaluation, Execution, Literal, Loc}

object Main extends App
{
    val start = Loc(1, 1)

    Method(start, "code", If(
        GTE(1, 3,)
    ))
}
