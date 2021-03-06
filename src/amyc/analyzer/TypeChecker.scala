package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  // Compute the least upped bound of two type.
  // This is defined if and only if the types are equal or one is NothingType
  def leastUpperBound(t1: Type, t2: Type): Option[Type] = (t1, t2) match {
    case (NothingType, t2) => Some(t2)
    case (t1, NothingType) => Some(t1)
    case (t1, t2) if t1 == t2 => Some(t1)
    case _ => None
  }

  // leastUpperBound for a list of types
  def leastUpperBound(tps: List[Type]): Option[Type] = {
    require(tps.nonEmpty)
    tps.foldLeft[Option[Type]](Some(NothingType)) {
      case (soFar, current) => soFar flatMap (leastUpperBound(_, current))
    }
  }

  val list: List[Int] = Nil

  def run(ctx: Context)(v: (Program, SymbolTable)) = {
    import ctx.reporter._

    val (program, table) = v

    // Type check an expression
    // 'expected' is the type 'expr' should have. If None, 'expr' can be of any type
    // Make sure the helper methods provided.
    // Remember to add local variables to the symbol table to use them later!
    def tc(expr: Expr, expected: Option[Type]): Type = {
      // Check a type 'actual' against the expected type if present,
      // and emit an error if it is not equal
      def check(actual: Type)(implicit pos: Positioned) = {
        expected.foreach { exp =>
          if (actual != exp) {
            error(s"Type error: expected $exp, found $actual", pos)
          }
        }
        actual
      }

      // Compute least upper bound, emit an error and return the first one if not defined
      def lub(t1: Type, t2: Type)(pos: Positioned) = leastUpperBound(t1, t2).getOrElse {
        error(s"Incompatible types $t1 and $t2", pos)
        t1
      }

      // Same as lub, but for a list of types
      def lub_*(ts: List[Type])(pos: Positioned): Type = {
        require(ts.nonEmpty)
        ts.reduceLeft[Type](lub(_, _)(pos))
      }

      // Implicit position for 'check'
      implicit val pos: Positioned = expr
      expr match {

        // Function or constructor call
        case Call(qname, args) =>
          val constrSig = table.getConstructor(qname).orNull
          if (constrSig != null) {
            val ConstrSig(argTypes: List[Type], _, _) = table.getConstructor(qname).get
            for (i <- args.indices) tc(args(i), Some(argTypes(i)))
            check(constrSig.retType)
          }
          else {
            val FunSig(argTypes, retType, _) = table.getFunction(qname).get
            for (i <- args.indices) tc(args(i), Some(argTypes(i)))
            check(retType)
          }

        // Expression sequence
        case Sequence(e1, e2) =>
          tc(e1, None)
          tc(e2, expected)

        // Variable definition
        case Let(df, value, body) =>
          table.addLocal(df.name, df.tpe.tpe)
          val valueType = tc(value, None)
          val dfType = df.tpe.tpe
          lub(valueType, dfType)(pos)
          tc(body, expected)

        // If then else
        case Ite(cond, thenn, elze) =>
          tc(cond, Some(BooleanType))
          lub(tc(thenn, expected), tc(elze, expected))(pos)

        // Match Case
        case Match(scrut, cases) =>
          val scrutType = tc(scrut, None)

          for(i <- cases.indices) {
            val cse: MatchCase = cases(i)
            val pattern = cse.pat
            val expr = cse.expr

            def checkPattern(pattern: Pattern, expectedType: Type): Unit = {
              pattern match {
                case WildcardPattern() => // No need to do something, matches everything
                case IdPattern(name) => // Needs to be added to local table
                  table.addLocal(name, expectedType)
                case LiteralPattern(lit) =>
                  tc(lit, Some(expectedType))
                case CaseClassPattern(constr, args) =>
                  // Case class should be compared to scrut
                  // Parameters should be compared to constructor signature and added to locals
                  val constrSignature = table.getConstructor(constr).get
                  lub(constrSignature.retType, expectedType)(pattern)
                  val ConstrSig(argTypes: List[Type], _, _) = table.getConstructor(constr).get
                  for (i <- args.indices)
                    checkPattern(args(i), argTypes(i))
              }
            }

            checkPattern(pattern, scrutType)
            tc(expr, expected)
          }

          val caseExprTypeList = cases.map(cse => cse.expr).map(expr => tc(expr, expected))
          lub_*(caseExprTypeList)(pos)

        // Variable
        case Variable(name) =>
          check(table.getLocal(name).get)

        // Literals
        case IntLiteral(_) =>
          check(IntType)
        case BooleanLiteral(_) =>
          check(BooleanType)
        case StringLiteral(_) =>
          check(StringType)
        case UnitLiteral() =>
          check(UnitType)

        // Binary operators
        case Plus(lhs, rhs) =>
          tc(lhs, Some(IntType))
          tc(rhs, Some(IntType))
          check(IntType)
        case Minus(lhs, rhs) =>
          tc(lhs, Some(IntType))
          tc(rhs, Some(IntType))
          check(IntType)
        case Times(lhs, rhs) =>
          tc(lhs, Some(IntType))
          tc(rhs, Some(IntType))
          check(IntType)
        case Div(lhs, rhs) =>
          tc(lhs, Some(IntType))
          tc(rhs, Some(IntType))
          check(IntType)
        case Mod(lhs, rhs) =>
          tc(lhs, Some(IntType))
          tc(rhs, Some(IntType))
          check(IntType)
        case LessThan(lhs, rhs) =>
          tc(lhs, Some(IntType))
          tc(rhs, Some(IntType))
          check(BooleanType)
        case LessEquals(lhs, rhs) =>
          tc(lhs, Some(IntType))
          tc(rhs, Some(IntType))
          check(BooleanType)
        case And(lhs, rhs) =>
          tc(lhs, Some(BooleanType))
          tc(rhs, Some(BooleanType))
          check(BooleanType)
        case Or(lhs, rhs) =>
          tc(lhs, Some(BooleanType))
          tc(rhs, Some(BooleanType))
          check(BooleanType)
        case Equals(lhs, rhs) =>
          lub(tc(lhs, None), tc(rhs, None))(pos)
          check(BooleanType)
        case Concat(lhs, rhs) =>
          tc(lhs, Some(StringType))
          tc(rhs, Some(StringType))
          check(StringType)

        // Unary operators
        case Not(e) =>
          tc(e, Some(BooleanType))
          check(BooleanType)
        case Neg(e) =>
          tc(e, Some(IntType))
          check(IntType)

        // Represents a computational error; prints its message, then exits
        case Error(msg) =>
          tc(msg, Some(StringType))
          expected.getOrElse(NothingType)

      }
    }

    // Putting it all together:
    program.modules.foreach { mod =>
      // put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        params foreach { case ParamDef(name, TypeTree(tpe)) => table.addLocal(name, tpe) }
        tc(body, Some(retType.tpe))
      }
      // Typecheck expression if present, not providing expected type.
      mod.optExpr.foreach(tc(_, None))
    }

    v

  }
}
