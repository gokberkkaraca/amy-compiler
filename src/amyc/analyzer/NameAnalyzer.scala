package amyc
package analyzer

import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}

// Name analyzer for Amy
// Takes a nominal program (names are plain string, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates and returns symbol table.
object NameAnalyzer extends Pipeline[N.Program, (S.Program, SymbolTable)] {
  def run(ctx: Context)(p: N.Program): (S.Program, SymbolTable) = {
    import ctx.reporter._

    // Step 0: Initialize symbol table
    val table = new SymbolTable

    // Helper method: will transform a nominal type 'tt' to a symbolic type,
    // given we are within module 'inModule'
    def transformType(tt: N.TypeTree, inModule: String): S.Type = {
      tt.tpe match {
        case N.IntType => S.IntType
        case N.BooleanType => S.BooleanType
        case N.StringType => S.StringType
        case N.UnitType => S.UnitType
        case N.ClassType(qn@N.QualifiedName(module, name)) =>
          table.getType(module getOrElse inModule, name) match {
            case Some(symbol) =>
              S.ClassType(symbol)
            case None =>
              fatal(s"Could not find type $qn", tt)
          }
      }
    }

    // Step 1: Add modules to table 
    val modNames = p.modules.groupBy(_.name)
    modNames.foreach { case (name, modules) =>
      if (modules.size > 1) {
        fatal(s"Two modules named $name in program", modules.head.position)
      }
    }
    modNames.keys.toList foreach table.addModule


    // Step 2: Check name uniqueness of definitions in each module
    p.modules.foreach {
      case (moduleDef) =>
        moduleDef.defs.groupBy(_.name).foreach {
          case (name, listDefs) =>
            if (listDefs.size > 1)
              fatal(s"Two definitions named $name in ${moduleDef.name}", listDefs.head.position)
        }
    }

    // Step 3: Discover types and add them to symbol table
    p.modules.foreach {
      case (moduleDef) =>
        moduleDef.defs foreach {
          case N.AbstractClassDef(name) =>
            table.addType(moduleDef.name, name)
          case _ => // Prevent match error for other types of ModuleDefs
        }
    }

    // Step 4: Discover type constructors, add them to table
    p.modules.foreach {
      case (moduleDef) =>
        moduleDef.defs foreach {
          case N.CaseClassDef(name, fields, parent) =>
            val args: List[S.Type] = fields.map(tt => transformType(tt, moduleDef.name))
            table.addConstructor(moduleDef.name, name, args, table.getType(moduleDef.name, parent).get)
          case _ => // Prevent match error for other types of ModuleDefs
        }
    }

    // Step 5: Discover functions signatures, add them to table
    p.modules.foreach {
      case (moduleDef) =>
        moduleDef.defs foreach {
          case N.FunDef(name, params, retType, _) =>
            val argTypes = params.map(param => param.tpe).map(tt => transformType(tt, moduleDef.name))
            val symRetType = transformType(retType, moduleDef.name)
            table.addFunction(moduleDef.name, name, argTypes, symRetType)
          case _ => // Prevent match error for other types of ModuleDefs
        }
    }

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions

    // This part is split into three transfrom functions,
    // for definitions, FunDefs, and expressions.
    // You will need to have in mind we transform constructs of the
    // NominalTreeModule 'N' to respective constructs of the SymbolicTreeModule 'S'.
    // transformFunDef is given as an example, as well as some code for the other ones

    def transformDef(df: N.ClassOrFunDef, module: String): S.ClassOrFunDef = {
      df match {
        case N.AbstractClassDef(name) =>
          transformAbsClassDef(name, module)
        case N.CaseClassDef(name, _, _) =>
          transformCaseClassDef(name, module)
        case fd: N.FunDef =>
          transformFunDef(fd, module)
      }
    }.setPos(df)

    def transformAbsClassDef(name: N.Name, module: String): S.ClassOrFunDef = {
      val Some(id) = table.getType(module, name)
      S.AbstractClassDef(id)
    }

    def transformCaseClassDef(name: N.Name, module: String): S.ClassOrFunDef = {
      val (id, sig) = table.getConstructor(module, name).get
      val ConstrSig(argTypes, parent, _) = sig

      val symArgTypes = argTypes.map(arg => S.TypeTree(arg))
      S.CaseClassDef(id, symArgTypes, parent)
    }

    def transformFunDef(fd: N.FunDef, module: String): S.FunDef = {
      val N.FunDef(name, params, retType, body) = fd
      val Some((sym, sig)) = table.getFunction(module, name)

      params.groupBy(_.name).foreach { case (name, ps) =>
        if (ps.size > 1) {
          fatal(s"Two parameters named $name in function ${fd.name}", fd)
        }
      }

      val paramNames = params.map(_.name)

      val newParams = params zip sig.argTypes map { case (pd@N.ParamDef(name, tt), tpe) =>
        val s = Identifier.fresh(name)
        S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.FunDef(
        sym,
        newParams,
        S.TypeTree(sig.retType).setPos(retType),
        transformExpr(body)(module, (paramsMap, Map()))
      ).setPos(fd)
    }

    // This function takes as implicit a pair of two maps:
    // The first is a map from names of parameters to their unique identifiers,
    // the second is similar for local variables.
    // Make sure to update them correctly if needed given the scoping rules of Amy
    def transformExpr(expr: N.Expr)
                     (implicit module: String, names: (Map[String, Identifier], Map[String, Identifier])): S.Expr = {
      val (params, locals) = names
      val res = expr match {
        // Function/ type constructor call
        case N.Call(qname, args) =>
          val sName = Identifier.fresh(qname.name)
          S.Call(sName, args.map(arg => transformExpr(arg)))
        case N.Sequence(e1, e2) => S.Sequence(transformExpr(e1), transformExpr(e2))
        case N.Let(df, value, body) =>
          val sName = Identifier.fresh(df.name)
          val sTypeTree = S.TypeTree(transformType(df.tpe, module))
          val paramDef = S.ParamDef(sName, sTypeTree)
          val sValue = transformExpr(value)
          val sBody = transformExpr(body)(module, (params, locals + (df.name -> sName)))
          S.Let(paramDef, sValue, sBody)
        case N.Ite(cond, thenn, elze) => S.Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze))
        case N.Match(scrut, cases) =>
          def transformCase(cse: N.MatchCase) = {
            val N.MatchCase(pat, rhs) = cse
            val (newPat, moreLocals) = transformPattern(pat)
            val newLocals: Map[String, Identifier] = locals ++ moreLocals.toMap
            S.MatchCase(newPat, transformExpr(rhs)(module, (params, newLocals)))
          }

          // Returns a transformed pattern along with all bindings
          // from strings to unique identifiers for names bound in the pattern.
          // Also, calls 'fatal' if a new name violates the Amy naming rules.
          def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = {
            pat match {
              case N.WildcardPattern() => (S.WildcardPattern(), List())
              case N.IdPattern(name) =>
                val id = Identifier.fresh(name)
                (S.IdPattern(id), List((name, id)))
              case N.LiteralPattern(lit) =>
                val sLit = lit match {
                  case N.IntLiteral(value) => S.IntLiteral(value)
                  case N.BooleanLiteral(value) => S.BooleanLiteral(value)
                  case N.StringLiteral(value) => S.StringLiteral(value)
                  case N.UnitLiteral() => S.UnitLiteral()
                }
                (S.LiteralPattern(sLit), Nil)
              case N.CaseClassPattern(constr, args) =>
                val owner = constr.module.getOrElse(module) // TODO Check if this getOrElse correct
                val name = constr.name
                println(owner, name)
                val sConstr = table.getConstructor(owner, name)
                (S.CaseClassPattern(sConstr.get._1, args.map(arg => transformPattern(arg)._1)), Nil) // TODO check what to return in the list
            }
          }

          S.Match(transformExpr(scrut), cases map transformCase)

        // Variable
        case N.Variable(name) =>
          val identifier = locals.getOrElse(name, params.getOrElse(name, fatal(s"Variable $name does not exist", expr)))
          S.Variable(identifier)

        // Literals
        case N.IntLiteral(value) => S.IntLiteral(value)
        case N.BooleanLiteral(value) => S.BooleanLiteral(value)
        case N.StringLiteral(value) => S.StringLiteral(value)
        case N.UnitLiteral() => S.UnitLiteral()

        // Binary operators
        case N.Plus(lhs, rhs) => S.Plus(transformExpr(lhs), transformExpr(rhs))
        case N.Minus(lhs, rhs) => S.Minus(transformExpr(lhs), transformExpr(rhs))
        case N.Times(lhs, rhs) => S.Times(transformExpr(lhs), transformExpr(rhs))
        case N.Div(lhs, rhs) => S.Div(transformExpr(lhs), transformExpr(rhs))
        case N.Mod(lhs, rhs) => S.Mod(transformExpr(lhs), transformExpr(rhs))
        case N.LessThan(lhs, rhs) => S.LessThan(transformExpr(lhs), transformExpr(rhs))
        case N.LessEquals(lhs, rhs) => S.LessEquals(transformExpr(lhs), transformExpr(rhs))
        case N.And(lhs, rhs) => S.And(transformExpr(lhs), transformExpr(rhs))
        case N.Or(lhs, rhs) => S.Or(transformExpr(lhs), transformExpr(rhs))
        case N.Equals(lhs, rhs) => S.Equals(transformExpr(lhs), transformExpr(rhs))
        case N.Concat(lhs, rhs) => S.Concat(transformExpr(lhs), transformExpr(rhs))

        // Unary operators
        case N.Not(e) => S.Not(transformExpr(e))
        case N.Neg(e) => S.Neg(transformExpr(e))

        // Represents a computational error; prints its message, then exits
        case N.Error(msg) => S.Error(transformExpr(msg))
      }

      res.setPos(expr)
    }

    // Putting it all together to construct the final program for step 6.
    val newProgram = S.Program(
      p.modules map { case mod@N.ModuleDef(name, defs, optExpr) =>
        S.ModuleDef(
          table.getModule(name).get,
          defs map (transformDef(_, name)),
          optExpr map (transformExpr(_)(name, (Map(), Map())))
        ).setPos(mod)
      }
    ).setPos(p)

    (newProgram, table)

  }
}
