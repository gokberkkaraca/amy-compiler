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
    modNames.foreach{ case (name, modules) =>
      if (modules.size > 1) {
        fatal(s"Two modules named $name in program", modules.head.position)
      }
    }
    modNames.keys.toList foreach table.addModule


    // Step 2: Check name uniqueness of definitions in each module
    p.modules.foreach {
      case (moduleDef) =>
          moduleDef.defs.groupBy(_.name).foreach{
            case (name, listDefs) =>
              if (listDefs.size > 1)
              fatal(s"Two definitions named $name in ${moduleDef.name}", listDefs.head.position)
        }
    }

    // Step 3: Discover types and add them to symbol table
    p.modules.foreach {
      case (moduleDef) =>
        moduleDef.defs foreach {
          case N.AbstractClassDef(name) => table.addType(moduleDef.name, name)
          case _ => _ // Prevent match error for other types of ModuleDefs
        }
    }

    // Step 4: Discover type constructors, add them to table
    p.modules.foreach {
      case (moduleDef) =>
        moduleDef.defs foreach {
          case N.CaseClassDef(name, fields, _) =>
            val args: List[S.Type] = fields.map(tt => transformType(tt, moduleDef.name))
            table.addConstructor(moduleDef.name, name, args, table.getType(moduleDef.name, name).get)
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
          }
    }

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions
    
    // This part is split into three transfrom functions,
    // for definitions, FunDefs, and expressions.
    // You will need to have in mind we transform constructs of the
    // NominalTreeModule 'N' to respective constructs of the SymbolicTreeModule 'S'.
    // transformFunDef is given as an example, as well as some code for the other ones

    def transformDef(df: N.ClassOrFunDef, module: String): S.ClassOrFunDef = { df match {
      case N.AbstractClassDef(name) =>
        transformAbsClassDef(name, module)
      case N.CaseClassDef(name, _, _) =>
        transformCaseClassDef(name, module)
      case fd: N.FunDef =>
        transformFunDef(fd, module)
    }}.setPos(df)

    def transformAbsClassDef(name: N.Name, module: String): S.ClassOrFunDef ={
      val Some(id) = table.getType(module, name)
      // TODO Add position
      S.AbstractClassDef(id)
    }

    def transformCaseClassDef(name: N.Name, module: String): S.ClassOrFunDef ={
      val (id, sig) = table.getConstructor(module, name).get
      val (argType, parent, index) = sig

      // TODO check if argType is given correctly
      S.CaseClassDef(id, argType, parent).setPos(index)
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
        case N.Match(scrut, cases) =>
          def transformCase(cse: N.MatchCase) = {
            val N.MatchCase(pat, rhs) = cse
            val (newPat, moreLocals) = transformPattern(pat)
            ??? // TODO
          }

          // Returns a transformed pattern along with all bindings
          // from strings to unique identifiers for names bound in the pattern.
          // Also, calls 'fatal' if a new name violates the Amy naming rules.
          def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = {
            ??? // TODO
          }

          S.Match(transformExpr(scrut), cases map transformCase)

        case _ =>
          ??? // TODO implement the rest of the cases
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
