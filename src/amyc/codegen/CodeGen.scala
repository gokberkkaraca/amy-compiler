package amyc
package codegen

import analyzer._
import ast.Identifier
import ast.SymbolicTreeModule.{And => AmyAnd, Call => AmyCall, Div => AmyDiv, Or => AmyOr, _}
import utils.{Context, Pipeline}
import wasm.{Instructions, _}
import Instructions._
import Utils._

// Generates WebAssembly code for an Amy program
object CodeGen extends Pipeline[(Program, SymbolTable), Module] {
  def run(ctx: Context)(v: (Program, SymbolTable)): Module = {
    val (program, table) = v

    // Generate code for an Amy module
    def cgModule(moduleDef: ModuleDef): List[Function] = {
      val ModuleDef(name, defs, optExpr) = moduleDef
      // Generate code for all functions
      defs.collect { case fd: FunDef if !builtInFunctions(fullName(name, fd.name)) =>
        cgFunction(fd, name, false)
      } ++
        // Generate code for the "main" function, which contains the functionality for the module expression
        optExpr.toList.map { expr =>
          val mainFd = FunDef(Identifier.fresh("main"), Nil, TypeTree(IntType), expr)
          cgFunction(mainFd, name, true)
        }
    }

    // Generate code for a function in module 'owner'
    def cgFunction(fd: FunDef, owner: Identifier, isMain: Boolean): Function = {
      // Note: We create the wasm function name from a combination of
      // module and function name, since we put everything in the same wasm module.
      val name = fullName(owner, fd.name)
      Function(name, fd.params.size, isMain) { lh =>
        val locals = fd.paramNames.zipWithIndex.toMap
        val body = cgExpr(fd.body)(locals, lh)
        if (isMain) {
          body <:> Drop // Main functions do not return a value,
          // so we need to drop the value generated by their body
        } else {
          body
        }
      }
    }

    // Generate code for an expression expr.
    // Additional arguments are a mapping from identifiers (parameters and variables) to their index in 
    // the wasm local variables, and a LocalsHandler which will generate fresh local slots as required.
    def cgExpr(expr: Expr)(implicit locals: Map[Identifier, Int], lh: LocalsHandler): Code = {
      expr match {

        // Function or constructor call
        case AmyCall(qname, args) =>
          val constrSigOpt = table.getConstructor(qname)
          constrSigOpt match {
            case Some(constrSig) => // It is Constructor call
              val oldMemBoundaryAddress = lh.getFreshLocal()
              val argsCodeAndIndex = args.map(arg => cgExpr(arg)) zip (1 to args.size)
              val storeArgFields: List[Code] = argsCodeAndIndex.map(argCode => GetGlobal(Utils.memoryBoundary) <:> Const(4*argCode._2) <:> Add <:> argCode._1 <:> Store)

              GetGlobal(Utils.memoryBoundary) <:>
              SetLocal(oldMemBoundaryAddress) <:>
              GetGlobal(Utils.memoryBoundary) <:>
              Const((args.size + 1) * 4) <:>
              Add <:>
              SetGlobal(Utils.memoryBoundary) <:>
              GetLocal(oldMemBoundaryAddress) <:>
              Const(constrSig.index) <:>
              Store <:>
              storeArgFields <:>
              GetLocal(oldMemBoundaryAddress)

            case None => // Which means the call is not a constructor call, then it is function call
              val funSig = table.getFunction(qname).get
              val fullName = Utils.fullName(funSig.owner, qname)
              args.map(arg => cgExpr(arg)) <:> Call(fullName)
          }

        // Expression sequence
        case Sequence(e1, e2) => cgExpr(e1) <:> Drop <:> cgExpr(e2)

        // Variable definition
        case Let(df, value, body) =>
          val ident = df.name
          val address = lh.getFreshLocal()
          cgExpr(value) <:> SetLocal(address) <:> cgExpr(body)(locals + (ident -> address), lh)

        // If then else
        case Ite(cond, thenn, elze) => cgExpr(cond) <:> If_i32 <:> cgExpr(thenn) <:> Else <:> cgExpr(elze) <:> End

        // Match Case
        case Match(scrut, cases) =>

          // Find patterns and expressions from cases list
          val casePatterns = cases.map(cse => cse.pat)
          val caseExprs = cases.map(cse => cse.expr)

          // Calculate the scrut and store it in a local variable
          val scrutCodeVarIndex = lh.getFreshLocal()
          val calcScrut: Code = cgExpr(scrut) <:> SetLocal(scrutCodeVarIndex) <:> GetLocal(scrutCodeVarIndex)

          def matchAndBind(v: Expr, casePattern: Pattern): (Code, Map[Identifier, Int]) = {
            casePattern match {
              case WildcardPattern() => (Drop <:> Const(1), locals)
              case IdPattern(name) =>  {
                val binding = lh.getFreshLocal()
                (SetLocal(binding) <:> Const(1), locals + (name -> binding))
              }
              case LiteralPattern(lit) => (cgExpr(lit) <:> Eq, locals)
              case CaseClassPattern(constr, args) => ???
            }
          }

          val (caseCode, newLocals) = matchAndBind(scrut, cases.head.pat)
          calcScrut <:> caseCode <:>
          If_i32 <:> cgExpr(cases.head.expr)(newLocals, lh) <:>
          Else <:> mkString("Match error!") <:> Call("Std_printString") <:> Unreachable <:>
          End


        // Variable
        case Variable(name) => GetLocal(locals(name))

        // Literals
        case IntLiteral(value) => Const(value)
        case BooleanLiteral(value) => if (value) Const(1) else Const(0)
        case StringLiteral(value) => mkString(value)
        case UnitLiteral() => Const(0)

        // Binary operators
        case Plus(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Add
        case Minus(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Sub
        case Times(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Mul
        case AmyDiv(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Div
        case Mod(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Rem
        case LessThan(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Lt_s
        case LessEquals(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Le_s
        case AmyAnd(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> And
        case AmyOr(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Or
        case Equals(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Eq
        case Concat(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Call(concatImpl.name)

        // Unary operators
        case Not(e) => cgExpr(e) <:> Eqz
        case Neg(e) => Const(0) <:> cgExpr(e) <:> Sub

        // Represents a computational error; prints its message, then exits
        case Error(msg) =>
          cgExpr(msg) <:>
          Call("Std_printString") <:>
          Instructions.Unreachable
      }
    }

    Module(
      program.modules.last.name.name,
      defaultImports,
      globalsNo,
      wasmFunctions ++ (program.modules flatMap cgModule)
    )

  }
}
