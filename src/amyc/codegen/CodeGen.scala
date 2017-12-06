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
        case AmyCall(qname, args) => args.map(arg => cgExpr(arg)) <:> Call(qname.fullName)

        // Expression sequence
        case Sequence(e1, e2) => cgExpr(e1) <:> Drop <:> cgExpr(e2)

        // Variable definition
        case Let(df, value, body) => cgExpr(value) <:> SetLocal(lh.getFreshLocal()) <:> cgExpr(body)

        // If then else
        case Ite(cond, thenn, elze) => cgExpr(cond) <:> If_i32 <:> cgExpr(thenn) <:> Else <:> cgExpr(elze) <:> End

        // Match Case
        case Match(scrut, cases) => ???

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
        case Error(msg) => ???
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