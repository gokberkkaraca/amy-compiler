package amyc
package parsing

import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._

// Implements the translation from parse trees to ASTs for the LL1 grammar.
// Corresponds to Parser.msGrammarLL1
// This extends the plain ASTConstructor as some things will be the same.
// You should override whatever has changed.
// Make sure to use ASTConstructor as an example
class ASTConstructorLL1 extends ASTConstructor {

  override def constructQname(pTree: NodeOrLeaf[Token]): (QualifiedName, Positioned) = {
    pTree match {
      case Node('QName ::= _, List(id)) =>
        val (name, pos) = constructName(id)
        (QualifiedName(None, name), pos)
      case Node('QName ::= _, List(id, qnameseq)) =>
        val (module, pos) = constructName(id)
        val name = constructQnameSeq(qnameseq)
        (QualifiedName(Some(module), name), pos)
    }
  }

  def constructQnameSeq(pTree: NodeOrLeaf[Token]): String ={
    pTree match {
      case Node('QNameSeq ::= _, List(_, id)) =>
        val (name, _) = constructName(id)
        name
    }
  }

  override def constructExpr(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr ::= _, List(Leaf(vt), param, _, expr2, _, expr)) =>
        Let(constructParam(param), constructExpr2(expr2), constructExpr(expr))
      case Node('Expr ::= _, List(expr2, exprhelper)) =>
        val part1 = constructExpr2(expr2)
        val part2 = constructExprHelper(exprhelper)
        Sequence(part1, part2).setPos(part1)
      case Node('Expr ::= _, List(expr2)) =>
        constructExpr2(expr2)
    }
  }

  def constructExprHelper(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('ExprHelper ::= _, List(_, expr)) =>
        constructExpr(expr)
    }
  }

  def constructExpr2(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr2 ::= _, List(expr3, exprseq2)) =>
        val scrut = constructExpr3(expr3)
        val cases = constructExprSeq2(exprseq2)
        Match(scrut, constructList1(cases, constructCase))
      case Node('Expr2 ::= _, List(expr3)) =>
        constructExpr3(expr3)
    }
  }

  def constructExprSeq2(ptree: NodeOrLeaf[Token]): NodeOrLeaf[Token] = {
    ptree match {
      case Node('ExprSeq2 ::= _, List(_, _, cases, _)) =>
        cases
    }
  }

  def constructExpr3(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr3 ::= _, List(expr4, exprseq3)) =>
        constructOpExpr(constructExpr4(expr4), exprseq3)
      case Node('Expr3 ::= _, List(expr4)) =>
        constructExpr4(expr4)
    }
  }

  def constructExpr4(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr4 ::= _, List(expr5, exprseq4)) =>
        constructOpExpr(constructExpr5(expr5), exprseq4)
      case Node('Expr4 ::= _, List(expr5)) =>
        constructExpr5(expr5)
    }
  }

  def constructExpr5(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr5 ::= _, List(expr6, exprseq5)) =>
        constructOpExpr(constructExpr6(expr6), exprseq5)
      case Node('Expr5 ::= _, List(expr6)) =>
        constructExpr6(expr6)
    }
  }

  def constructExpr6(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr6 ::= _, List(expr7, exprseq6)) =>
        constructOpExpr(constructExpr7(expr7), exprseq6)
      case Node('Expr6 ::= _, List(expr7)) =>
        constructExpr7(expr7)
    }
  }

  def constructExpr7(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr7 ::= _, List(expr8, exprseq7)) =>
        constructOpExpr(constructExpr8(expr8), exprseq7)
      case Node('Expr7 ::= _, List(expr8)) =>
        constructExpr8(expr8)
    }
  }

  def constructExpr8(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr8 ::= _, List(expr9, exprseq8)) =>
        constructOpExpr(constructExpr9(expr9), exprseq8)
      case Node('Expr8 ::= _, List(expr9)) =>
        constructExpr9(expr9)
    }
  }

  def constructExpr9(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr9 ::= List(BANG(), _), List(Leaf(bt), expr10)) =>
        Not(constructExpr10(expr10)).setPos(bt)
      case Node('Expr9 ::= List(MINUS(), _), List(Leaf(mt), expr10)) =>
        Neg(constructExpr10(expr10)).setPos(mt)
    }
  }

  def constructExpr10(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr10 ::= List('Id), List(id)) =>
        val (name, pos) = constructName(id)
        Variable(name).setPos(pos)
    }
  }

  override def constructPattern(pTree: NodeOrLeaf[Token]): Pattern = {
    pTree match {
      case Node('Pattern ::= List(UNDERSCORE()), List(Leaf(ut))) =>
        WildcardPattern().setPos(ut)
      case Node('Pattern ::= List('Literal), List(lit)) =>
        val literal = constructLiteral(lit)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::= List('Id), List(id)) =>
        val (name, pos) = constructName(id)
        IdPattern(name).setPos(pos)
      case Node('Pattern ::= _, List(id, patternseq)) =>
        val (module, pos) = constructName(id)
        val (name, patterns) = constructPatternSeq(patternseq)
        CaseClassPattern(QualifiedName(Some(module), name), patterns).setPos(pos)
    }
  }


  def constructPatternSeq(pTree: NodeOrLeaf[Token]): (String, List[Pattern]) = {
    pTree match {
      case Node('PatternSeq ::= _, List(qn, _, patts, _)) =>
        val qname = constructQnameSeq(qn)
        val patterns = constructList(patts, constructPattern, hasComma = true)
        (qname, patterns)
    }
  }

  // Important helper method:
  // Because LL1 grammar is not helpful in implementing left associativity,
  // we give you this method to reconstruct it.
  // This method takes the left operand of an operator (leftopd)
  // as well as the tree that corresponds to the operator plus the right operand (ptree)
  // It parses the right hand side and then reconstruct the operator expression
  // with correct associativity.
  // If ptree is empty, it means we have no more operators and the leftopd is returned.
  // Note: You may have to override constructOp also, depending on your implementation
  def constructOpExpr(leftopd: Expr, ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node(_, List()) => //epsilon rule of the nonterminals
        leftopd
      case Node(sym ::= _, List(op, rightNode))
        if Set('OrExpr, 'AndExpr, 'EqExpr, 'CompExpr, 'AddExpr, 'MultExpr) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = constructExpr(nextOpd)
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
        }
    }
  }

}