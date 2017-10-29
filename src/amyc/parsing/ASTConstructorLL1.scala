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
      case Node('QName ::= _, List(id, qnameseq)) =>
        val (module, pos) = constructName(id)
        val (isQNameSeqEpsilon, name) = constructQNameSeq(qnameseq)
        if(isQNameSeqEpsilon) (QualifiedName(None, name), pos)
        else (QualifiedName(Some(module), name), pos)
    }
  }

  def constructQNameSeq(pTree: NodeOrLeaf[Token]): (Boolean, String) = {
    pTree match {
      case Node('QNameSeq ::= _, List(_, id)) =>
        val (name, _) = constructName(id)
        (false, name)
      case _ => (true, null) // epsilon case
    }
  }

  override def constructExpr(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr ::= _, List(Leaf(vt), param, _, expr2, _, expr)) =>
        Let(constructParam(param), constructExpr2(expr2), constructExpr(expr))
      case Node('Expr ::= _, List(expr2, exprSeq)) =>
        val (isExprSeqEpsilon, part2) = constructExprSeq(exprSeq)

        if(isExprSeqEpsilon) constructExpr2(expr2)
        else {
          val part1 = constructExpr2(expr2)
          Sequence(part1, part2).setPos(part1)
        }
    }
  }

  def constructExprSeq(ptree: NodeOrLeaf[Token]): (Boolean, Expr) = {
    ptree match {
      case Node('ExprSeq ::= _, List(_, expr)) =>
        (false, constructExpr(expr))
      case _ => (true, null)
    }
  }

  def constructExpr2(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr2 ::= _, List(expr3, exprseq2)) =>
        val (isExprSeq2Epsilon, cases) = constructExprSeq2(exprseq2)

        if(isExprSeq2Epsilon) constructExpr3(expr3)
        else {
          val scrut = constructExpr3(expr3)
          Match(scrut, constructCases(cases))
        }
    }
  }

  def constructCases(ptree: NodeOrLeaf[Token]): List[MatchCase] = {
    ptree match {
      case Node('Cases ::= _, List(cs, caseseq)) =>
        val cases = constructCaseSeq(caseseq)
        constructCase(cs) :: constructCaseSeq(caseseq)
    }
  }

  def constructCaseSeq(ptree: NodeOrLeaf[Token]): List[MatchCase] = {
    ptree match {
      case Node('CasesSeq ::= ('Cases :: Nil), List(cases)) => constructCases(cases)
      case Node('CasesSeq ::= _, _) => Nil // epsilon case
    }
  }

  def constructExprSeq2(ptree: NodeOrLeaf[Token]): (Boolean, NodeOrLeaf[Token]) = {
    ptree match {
      case Node('ExprSeq2 ::= _, List(_, _, cases, _)) =>
        (false, cases)
      case _ => (true, null) // epsilon case
    }
  }

  def constructExpr3(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr3 ::= _, List(expr4, exprseq3)) =>
        constructOpExpr(constructExpr4(expr4), exprseq3, constructExpr4)
      case Node('Expr3 ::= _, List(expr4)) =>
        constructExpr4(expr4)
    }
  }

  def constructExpr4(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr4 ::= _, List(expr5, exprseq4)) =>
        constructOpExpr(constructExpr5(expr5), exprseq4, constructExpr5)
      case Node('Expr4 ::= _, List(expr5)) =>
        constructExpr5(expr5)
    }
  }

  def constructExpr5(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr5 ::= _, List(expr6, exprseq5)) =>
        constructOpExpr(constructExpr6(expr6), exprseq5, constructExpr6)
      case Node('Expr5 ::= _, List(expr6)) =>
        constructExpr6(expr6)
    }
  }

  def constructExpr6(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr6 ::= _, List(expr7, exprseq6)) =>
        constructOpExpr(constructExpr7(expr7), exprseq6, constructExpr7)
      case Node('Expr6 ::= _, List(expr7)) =>
        constructExpr7(expr7)
    }
  }

  def constructExpr7(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr7 ::= _, List(expr8, exprseq7)) =>
        constructOpExpr(constructExpr8(expr8), exprseq7, constructExpr8)
      case Node('Expr7 ::= _, List(expr8)) =>
        constructExpr8(expr8)
    }
  }

  def constructExpr8(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr8 ::= _, List(expr9, exprseq8)) =>
        constructOpExpr(constructExpr9(expr9), exprseq8, constructExpr9)
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
      case Node('Expr9 ::= _, List(expr10)) =>
        constructExpr10(expr10)
    }
  }

  def constructExpr10(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {

      // 'Id ~ 'QNameHelper
      case Node('Expr10 ::= List('Id, _), List(id, qnamehelper)) =>
        val (isQNameSeqEpsilon, isQNameHelperEpsilon, qnameid, args) = constructQNameHelper(qnamehelper)
        if(isQNameHelperEpsilon) {
          val (name, pos) = constructName(id)
          Variable(name).setPos(pos)
        }
        else {
          if(isQNameSeqEpsilon) {
            val (name, pos) = constructName(id)
            val qname = QualifiedName(None, name)
            Call(qname, args)
          }
          else {
            val (module, pos) = constructName(id)
            val qname = QualifiedName(Some(module), qnameid.get)
            Call(qname, args)
          }
        }

      // LiteralNoParen
      case Node('Expr10 ::= List('LiteralNoParen), List(lit)) =>
        constructLiteralNoParen(lit)

      // LPAREN() 'LParenHelper
      case Node('Expr10 ::= List(LPAREN(), _), List(Leaf(lp@LPAREN()), lparenhelper)) =>
        val (unit, expr) = constructLParenHelper(lparenhelper)
        if (unit) UnitLiteral().setPos(lp)
        else expr.setPos(lp)

      // ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN()
      case Node('Expr10 ::= (ERROR() :: _), List(Leaf(ert), _, msg, _)) =>
        Error(constructExpr(msg)).setPos(ert)

      // IF ELSE
      case Node('Expr10 ::= (IF() :: _), List(Leaf(it), _, cond, _, _, thenn, _, _, _, elze, _)) =>
        Ite(
          constructExpr(cond),
          constructExpr(thenn),
          constructExpr(elze)
        ).setPos(it)
    }
  }

  def constructQNameHelper(ptree: NodeOrLeaf[Token]): (Boolean, Boolean, Option[String], List[Expr]) = {
    ptree match {
      case Node('QNameHelper ::= _, List(qnameseq, _, as, _)) =>
        val (isQNameSeqEpsilon, name) = constructQNameSeq(qnameseq)

        if(isQNameSeqEpsilon) {
          val isQNameHelperEpsilon = false
          val args = constructList(as, constructExpr, hasComma = true)
          (isQNameSeqEpsilon, isQNameHelperEpsilon, None, args)
        }
        else {
          val isQNameHelperEpsilon = false
          val args = constructList(as, constructExpr, hasComma = true)

          if (isQNameSeqEpsilon) (isQNameSeqEpsilon, isQNameHelperEpsilon, None, args)
          else (isQNameSeqEpsilon, isQNameHelperEpsilon, Some(name), args)
        }
      case _ => (true, true, null, null)
    }
  }

  def constructLiteralNoParen(ptree: NodeOrLeaf[Token]): Literal[_] = {
    ptree match {
      case Node('LiteralNoParen ::= List(INTLITSENT), List(Leaf(it@INTLIT(i)))) =>
        IntLiteral(i).setPos(it)
      case Node('LiteralNoParen ::= List(STRINGLITSENT), List(Leaf(st@STRINGLIT(s)))) =>
        StringLiteral(s).setPos(st)
      case Node('LiteralNoParen ::= _, List(Leaf(tt@TRUE()))) =>
        BooleanLiteral(true).setPos(tt)
      case Node('LiteralNoParen ::= _, List(Leaf(tf@FALSE()))) =>
        BooleanLiteral(false).setPos(tf)
    }
  }

  def constructLParenHelper(ptree: NodeOrLeaf[Token]): (Boolean, Expr) ={
    ptree match {
      case Node('LParenHelper ::= List(RPAREN()), List(_)) =>
        // TODO check if this is correct
        val expr: Expr = null
        (true, expr)
      case Node('LParenHelper ::= _, List(expr, _)) =>
        (false, constructExpr(expr))
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
        val (isQNameSeqEpsilon, isPatternSeqEpsilon, module, patterns) = constructPatternSeq(patternseq)

        if(isPatternSeqEpsilon) {
          val (name, pos) = constructName(id)
          IdPattern(name).setPos(pos)
        }
        else {
          if(isQNameSeqEpsilon){
            val (name, pos) = constructName(id)
            val qname = QualifiedName(None, name)
            CaseClassPattern(qname, patterns).setPos(pos)
          }
          else {
            val (name, pos) = constructName(id)
            val qname = QualifiedName(Some(module), name)
            CaseClassPattern(qname, patterns).setPos(pos)
          }
        }
    }
  }


  def constructPatternSeq(pTree: NodeOrLeaf[Token]): (Boolean, Boolean, String, List[Pattern]) = {
    pTree match {
      case Node('PatternSeq ::= _, List(qnseq, _, patts, _)) =>
        val (isQNameSeqEpsilon, qnameseq) = constructQNameSeq(qnseq)

        if(isQNameSeqEpsilon){
          val patterns = constructList(patts, constructPattern, hasComma = true)
          (false, false, null, patterns)
        }
        else {
          val patterns = constructList(patts, constructPattern, hasComma = true)
          (true, false, qnameseq, patterns)
        }
      case _ => (true, true, null, null)
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
  def constructOpExpr(leftopd: Expr, ptree: NodeOrLeaf[Token], nextConstructor: (NodeOrLeaf[Token] => Expr)): Expr = {
    ptree match {
      case Node(_, List()) => //epsilon rule of the nonterminals
        leftopd
      case Node(sym ::= _, List(op, rightNode))
        if Set('ExprSeq3, 'ExprSeq4, 'ExprSeq5, 'ExprSeq6, 'ExprSeq7, 'ExprSeq8) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = nextConstructor(nextOpd)
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf, nextConstructor) // captures left associativity
        }
    }
  }
}