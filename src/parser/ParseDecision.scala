package parser

import components.Token

abstract class ParseDecision
case class LeftReduce(root: Token, dep: Token) extends ParseDecision
case class RightReduce(root: Token, dep: Token) extends ParseDecision
class Shift extends ParseDecision