package de.thm.mni.compilerbau.absyn

/**
 * This class is the abstract superclass of all expressions in SPL.
 *
 * Everything that behaves like a value is an [Expression] in SPL.
 * There are three types of expressions: [BinaryExpression], [IntLiteral] and [VariableExpression]
 *
 * Every expression has a semantic type, which has to be calculated in phase 4.
 */
sealed class Expression(position: Position) : Node(position)
