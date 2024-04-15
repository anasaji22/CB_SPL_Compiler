package de.thm.mni.compilerbau.absyn

import de.thm.mni.compilerbau.absyn.visitor.Visitable
import de.thm.mni.compilerbau.utils.StringOps
import java.util.*
import java.util.stream.Collectors

/**
 * This abstract class is the root in the hierarchy of AST classes.
 *
 * Every part of the AST has to extend this class.
 */
sealed class Node(@JvmField val position: Position) : Visitable {

    companion object {
        private fun formatAstBasic(name: String, arguments: List<String>): String {
            val indent = 2
            return if (arguments.isEmpty()) "$name()"
            else "$name(\n${StringOps.indent(arguments.joinToString(",\n"), indent)})"
        }

        fun formatAst(name: String, vararg arguments: Any) = formatAstList(name, arguments.toList())

        fun formatAstList(name: String, arguments: List<*>?): String {
            return formatAstBasic(name,
                if (arguments == null) listOf("LIST_IS_NULL")
                else Arrays.stream(arguments.toTypedArray()).map { o: Any? -> o?.toString() ?: "NULL" }
                    .collect(Collectors.toList()))
        }
    }
}
