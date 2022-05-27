package io.github.noeppi_noeppi.tools.sourcetransform.util

import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.core.dom.AST
import org.eclipse.jdt.core.dom.ASTParser

import java.util.Hashtable

enum LanguageLevel (level: Int, spec: String) extends Enum[LanguageLevel] {
    case JAVA_8 extends LanguageLevel(AST.JLS8, JavaCore.VERSION_1_8)
    case JAVA_16 extends LanguageLevel(AST.JLS16, JavaCore.VERSION_16)
    case JAVA_17 extends LanguageLevel(AST.JLS17, JavaCore.VERSION_17)
    
    def createParser(): ASTParser = {
        val parser = ASTParser.newParser(level)
        val options = JavaCore.getDefaultOptions
        JavaCore.setComplianceOptions(spec, options)
        parser.setCompilerOptions(options)
        parser
    }
}

object LanguageLevel {
    val DEFAULT: LanguageLevel = JAVA_17
}
