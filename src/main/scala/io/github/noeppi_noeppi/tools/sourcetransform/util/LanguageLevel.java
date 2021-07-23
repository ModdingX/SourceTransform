package io.github.noeppi_noeppi.tools.sourcetransform.util;

import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;

import java.util.Hashtable;

@SuppressWarnings("deprecation")
public enum LanguageLevel {
    JAVA_8(AST.JLS8, JavaCore.VERSION_1_8),
    JAVA_10(AST.JLS10, JavaCore.VERSION_10),
    JAVA_16(AST.JLS16, JavaCore.VERSION_16);

    private final int level;
    private final String spec;

    LanguageLevel(int level, String spec) {
        this.level = level;
        this.spec = spec;
    }
    
    public ASTParser createParser() {
        ASTParser parser = ASTParser.newParser(level);
        Hashtable<String, String> options = JavaCore.getDefaultOptions();
        JavaCore.setComplianceOptions(spec, options);
        parser.setCompilerOptions(options);
        return parser;
    }
}
