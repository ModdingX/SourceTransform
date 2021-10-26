package io.github.noeppi_noeppi.tools.sourcetransform.util.signature;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.signature.SignatureVisitor;

import java.util.ArrayList;
import java.util.List;

public class SignatureNode extends SignatureVisitor {
    
    public List<FormalType> formalTypeParameters = new ArrayList<>();
    public SignatureNode superClass = null;
    public List<SignatureNode> superInterfaces = new ArrayList<>();
    public List<SignatureNode> parameters = new ArrayList<>();
    public SignatureNode returnType = null;
    public List<SignatureNode> exceptions = new ArrayList<>();
    public char baseType = 0;
    public String typeVariable = null;
    public SignatureNode arrayOf = null;
    public ClassType classType;
    
    private FormalType currentFormalType = null;
    
    public SignatureNode() {
        super(Opcodes.ASM9);
    }

    @Override
    public void visitFormalTypeParameter(String name) {
        currentFormalType = new FormalType(name);
        formalTypeParameters.add(currentFormalType);
    }

    @Override
    public SignatureVisitor visitClassBound() {
        currentFormalType.classBound = new SignatureNode();
        return currentFormalType.classBound;
    }

    @Override
    public SignatureVisitor visitInterfaceBound() {
        SignatureNode node = new SignatureNode();
        currentFormalType.interfaceBounds.add(node);
        return node;
    }

    @Override
    public SignatureVisitor visitSuperclass() {
        superClass = new SignatureNode();
        return superClass;
    }

    @Override
    public SignatureVisitor visitInterface() {
        SignatureNode node = new SignatureNode();
        superInterfaces.add(node);
        return node;
    }

    @Override
    public SignatureVisitor visitParameterType() {
        SignatureNode node = new SignatureNode();
        parameters.add(node);
        return node;
    }

    @Override
    public SignatureVisitor visitReturnType() {
        this.returnType = new SignatureNode();
        return this.returnType;
    }

    @Override
    public SignatureVisitor visitExceptionType() {
        SignatureNode node = new SignatureNode();
        exceptions.add(node);
        return node;
    }

    @Override
    public void visitBaseType(char descriptor) {
        this.baseType = descriptor;
    }

    @Override
    public void visitTypeVariable(String name) {
        this.typeVariable = name;
    }

    @Override
    public SignatureVisitor visitArrayType() {
        this.arrayOf = new SignatureNode();
        return this.arrayOf;
    }

    @Override
    public void visitClassType(String name) {
        this.classType = new ClassType(name);
    }

    @Override
    public void visitInnerClassType(String name) {
        //
    }

    @Override
    public void visitTypeArgument() {
        this.classType.arguments.add(null);
    }

    @Override
    public SignatureVisitor visitTypeArgument(char wildcard) {
        SignatureNode node = new SignatureNode();
        this.classType.arguments.add(node);
        return node;
    }

    @Override
    public void visitEnd() {
        //
    }
}
