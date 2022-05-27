package io.github.noeppi_noeppi.tools.sourcetransform.util.signature;

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
    
    public SignatureNode(int api) {
        super(api);
    }

    @Override
    public void visitFormalTypeParameter(String name) {
        this.currentFormalType = new FormalType(name);
        this.formalTypeParameters.add(this.currentFormalType);
    }

    @Override
    public SignatureVisitor visitClassBound() {
        this.currentFormalType.classBound = new SignatureNode(this.api);
        return this.currentFormalType.classBound;
    }

    @Override
    public SignatureVisitor visitInterfaceBound() {
        SignatureNode node = new SignatureNode(this.api);
        this.currentFormalType.interfaceBounds.add(node);
        return node;
    }

    @Override
    public SignatureVisitor visitSuperclass() {
        this.superClass = new SignatureNode(this.api);
        return this.superClass;
    }

    @Override
    public SignatureVisitor visitInterface() {
        SignatureNode node = new SignatureNode(this.api);
        this.superInterfaces.add(node);
        return node;
    }

    @Override
    public SignatureVisitor visitParameterType() {
        SignatureNode node = new SignatureNode(this.api);
        this.parameters.add(node);
        return node;
    }

    @Override
    public SignatureVisitor visitReturnType() {
        this.returnType = new SignatureNode(this.api);
        return this.returnType;
    }

    @Override
    public SignatureVisitor visitExceptionType() {
        SignatureNode node = new SignatureNode(this.api);
        this.exceptions.add(node);
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
        this.arrayOf = new SignatureNode(this.api);
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
        SignatureNode node = new SignatureNode(this.api);
        this.classType.arguments.add(node);
        return node;
    }

    @Override
    public void visitEnd() {
        //
    }
}
