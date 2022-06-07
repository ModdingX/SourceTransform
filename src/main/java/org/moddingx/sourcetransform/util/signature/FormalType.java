package org.moddingx.sourcetransform.util.signature;

import java.util.ArrayList;
import java.util.List;

public class FormalType {
    
    public final String name;
    public SignatureNode classBound = null;
    public List<SignatureNode> interfaceBounds = new ArrayList<>();

    public FormalType(String name) {
        this.name = name;
    }
}
