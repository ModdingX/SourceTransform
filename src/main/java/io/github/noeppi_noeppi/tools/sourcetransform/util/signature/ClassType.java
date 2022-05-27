package io.github.noeppi_noeppi.tools.sourcetransform.util.signature;

import java.util.ArrayList;
import java.util.List;

public class ClassType {
    
    public final String desc;
    // Null elements: wildcard
    public List<SignatureNode> arguments = new ArrayList<>();

    public ClassType(String desc) {
        this.desc = desc;
    }
}
