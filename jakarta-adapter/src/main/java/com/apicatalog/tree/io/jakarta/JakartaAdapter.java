package com.apicatalog.tree.io.jakarta;

import java.util.Set;

import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeType;

class JakartaAdapter {

    static final Features FEATURES = new Features(
            // keys
            Set.of(NodeType.STRING),
            // nodes
            Set.of(
                    NodeType.SEQUENCE,
                    NodeType.MAP,
                    NodeType.NUMBER,
                    NodeType.STRING,
                    NodeType.FALSE,
                    NodeType.TRUE,
                    NodeType.NULL));
}
