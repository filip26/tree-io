package com.apicatalog.tree.io;

import java.io.IOException;
import java.io.OutputStream;

public interface TreeIOWriter {

    void write(TreeIO node, OutputStream os) throws IOException;

}
