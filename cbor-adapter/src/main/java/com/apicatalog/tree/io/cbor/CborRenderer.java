package com.apicatalog.tree.io.cbor;

import java.io.OutputStream;
import java.util.List;

import com.apicatalog.tree.io.TreeAdapter;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.TreeRenderer;

import co.nstant.in.cbor.CborEncoder;
import co.nstant.in.cbor.CborException;
import co.nstant.in.cbor.model.DataItem;

public class CborRenderer implements TreeRenderer {

    @SuppressWarnings("unchecked")
    @Override
    public void render(Object node, TreeAdapter adapter, OutputStream os) throws TreeIOException {

        try {
            if (CborAdapter.instance().isEqualTo(adapter)) {
                if (node instanceof List list) {
                    new CborEncoder(os).encode((List<DataItem>) list);
                    return;
                }
                if (node instanceof DataItem item) {
                    new CborEncoder(os).encode(item);
                    return;
                }
            }

            new CborEncoder(os).encode(CborMaterializer.node(node, adapter));

        } catch (CborException e) {
            throw new TreeIOException(e);
        }
    }

}
