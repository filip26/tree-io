package com.apicatalog.tree.io;

import java.io.IOException;
import java.util.ArrayDeque;
import java.util.Deque;

import jakarta.json.Json;
import jakarta.json.JsonValue;

public class JakartaMaterializer extends AbstractNodeConsumer {

    protected JsonValue value;
    protected String key;
    protected final Deque<Object> builders;
 
    
    public JakartaMaterializer() {
        super(new ArrayDeque<>());
        this.value = null;
        this.key = null;
        this.builders = new ArrayDeque<>();
    }

    @Override
    protected void scalar(Context ctx, Object node) throws IOException {
        // TODO Auto-generated method stub
        
    }

    @Override
    protected void beginMap(Context ctx) throws IOException {
        
        // TODO Auto-generated method stub
        
    }

    @Override
    protected void beginCollection(Context ctx) throws IOException {
        // TODO Auto-generated method stub
        
    }

    @Override
    protected void end() throws IOException {
        // TODO Auto-generated method stub
        
    }
    
    public JsonValue value() {
        return value;
    }
}
