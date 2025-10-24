package com.apicatalog.tree.io.jakarta;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Stream;

import com.apicatalog.tree.io.Features;
import com.apicatalog.tree.io.NodeAdapter;
import com.apicatalog.tree.io.NodeType;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

/**
 * A {@link NodeAdapter} implementation that bridges the generic tree processing
 * framework with the Jakarta JSON-P {@link JsonValue} object model.
 * <p>
 * This adapter allows for the traversal and inspection of JSON structures that
 * have been parsed into Jakarta's native tree representation. It correctly
 * identifies JSON objects ({@link JsonObject}) as maps and JSON arrays
 * ({@link JsonArray}) as collections. Map keys are always {@link String}s.
 * </p>
 * <p>
 * Note that the standard Jakarta JSON-P API does not provide a native
 * representation for binary data; therefore, this adapter does not support the
 * {@code BINARY} node type.
 * </p>
 * <p>
 * The class is implemented as a stateless singleton, accessible via the
 * {@link #instance()} method.
 * </p>
 */
public class JakartaAdapter implements NodeAdapter {

    static final Set<NodeType> NODES = new HashSet<>(Arrays.asList(
            NodeType.COLLECTION,
            NodeType.MAP,
            NodeType.NUMBER,
            NodeType.STRING,
            NodeType.FALSE,
            NodeType.TRUE,
            NodeType.NULL));

    static final Set<NodeType> KEYS = Collections.singleton(NodeType.STRING);

    static final Features FEATURES = new Features(NODES, KEYS);

    static final JakartaAdapter INSTANCE = new JakartaAdapter();

    /**
     * Provides the singleton instance of the {@code JakartaAdapter}.
     * 
     * @return the singleton instance
     */
    public static JakartaAdapter instance() {
        return INSTANCE;
    }

    @Override
    public Features features() {
        return FEATURES;
    }

    /**
     * {@inheritDoc}
     * <p>
     * This implementation returns {@code true} for instances of {@link JsonValue}
     * (representing values) and {@link String} (representing map keys).
     * </p>
     */
    @Override
    public boolean isNode(Object node) {
        return node != null && (node instanceof JsonValue || node instanceof String);
    }

    /**
     * {@inheritDoc}
     * <p>
     * For {@link String} objects, this method returns {@link NodeType#STRING}, as
     * they are treated as map keys. Otherwise, it determines the type from the
     * {@link JsonValue}'s {@link ValueType}.
     * </p>
     */
    @Override
    public NodeType type(Object node) {
        // null values are allowed
        if (node == null) {
            return NodeType.NULL;
        }
        
        // property keys are strings
        if (node instanceof String) {
            return NodeType.STRING;
        }

        // all other values
        switch (((JsonValue) node).getValueType()) {
        case NULL:
            return NodeType.NULL;
        case TRUE:
            return NodeType.TRUE;
        case FALSE:
            return NodeType.FALSE;
        case STRING:
            return NodeType.STRING;
        case NUMBER:
            return NodeType.NUMBER;
        case ARRAY:
            return NodeType.COLLECTION;
        case OBJECT:
            return NodeType.MAP;
        default:
            throw new IllegalArgumentException("Unsupported JsonValue type.");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> keys(Object node) {
        return ((JsonObject) node).keySet();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public JsonValue property(Object property, Object node) {
        return ((JsonObject) node).get((String) property);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public JsonValue property(Object key, NodeAdapter keyAdapter, Object node) {
        return property(keyAdapter.asString(key), node);
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Iterable<Entry<?, ?>> entries(Object node) {
        return (Iterable) ((JsonObject) node).entrySet();
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Stream<Entry<?, ?>> entryStream(Object node) {
        return (Stream) ((JsonObject) node).entrySet().stream();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Iterable<JsonValue> elements(Object node) {
        return (JsonArray) node;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Stream<JsonValue> elementStream(Object node) {
        return ((JsonArray) node).stream();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String stringValue(Object node) {
        if (node instanceof String) {
            return (String) node;
        }
        return ((JsonString) node).getString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int intValue(Object node) {
        return ((JsonNumber) node).intValueExact();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long longValue(Object node) {
        return ((JsonNumber) node).longValueExact();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BigInteger bigIntegerValue(Object node) {
        return ((JsonNumber) node).bigIntegerValueExact();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double doubleValue(Object node) {
        return ((JsonNumber) node).doubleValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BigDecimal decimalValue(Object node) {
        return ((JsonNumber) node).bigDecimalValue();
    }

    /**
     * {@inheritDoc}
     * <p>
     * This operation is not supported as the Jakarta JSON-P API does not provide a
     * native binary type.
     * </p>
     * 
     * @throws UnsupportedOperationException always
     */
    @Override
    public byte[] binaryValue(Object node) {
        throw new UnsupportedOperationException("Jakarta JSON-P does not support a native binary type.");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<JsonValue> asIterable(Object node) {
        if (node == null) {
            return Collections.emptyList();
        }
        if (node instanceof JsonArray) {
            return ((JsonArray) node);
        }
        return Collections.singletonList((JsonValue) node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Stream<JsonValue> asStream(Object node) {
        if (node == null) {
            return Stream.empty();
        }
        if (node instanceof JsonArray) {
            return ((JsonArray) node).stream();
        }
        return Stream.of((JsonValue) node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isNull(Object node) {
        return node == null
                || (node instanceof JsonValue
                        && ValueType.NULL.equals(((JsonValue) node).getValueType()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isBoolean(Object node) {
        return node instanceof JsonValue
                && (ValueType.TRUE.equals(((JsonValue) node).getValueType())
                        || ValueType.FALSE.equals(((JsonValue) node).getValueType()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isMap(Object node) {
        return node instanceof JsonObject;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isCollection(Object node) {
        return node instanceof JsonArray;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Always returns {@code false} as {@link JsonArray} does not enforce element
     * uniqueness.
     * </p>
     */
    @Override
    public boolean isSet(Object node) {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isList(Object node) {
        return node instanceof JsonArray;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isString(Object node) {
        return node instanceof String || node instanceof JsonString;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isNumber(Object node) {
        return node instanceof JsonNumber;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isIntegral(Object node) {
        return node instanceof JsonNumber && ((JsonNumber) node).isIntegral();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Always returns {@code false} as the Jakarta JSON-P API does not provide a
     * native binary type.
     * </p>
     */
    @Override
    public boolean isBinary(Object node) {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEmpty(Object node) {
        return isEmptyCollection(node) || isEmptyMap(node);
    }

    @Override
    public boolean isEmptyCollection(Object node) {
        return node instanceof JsonArray && ((JsonArray) node).isEmpty();
    }

    @Override
    public boolean isEmptyMap(Object node) {
        return node instanceof JsonObject && ((JsonObject) node).isEmpty();
    }

    @Override
    public boolean isTrue(Object node) {
        return node instanceof JsonValue && ((JsonValue) node).getValueType() == ValueType.TRUE;
    }

    @Override
    public boolean isFalse(Object node) {
        return node instanceof JsonValue && ((JsonValue) node).getValueType() == ValueType.FALSE;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int size(Object node) {
        if (node instanceof JsonObject) {
            return ((JsonObject) node).size();
        }
        if (node instanceof JsonArray) {
            return ((JsonArray) node).size();
        }
        throw new ClassCastException("Node must be a JsonObject or a JsonArray.");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String asString(Object node) {
        if (node instanceof String) {
            return (String) node;
        }
        if (node instanceof JsonString) {
            return ((JsonString) node).getString();
        }
        return Objects.toString(node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BigDecimal asDecimal(Object node) {
        if (node instanceof JsonNumber) {
            return ((JsonNumber) node).bigDecimalValue();
        }
        throw new IllegalArgumentException("Node must be a JsonNumber.");
    }
}
