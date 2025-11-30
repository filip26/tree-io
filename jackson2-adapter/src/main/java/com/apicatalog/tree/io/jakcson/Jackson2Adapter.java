package com.apicatalog.tree.io.jakcson;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.apicatalog.tree.io.Tree.Capability;
import com.apicatalog.tree.io.Tree.Features;
import com.apicatalog.tree.io.Tree.NodeType;
import com.apicatalog.tree.io.TreeAdapter;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * A {@link TreeAdapter} implementation that bridges the generic tree processing
 * framework with the Jackson 2 {@link JsonNode} object model.
 * <p>
 * This adapter allows for the traversal and inspection of JSON structures that
 * have been parsed into Jackson's native tree representation. It correctly
 * identifies JSON objects as maps (with {@link String} keys) and JSON arrays as
 * collections.
 * </p>
 * <p>
 * The class is implemented as a stateless singleton, accessible via the
 * {@link #instance()} method.
 * </p>
 */
public class Jackson2Adapter implements TreeAdapter {

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
                    NodeType.NULL),
            // capabilities
            Set.of(Capability.SCALAR_OBJECT_EQUALS));

    static final Jackson2Adapter INSTANCE = new Jackson2Adapter();

    /**
     * Provides the singleton instance of the {@code Jackson2Adapter}.
     * 
     * @return the singleton instance
     */
    public static Jackson2Adapter instance() {
        return INSTANCE;
    }

    @Override
    public Features features() {
        return FEATURES;
    }

    /**
     * {@inheritDoc}
     * <p>
     * This implementation returns {@code true} for instances of {@link JsonNode}
     * (representing values) and {@link String} (representing map keys).
     * </p>
     */
    @Override
    public boolean isNode(Object node) {
        return node != null && (node instanceof JsonNode || node instanceof String);
    }

    /**
     * {@inheritDoc}
     * <p>
     * For {@link String} objects, this method returns {@link NodeType#STRING}, as
     * they are treated as map keys. Otherwise, it determines the type from the
     * {@link JsonNode}'s internal node type.
     * </p>
     */
    @Override
    public NodeType type(Object node) {
        // property keys are strings
        if (node instanceof String) {
            return NodeType.STRING;
        }

        // all other values
        switch (((JsonNode) node).getNodeType()) {
        case NULL:
        case MISSING:
            return NodeType.NULL;
        case BOOLEAN:
            return ((JsonNode) node).asBoolean() ? NodeType.TRUE : NodeType.FALSE;
        case STRING:
            return NodeType.STRING;
        case NUMBER:
            return NodeType.NUMBER;
        case ARRAY:
            return NodeType.SEQUENCE;
        case OBJECT:
            return NodeType.MAP;
        case BINARY:
            return NodeType.BINARY;
        default:
            throw new IllegalArgumentException("Unsupported JsonNode type.");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> keys(Object node) {
        return ((ObjectNode) node).propertyStream().map(Map.Entry::getKey).collect(Collectors.toCollection(LinkedHashSet::new));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Stream<String> keyStream(Object node) {
        return ((ObjectNode) node).propertyStream().map(Map.Entry::getKey);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public JsonNode property(Object key, Object node) {
        return ((ObjectNode) node).get((String) key);
    }

    @Override
    public JsonNode property(Object key, TreeAdapter keyAdapter, Object node) {
        return property(keyAdapter.asString(key), node);
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Iterable<Entry<?, ?>> entries(Object node) {
        return (Iterable) ((ObjectNode) node).properties();
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Stream<Entry<?, ?>> entryStream(Object node) {
        return (Stream) ((ObjectNode) node).propertyStream();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<JsonNode> elements(Object node) {
        return ((ArrayNode) node).valueStream().collect(Collectors.toList());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Stream<JsonNode> elementStream(Object node) {
        return ((ArrayNode) node).valueStream();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String stringValue(Object node) {
        if (node instanceof String) {
            return (String) node;
        }
        return ((JsonNode) node).textValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int intValue(Object node) {
        return ((JsonNode) node).intValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long longValue(Object node) {
        return ((JsonNode) node).longValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BigInteger integerValue(Object node) {
        return ((JsonNode) node).bigIntegerValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double doubleValue(Object node) {
        return ((JsonNode) node).doubleValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BigDecimal decimalValue(Object node) {
        return ((JsonNode) node).decimalValue();
    }

    /**
     * {@inheritDoc}
     * <p>
     * This implementation wraps the potential {@link IOException} from Jackson in
     * an {@link IllegalStateException}.
     * </p>
     */
    @Override
    public byte[] binaryValue(Object node) {
        try {
            return ((JsonNode) node).binaryValue();
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings({ "unchecked" })
    @Override
    public Collection<JsonNode> asIterable(Object node) {
        if (node == null) {
            return Collections.emptySet();
        }
        if (node instanceof Collection) {
            return (Collection<JsonNode>) node;
        }
        if (node instanceof ArrayNode) {
            return ((ArrayNode) node).valueStream().collect(Collectors.toList());
        }
        if (node instanceof Stream) {
            return ((Stream<JsonNode>) node).collect(Collectors.toList());
        }
        return Collections.singleton((JsonNode) node);
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings({ "unchecked" })
    @Override
    public Stream<JsonNode> asStream(Object node) {
        if (node == null) {
            return Stream.empty();
        }
        if (node instanceof Collection) {
            return ((Collection<JsonNode>) node).stream();
        }
        if (node instanceof Stream) {
            return (Stream<JsonNode>) node;
        }
        if (node instanceof ArrayNode) {
            return ((ArrayNode) node).valueStream();
        }
        return Stream.of((JsonNode) node);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Treats both {@code null} nodes and Jackson's {@code missingNode} as null.
     * </p>
     */
    @Override
    public boolean isNull(Object node) {
        return node == null
                || (node instanceof JsonNode
                        && (((JsonNode) node).isNull()));
//                                || ((JsonNode) node).isMissingNode()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isBoolean(Object node) {
        return node != null
                && node instanceof JsonNode
                && ((JsonNode) node).isBoolean();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isMap(Object node) {
        return node != null
                && node instanceof JsonNode
                && ((JsonNode) node).isObject();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSequence(Object node) {
        return node != null
                && node instanceof JsonNode
                && ((JsonNode) node).isArray();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isString(Object node) {
        return node != null
                && (node instanceof String
                        || (node instanceof JsonNode
                                && ((JsonNode) node).isTextual()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isNumber(Object node) {
        return node != null
                && node instanceof JsonNode
                && ((JsonNode) node).isNumber();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isIntegral(Object node) {
        return node != null
                && node instanceof JsonNode
                && ((JsonNode) node).isIntegralNumber();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isBinary(Object node) {
        return node != null
                && node instanceof JsonNode
                && ((JsonNode) node).isBinary();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEmpty(Object node) {
        Objects.requireNonNull(node);

        if (node instanceof ObjectNode) {
            return ((ObjectNode) node).isEmpty();
        }
        if (node instanceof ArrayNode) {
            return ((ArrayNode) node).isEmpty();
        }
        throw new ClassCastException("Node must be an ObjectNode or an ArrayNode.");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int size(Object node) {
        Objects.requireNonNull(node);

        if (node instanceof ObjectNode) {
            return ((ObjectNode) node).size();
        }
        if (node instanceof ArrayNode) {
            return ((ArrayNode) node).size();
        }
        throw new ClassCastException("Node must be an ObjectNode or an ArrayNode.");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String asString(Object node) {
        if (node instanceof String) {
            return (String) node;
        }
        if (node instanceof JsonNode) {
            return ((JsonNode) node).asText();
        }
        return Objects.toString(node);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BigDecimal asDecimal(Object node) {
        return ((JsonNode) node).decimalValue();
    }
}
