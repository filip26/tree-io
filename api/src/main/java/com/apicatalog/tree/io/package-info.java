/**
 * Provides a flexible, format-agnostic framework for processing tree-like data
 * structures. This package is designed to decouple data processing logic from
 * any specific data format (e.g., JSON, YAML, CBOR) or underlying
 * implementation library.
 *
 * <h3>Core Design</h3> The framework is built upon three primary abstractions
 * that work in concert to enable data transformation workflows:
 * 
 * <ol>
 * <li><b>{@link com.apicatalog.tree.io.NodeAdapter}: The Reader</b><br>
 * Acts as a read-only "view" or "wrapper" over a native tree structure. It
 * translates a specific format into a generic interface that the rest of the
 * framework can understand.</li>
 * <li><b>{@link com.apicatalog.tree.io.NodeGenerator}: The Writer</b><br>
 * Provides a write-only, streaming API for building a new tree structure. It
 * abstracts the destination, which could be anything from a file on disk to an
 * in-memory object model.</li>
 * <li><b>{@link com.apicatalog.tree.io.NodeVisitor}: The Engine</b><br>
 * A stateful, non-recursive iterator that traverses the structure exposed by a
 * {@code NodeAdapter} and drives a {@code NodeGenerator}. It is the engine that
 * connects a source to a destination.</li>
 * </ol>
 *
 * <h3>Key Features</h3>
 * <ul>
 * <li><b>Format-Agnostic:</b> Write processing logic once and apply it to any
 * data format for which a {@code NodeAdapter} exists.</li>
 * <li><b>Decoupled:</b> Application logic does not need to depend on any
 * specific data-binding library.</li>
 * <li><b>Non-Recursive Traversal:</b> The {@code NodeVisitor} uses an
 * iterative, stack-based approach, making it safe for processing very large or
 * deeply nested documents without risking a {@code StackOverflowError}.</li>
 * <li><b>Extensible:</b> Support for new data formats can be added by
 * implementing the core interfaces.</li>
 * </ul>
 */
package com.apicatalog.tree.io;
