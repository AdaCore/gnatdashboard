/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada;

import org.sonar.api.resources.AbstractLanguage;

public class Ada extends AbstractLanguage {

    public static final Ada INSTANCE = new Ada();
    /**
     * Ada key
     */
    public static final String KEY = "ada";
    /**
     * Ada default suffixes file, as suffixes are customizable
     * no specific suffixes are mentioned.
     */
    public static final String[] DEFAULT_SUFFIXES = {};
    /**
     * Ada reserved key word, used for code Colorizer
     */
    public static final String[] ADA_RESERVED_KEYWORDS = {"abort", "abs", "abstract", "accept", "access", "aliased", "all", "and", "array",
        "at", "begin", "body", "case", "constant", "declare", "delay", "delta", "digits", "do", "else", "elsif", "end of line", "end", "entry",
        "exception", "exit", "for", "function", "generic", "goto", "if", "in", "is", "limited", "loop", "mod", "new", "not", "null", "of",
        "or", "other control function", "others", "out", "package", "pair of quotation mark", "pragma", "private", "procedure", "protected",
        "quotation mark", "raise", "range", "record", "rem", "renames", "requeue", "return", "reverse", "select", "separate", "subtype",
        "tagged", "task", "terminate", "then", "type", "until", "use", "when", "while", "with", "xor",};
    /**
     * Is used for Source code colorizer, to be filled
     */
    public static final String[] ADA_RESERVED_VARIABLES = {};

    /**
     * Default constructor
     */
    public Ada() {
        super(KEY, "ada");
    }

    /**
     * {@inheritDoc}
     *
     * @see AbstractLanguage#getFileSuffixes()
     */
    public String[] getFileSuffixes() {
        return DEFAULT_SUFFIXES;
    }

}
