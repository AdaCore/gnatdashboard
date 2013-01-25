/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada;

import java.util.ArrayList;
import java.util.List;
import org.sonar.api.web.CodeColorizerFormat;
import org.sonar.colorizer.KeywordsTokenizer;
import org.sonar.colorizer.Tokenizer;
import org.sonar.plugins.ada.tokenizer.AdaCommentTokenizer;
import org.sonar.plugins.ada.tokenizer.AdaLiteralTokenizer;

/**
 * {@inheritDoc}
 */
public class AdaColorizer extends CodeColorizerFormat {

    /**
     * Ada reserved key word, used for syntax highlighting
     */
    private final String[] KEYWORDS = {
        "abort", "abs", "abstract", "accept", "access", "aliased", "all",
        "and", "array", "at", "begin", "body", "case", "constant", "declare",
        "delay", "delta", "digits", "do", "else", "elsif", "end", "entry", "exception",
        "exit", "for", "function", "generic", "goto", "if", "in", "interface", "is",
        "limited", "loop", "mod", "new", "not", "null", "others", "out", "of", "or",
        "overriding", "package", "pragma", "private", "procedure", "protected", "raise",
        "range", "record", "rem", "renames", "requeue", "return", "reverse", "select",
        "separate", "some", "subtype", "synchronized", "tagged", "task", "terminate",
        "then", "type", "until", "use", "when", "while", "with", "xor"
    };
    private List<Tokenizer> tokenizers;

    /**
     * {@inheritDoc}
     */
    public AdaColorizer() {
        super(Ada.KEY);
    }

    /**
     * sonar-colorizer tokenizers for HTML output.
     *
     * @return a not null list (empty if no tokenizers)
     */
    @Override
    public List<Tokenizer> getTokenizers() {
        if (tokenizers == null) {
            String tagAfter = "</span>";
            tokenizers = new ArrayList<Tokenizer>();
            tokenizers.add(new AdaLiteralTokenizer("<span class=\"s\">", tagAfter));
            tokenizers.add(new KeywordsTokenizer("<span class=\"k\">", tagAfter, KEYWORDS));
            tokenizers.add(new AdaCommentTokenizer("<span class=\"c\">", tagAfter));
        }
        return tokenizers;
    }
}
