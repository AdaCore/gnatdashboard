/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.tokenizer;

import org.sonar.colorizer.InlineDocTokenizer;

public class AdaCommentTokenizer extends InlineDocTokenizer {

    public AdaCommentTokenizer(String tagBefore, String tagAfter) {
        super("--", tagBefore, tagAfter);
    }
}
