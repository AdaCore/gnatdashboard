/*
 * GNATdashboard
 * Copyright (C) 2016, AdaCore
 *
 * This is free software;  you can redistribute it  and/or modify it  under
 * terms of the  GNU General Public License as published  by the Free Soft-
 * ware  Foundation;  either version 3,  or (at your option) any later ver-
 * sion.  This software is distributed in the hope  that it will be useful,
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
 * License for  more details.  You should have  received  a copy of the GNU
 * General  Public  License  distributed  with  this  software;   see  file
 * COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy
 * of the license.
 */

package org.sonar.plugins.ada.lang;

import com.google.common.collect.ImmutableList;
import org.sonar.api.web.CodeColorizerFormat;
import org.sonar.colorizer.KeywordsTokenizer;
import org.sonar.colorizer.Tokenizer;

import java.util.List;

/**
 * Ada source code colorizer.
 *
 * ??? Review this class and remove deprecated code.
 */
public class AdaColorizer extends CodeColorizerFormat {
  /**
   * Ada reserved key word, used for syntax highlighting
   */
  private final String[] KEYWORDS = {
      "abort", "abs", "abstract", "accept", "access", "aliased", "all",
      "and", "array", "at", "begin", "body", "case", "constant", "declare",
      "delay", "delta", "digits", "do", "else", "elsif", "end", "entry",
      "exception", "exit", "for", "function", "generic", "goto", "if", "in",
      "interface", "is", "limited", "loop", "mod", "new", "not", "null",
      "others", "out", "of", "or", "overriding", "package", "pragma",
      "private", "procedure", "protected", "raise", "range", "record", "rem",
      "renames", "requeue", "return", "reverse", "select", "separate", "some",
      "subtype", "synchronized", "tagged", "task", "terminate", "then",
      "type", "until", "use", "when", "while", "with", "xor"
  };

  /**
   * Lazily generated.
   */
  private List<Tokenizer> tokenizers;

  /**
   * {@inheritDoc}
   */
  public AdaColorizer() {
    super(Ada.KEY);
  }

  /**
   * Generate the HTML tag.
   *
   * @param clazz The CSS class to apply to the <span> tag.
   * @return The opening span tag.
   */
  private String HTMLTagBegin(final String clazz) {
    return String.format("<span class=\"%s\">", clazz);
  }

  /**
   * Tokenizers for HTML output.
   *
   * @return a not null list (empty if no lang)
   */
  @Override
  public List<Tokenizer> getTokenizers() {
    if (tokenizers == null) {
      final String HTMLTagEnd = "</span>";

      tokenizers = ImmutableList.of(
          new AdaLiteralTokenizer(HTMLTagBegin("s"), HTMLTagEnd),
          new KeywordsTokenizer(HTMLTagBegin("k"), HTMLTagEnd, KEYWORDS),
          new AdaCommentTokenizer(HTMLTagBegin("c"), HTMLTagEnd)
      );
    }

    return tokenizers;
  }
}
