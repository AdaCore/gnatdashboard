/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.tokenizer;

import org.sonar.channel.CodeReader;
import org.sonar.channel.EndMatcher;
import org.sonar.colorizer.HtmlCodeBuilder;
import org.sonar.colorizer.Tokenizer;

public class AdaLiteralTokenizer extends Tokenizer{

 private final String tagBefore;
  private final String tagAfter;

  public AdaLiteralTokenizer(String tagBefore, String tagAfter) {
    this.tagBefore = tagBefore;
    this.tagAfter = tagAfter;
  }

  public AdaLiteralTokenizer() {
    this("", "");
  }

  @Override
  public boolean consume(CodeReader code, HtmlCodeBuilder codeBuilder) {
    if (code.peek() == '\"') {
      codeBuilder.appendWithoutTransforming(tagBefore);
      int firstChar = code.peek();
      code.popTo(new AdaLiteralTokenizer.EndCommentMatcher(firstChar, code), codeBuilder);
      codeBuilder.appendWithoutTransforming(tagAfter);
      return true;
    } else {
      return false;
    }
  }

  private static class EndCommentMatcher implements EndMatcher {

    private final int firstChar;
    private final CodeReader code;
    private StringBuilder literalValue;

    public EndCommentMatcher(int firstChar, CodeReader code) {
      this.firstChar = firstChar;
      this.code = code;
      literalValue = new StringBuilder();
    }

    public boolean match(int endFlag) {
      literalValue.append((char) endFlag);
      return (code.lastChar() == firstChar && evenNumberOfBackSlashBeforeDelimiter() && literalValue.length() > 1);
    }

    private boolean evenNumberOfBackSlashBeforeDelimiter() {
      int numberOfBackSlashChar = 0;
      for (int index = literalValue.length() - 3; index >= 0; index--) {
        if (literalValue.charAt(index) == '\\') {
          numberOfBackSlashChar++;
        } else {
          break;
        }
      }
      return numberOfBackSlashChar % 2 == 0;
    }
  }
}
