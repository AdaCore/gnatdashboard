/**
 * Sonar Ada Plugin (GNATdashboard)
 * Copyright (C) 2013-2015, AdaCore
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

import lombok.AllArgsConstructor;
import org.sonar.channel.CodeReader;
import org.sonar.channel.EndMatcher;
import org.sonar.colorizer.HtmlCodeBuilder;
import org.sonar.colorizer.Tokenizer;

/**
 * Ada source code comment tokenizer.
 *
 * ??? Review this class and remove deprecated code.
 */
@AllArgsConstructor
public class AdaLiteralTokenizer extends Tokenizer {

    private final String tagBefore;
    private final String tagAfter;

    public AdaLiteralTokenizer() {
        this("", "");
    }

    @Override
    public boolean consume(CodeReader code, HtmlCodeBuilder codeBuilder) {
        if (code.peek() == '\"') {
            codeBuilder.appendWithoutTransforming(tagBefore);
            int firstChar = code.peek();
            code.popTo(
                    new AdaLiteralTokenizer.EndCommentMatcher(firstChar, code),
                    codeBuilder);
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
            return code.lastChar() == firstChar &&
                evenNumberOfBackSlashBeforeDelimiter() &&
                literalValue.length() > 1;
        }

        private boolean evenNumberOfBackSlashBeforeDelimiter() {
            int numberOfBackSlashChar = 0;
            for (int index = literalValue.length() - 3; index >= 0; --index) {
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
