/**
 *  Sonar Ada Plugin
 *  Copyright (C) 2001-2012, AdaCore
 */
package org.sonar.plugins.ada;

import java.util.List;
import net.sourceforge.pmd.cpd.SourceCode;
import net.sourceforge.pmd.cpd.TokenEntry;
import net.sourceforge.pmd.cpd.Tokenizer;
import net.sourceforge.pmd.cpd.Tokens;

public class AdaTokenizer implements Tokenizer {
     public void tokenize(SourceCode tokens, Tokens tokenEntries) {
    List<String> code = tokens.getCode();
    for (int i = 0; i < code.size(); i++) {
      String currentLine = (String) code.get(i);
      for (int j = 0; j < currentLine.length(); j++) {
        char tok = currentLine.charAt(j);
        if ( !Character.isWhitespace(tok) && tok != '{' && tok != '}' && tok != ';') {
          tokenEntries.add(new TokenEntry(String.valueOf(tok), tokens.getFileName(), i + 1));
        }
      }
    }
    tokenEntries.add(TokenEntry.getEOF());
  }
}
