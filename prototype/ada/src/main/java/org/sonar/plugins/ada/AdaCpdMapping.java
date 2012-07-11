/**
 *
 * Sonar Ada Plugin
 * Copyright AdaCore, 2012
 */
package org.sonar.plugins.ada;

import java.io.File;
import java.util.List;

import net.sourceforge.pmd.cpd.Tokenizer;

import org.sonar.api.batch.AbstractCpdMapping;
import org.sonar.api.resources.Language;

public class AdaCpdMapping extends AbstractCpdMapping {

    private Ada lang;

    public AdaCpdMapping(Ada lang){
        this.lang = lang;
    }

    public Language getLanguage() {
        return lang;
    }

    public Tokenizer getTokenizer() {
        return new AdaTokenizer();
    }
}