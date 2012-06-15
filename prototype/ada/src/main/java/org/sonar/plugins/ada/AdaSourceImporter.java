/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.sonar.plugins.ada;

import org.sonar.api.batch.AbstractSourceImporter;

/**
 *
 * @author martin
 */
public class AdaSourceImporter extends AbstractSourceImporter {

    public AdaSourceImporter(AdaLanguage lang) {
        super(lang);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
