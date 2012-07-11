/**
 *
 * Sonar Ada Plugin
 * Copyright AdaCore, 2012
 */
package org.sonar.plugins.ada;

import org.sonar.api.batch.AbstractSourceImporter;

public class AdaSourceImporter extends AbstractSourceImporter {

    public AdaSourceImporter(Ada lang) {
        super(lang);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
