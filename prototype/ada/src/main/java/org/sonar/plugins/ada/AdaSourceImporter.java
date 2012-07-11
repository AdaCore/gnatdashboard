/**
 *
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada;

import org.sonar.api.batch.AbstractSourceImporter;
import org.sonar.api.batch.Phase;
import org.sonar.api.batch.ResourceCreationLock;

@Phase(name = Phase.Name.PRE)
public class AdaSourceImporter extends AbstractSourceImporter {

    public AdaSourceImporter(Ada lang) {
        super(lang);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
