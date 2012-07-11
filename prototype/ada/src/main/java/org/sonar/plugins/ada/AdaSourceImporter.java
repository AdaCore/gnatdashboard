/**
 *
 * Sonar Ada Plugin
 */
package org.sonar.plugins.ada;

import org.sonar.api.batch.AbstractSourceImporter;
import org.sonar.api.batch.Phase;

/**
 *
 * @author martin
 */
public class AdaSourceImporter extends AbstractSourceImporter {

    public AdaSourceImporter(Ada lang) {
        super(lang);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
