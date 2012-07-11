/**
 *
 * Sonar Ada Plugin
 */
package org.sonar.plugins.ada;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.lang.StringUtils;
import org.sonar.api.resources.AbstractLanguage;

/**
 *
 * @author martin
 */
public class Ada extends AbstractLanguage {

    public static final Ada INSTANCE = new Ada();
    /**
     * Ada key
     */
    public static final String KEY = "ada";
    /**
     * Ada files knows suffixes
     */
    static final String[] SUFFIXES = {"abs", "adb"};

    /**
     * Default constructor
     */
    public Ada() {
        super(KEY, "ada");
    }

    /**
     * {@inheritDoc}
     *
     * @see AbstractLanguage#getFileSuffixes()
     */
    public String[] getFileSuffixes() {
        return SUFFIXES;
    }
}
