/**
 *
 * Sonar Ada Plugin
 */
package org.sonar.plugins.ada.gnatcheck;

import org.sonar.plugins.ada.utils.AdaAbstractRuleRepository;

/**
 *
 * @author martin
 */
public class AdaGnatCheckRuleRepository extends AdaAbstractRuleRepository {

    static final String KEY = "gnatcheck";

    public AdaGnatCheckRuleRepository() {
        super(KEY);
        setName(KEY);
    }

    protected String fileName() {
        return "/gnatcheck.xml";
    }
}
