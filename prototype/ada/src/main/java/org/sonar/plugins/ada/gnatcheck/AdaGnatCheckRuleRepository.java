/**
 *
 * Sonar Ada Plugin
 * Copyright AdaCore, 2012
 */
package org.sonar.plugins.ada.gnatcheck;

import org.sonar.plugins.ada.utils.AdaAbstractRuleRepository;

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
