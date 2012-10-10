/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.codepeer;

import org.sonar.plugins.ada.utils.AdaAbstractRuleRepository;

public class AdaCodepeerRuleRepository extends AdaAbstractRuleRepository {

    static final String KEY = "codepeer";

    public AdaCodepeerRuleRepository() {
        super(KEY);
        setName(KEY);
    }

    protected String fileName() {
        return "/codepeer.xml";
    }
}
