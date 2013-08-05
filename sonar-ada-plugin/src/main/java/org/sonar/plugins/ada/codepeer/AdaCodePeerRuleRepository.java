/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.codepeer;

import org.sonar.plugins.ada.utils.AdaAbstractRuleRepository;

/**
 * Represent CodePeer rule repository.
 */
public class AdaCodePeerRuleRepository extends AdaAbstractRuleRepository {

    public static final String KEY = "codepeer";
    public static final String RULE_KEY_SEPARATOR = "__";

    public AdaCodePeerRuleRepository() {
        super(KEY);
        setName(KEY);
    }

    /**
     * Return location of CodePeer rule repository, from resource directory.
     *
     * For now, as it not possible to set a severity to a violation, 4 rules
     * has been created for every CodePeer rule corresponding to each Sonar
     * severity.
     */
    @Override
    protected String fileName() {
        return "/codepeer.xml";
    }
}
