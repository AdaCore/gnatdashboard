/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.gnatcheck;

import org.sonar.plugins.ada.utils.AdaAbstractRuleRepository;

/**
 * Represent GNAT Check rule repository.
 */
public class AdaGnatCheckRuleRepository extends AdaAbstractRuleRepository {

    static final String KEY = "gnatcheck";

    public AdaGnatCheckRuleRepository() {
        super(KEY);
        setName(KEY);
    }

    /**
     * Return location of GNAT Check rule repository, from resource directory.
     *
     * No default severity set in the rule repository.
     * GNAT Check rules default severity is set to MAJOR in the Sonar Ada
     * default profile.
     */
    protected String fileName() {
        return "/gnatcheck.xml";
    }
}
