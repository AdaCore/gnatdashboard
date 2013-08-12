/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatprove;

import org.sonar.plugins.ada.utils.AdaAbstractRuleRepository;

/**
 * Represent GNATcheck rule repository.
 */
public class GNATproveRuleRepository extends AdaAbstractRuleRepository {

    static final String KEY = "gnatprove";

    public GNATproveRuleRepository() {
        super(KEY);
        setName(KEY);
    }

    /**
     * Return location of GNATcheck rule repository, from resource directory.
     *
     * No default severity set in the rule repository.
     * GNATcheck rules default severity is set to MAJOR in the Sonar Ada
     * default profile.
     */
    protected String fileName() {
        return "/gnatprove.xml";
    }
}
