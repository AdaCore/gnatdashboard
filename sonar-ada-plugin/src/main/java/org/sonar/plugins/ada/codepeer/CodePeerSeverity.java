/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.codepeer;

import org.sonar.api.rule.Severity;

public enum CodePeerSeverity {

    HIGH(Severity.CRITICAL),
    MEDIUM(Severity.MAJOR),
    LOW(Severity.MINOR);

    private final String sonarSeverity;

    private CodePeerSeverity(String sonarSeverity){
        this.sonarSeverity = sonarSeverity;
    }

    public String getSonarSeverity(){
        return this.sonarSeverity;
    }
}
