/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada;

import org.sonar.api.profiles.ProfileDefinition;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.profiles.XMLProfileParser;
import org.sonar.api.utils.ValidationMessages;

public class AdaDefaultProfile extends ProfileDefinition {

    private XMLProfileParser xmlProfileParser;

    public AdaDefaultProfile(XMLProfileParser xmlProfileParser) {
        this.xmlProfileParser = xmlProfileParser;
    }

    /**
     * Import the default sonar Ada profile
     */
    @Override
    public RulesProfile createProfile(ValidationMessages messages) {
        RulesProfile profile = xmlProfileParser.parseResource(getClass().getClassLoader(),
                "default-profile.xml", messages);
        profile.setDefaultProfile(true);
        profile.setProvided(true);
        return profile;
    }
}
