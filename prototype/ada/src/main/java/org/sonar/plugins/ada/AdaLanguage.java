/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.sonar.plugins.ada;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.lang.StringUtils;
import org.sonar.api.resources.AbstractLanguage;

/**
 *
 * @author martin
 */
public class AdaLanguage extends AbstractLanguage{
    static final String DEFAULT_FILE_SUFFIXES = "abs, adb";
    private Configuration config;
    
    public static final String KEY = "ada";
    
    public AdaLanguage(Configuration config){
        super(KEY, "ada");
        this.config = config;
    }

    public String[] getFileSuffixes() {
        String[] suffixes = config.getStringArray(AdaPlugin.FILE_SUFFIXES_KEY);
    if (suffixes == null || suffixes.length == 0) {
      suffixes = StringUtils.split(DEFAULT_FILE_SUFFIXES, ",");
    }
    return suffixes;
    }
    
}
