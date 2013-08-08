/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gcov;

import org.sonar.plugins.ada.api.resource.AdaFile;


public class CoverageData {

    private AdaFile resource;
    private Integer line;
    private Integer hits;

    public CoverageData(AdaFile resource, Integer line, Integer hits) {
        this.resource = resource;
        this.line = line;
        this.hits = hits;
    }

    public AdaFile getResource() {
        return resource;
    }

    public void setResource(AdaFile resource) {
        this.resource = resource;
    }

    public Integer getLine() {
        return line;
    }

    public void setLine(Integer line) {
        this.line = line;
    }

    public Integer getHits() {
        return hits;
    }

    public void setHits(Integer hits) {
        this.hits = hits;
    }


}
