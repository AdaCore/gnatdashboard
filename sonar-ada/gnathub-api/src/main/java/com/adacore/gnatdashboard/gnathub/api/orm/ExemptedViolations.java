/*
 * GNATdashboard
 * Copyright (C) 2020, AdaCore
 *
 * This is free software;  you can redistribute it  and/or modify it  under
 * terms of the  GNU General Public License as published  by the Free Soft-
 * ware  Foundation;  either version 3,  or (at your option) any later ver-
 * sion.  This software is distributed in the hope  that it will be useful,
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
 * License for  more details.  You should have  received  a copy of the GNU
 * General  Public  License  distributed  with  this  software;   see  file
 * COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy
 * of the license.
 */

package com.adacore.gnatdashboard.gnathub.api.orm;

        import com.google.common.collect.ImmutableList;
        import lombok.Getter;

        import java.util.List;
        import java.util.stream.Collectors;

public class ExemptedViolations {
    @Getter private final ImmutableList<Issue> exemptedViolations;

    public ExemptedViolations(final List<Issue> issues) {
        this.exemptedViolations = ImmutableList.copyOf(issues);
    }

    public List<Issue> fromToolIgnoreCase(final String toolName) {
        return exemptedViolations.stream()
                .filter(issue -> issue.getTool().equalsIgnoreCase(toolName))
                .collect(Collectors.toList());
    }
}

