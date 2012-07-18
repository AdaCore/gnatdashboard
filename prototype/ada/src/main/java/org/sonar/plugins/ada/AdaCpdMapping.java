/**
 *  Sonar Ada Plugin
 *  Copyright (C) 2001-2012, AdaCore
 */
package org.sonar.plugins.ada;

import java.io.File;
import java.util.List;
import net.sourceforge.pmd.cpd.Tokenizer;
import org.sonar.api.batch.AbstractCpdMapping;
import org.sonar.api.resources.Language;

public class AdaCpdMapping extends AbstractCpdMapping {

    @Override
    public AdaFile createResource(File file, List<File> sourceDirs) {
        return AdaFile.fromIOFile(file, sourceDirs);
    }

    public Language getLanguage() {
        return Ada.INSTANCE;
    }

    public Tokenizer getTokenizer() {
        return new AdaTokenizer();
    }
}
