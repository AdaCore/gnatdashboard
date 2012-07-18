/**
 *  Sonar Ada Plugin
 *  Copyright (C) 2001-2012, AdaCore
 */
package org.sonar.plugins.ada;

import java.io.File;
import java.util.List;
import org.sonar.api.batch.AbstractSourceImporter;
import org.sonar.api.batch.Phase;
import org.sonar.api.batch.ResourceCreationLock;
import org.sonar.api.resources.JavaFile;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.ada.utils.AdaUtils;

@Phase(name = Phase.Name.PRE)
public class AdaSourceImporter extends AbstractSourceImporter {

    private ResourceCreationLock lock;

    public AdaSourceImporter(ResourceCreationLock lock) {
        super(Ada.INSTANCE);
        this.lock = lock;
    }

    @Override
    protected Resource createResource(File file, List<File> sourceDirs, boolean unitTest) {
        AdaFile adaFile = file != null ? AdaFile.fromIOFile(file, sourceDirs) : null;
        if (adaFile != null) {
            AdaUtils.LOG.info(adaFile.toString());
        }
        return adaFile;
    }

    @Override
    protected void onFinished() {
        lock.lock();
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
