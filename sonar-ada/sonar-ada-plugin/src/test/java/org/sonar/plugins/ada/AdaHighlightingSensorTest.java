package org.sonar.plugins.ada;

import com.google.common.base.Charsets;
import org.apache.commons.io.FileUtils;
import org.junit.Test;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.internal.TestInputFileBuilder;
import org.sonar.api.batch.sensor.internal.SensorContextTester;
import org.sonar.plugins.ada.lang.AdaHighlightingSensor;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;

public class AdaHighlightingSensorTest {

    @Test
    public void noOverlapsHighlightingStringWithSimilarContent() throws IOException {
        String dir = "src/test/resources/test_sources";
        File file = new File(dir, "/SourceWithSimilarStringsOnSameLine.adb");
        InputFile inputFile = new TestInputFileBuilder("foo", "SourceWithSimilarStringsOnSameLine.adb")
                .setLanguage("ada")
                .initMetadata(FileUtils.readFileToString(file, Charsets.UTF_8))
                .setModuleBaseDir(Paths.get(dir))
                .build();
        SensorContextTester context = SensorContextTester.create(new File(dir));
        context.fileSystem().add(inputFile);

        AdaHighlightingSensor highlighter = new AdaHighlightingSensor();
        //  Running highlighter will cause exception if it has overlapping highlightings
        highlighter.execute(context);
    }

}
