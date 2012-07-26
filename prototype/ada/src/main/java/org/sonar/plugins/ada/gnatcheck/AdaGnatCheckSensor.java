/**
 *
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.gnatcheck;

import java.io.File;
import javax.xml.stream.XMLStreamException;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.lang.StringUtils;
import org.codehaus.staxmate.in.SMHierarchicCursor;
import org.codehaus.staxmate.in.SMInputCursor;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.resources.Project;
import org.sonar.api.rules.RuleFinder;
import org.sonar.api.utils.StaxParser;
import org.sonar.plugins.ada.utils.AdaSensor;
import org.sonar.plugins.ada.utils.AdaUtils;

/**
 * Sensor for GnatCheck external tool, retrieve informations from a report
 * to create violations.
 */
public class AdaGnatCheckSensor extends AdaSensor {

    public static final String REPORT_PATH_KEY = "sonar.ada.gnatcheck.reportPath";

    public AdaGnatCheckSensor(RuleFinder ruleFinder, Configuration conf,
            RulesProfile profile) {
        super(ruleFinder, conf);
    }

    @Override
    protected String reportPathKey() {
        return REPORT_PATH_KEY;
    }


    @Override
    protected void processReport(final Project project, final SensorContext context, File report)
    throws javax.xml.stream.XMLStreamException
  {
    StaxParser parser = new StaxParser(new StaxParser.XmlStreamHandler() {
      /**
       * {@inheritDoc}
       */
      public void stream(SMHierarchicCursor rootCursor) throws XMLStreamException {
        rootCursor.advance(); //results

        SMInputCursor errorCursor = rootCursor.childElementCursor("error"); //error
        while (errorCursor.getNext() != null) {
          String file = errorCursor.getAttrValue("file");
          String line = errorCursor.getAttrValue("line");
          String id = errorCursor.getAttrValue("id");
          String msg = errorCursor.getAttrValue("msg");
          String prj = errorCursor.getAttrValue("project");
          String dir = errorCursor.getAttrValue("directory");
          if(isInputValid(file, line, id, msg)) {
            saveViolation(project, context, AdaGnatCheckRuleRepository.KEY,
                        file, Integer.parseInt(line), id, msg, prj, dir);
          } else {
            AdaUtils.LOG.warn("AdaCheck warning: {}", msg );
          }
        }
      }

      private boolean isInputValid(String file, String line, String id, String msg) {
        return !StringUtils.isEmpty(file) && !StringUtils.isEmpty(line)
          && !StringUtils.isEmpty(id) && !StringUtils.isEmpty(msg);
      }
    });

    parser.parse(report);
  }
}
