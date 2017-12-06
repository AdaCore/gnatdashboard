package org.sonar.plugins.ada.lang;
import org.apache.commons.lang.StringUtils;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.fs.TextRange;
import org.sonar.api.batch.sensor.Sensor;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.SensorDescriptor;
import org.sonar.api.batch.sensor.highlighting.NewHighlighting;
import org.sonar.api.batch.sensor.highlighting.TypeOfText;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AdaHighlightingSensor implements Sensor {

    /**
     * Ada reserved key word, used for syntax highlighting
     */
    private final String[] KEYWORDS = {
            "abort", "abs", "abstract", "accept", "access", "aliased", "all",
            "and", "array", "at", "begin", "body", "case", "constant", "declare",
            "delay", "delta", "digits", "do", "else", "elsif", "end", "entry",
            "exception", "exit", "for", "function", "generic", "goto", "if", "in",
            "interface", "is", "limited", "loop", "mod", "new", "not", "null",
            "others", "out", "of", "or", "overriding", "package", "pragma",
            "private", "procedure", "protected", "raise", "range", "record", "rem",
            "renames", "requeue", "return", "reverse", "select", "separate", "some",
            "subtype", "synchronized", "tagged", "task", "terminate", "then",
            "type", "until", "use", "when", "while", "with", "xor"
    };


    public AdaHighlightingSensor () {
    }

    /**
     * Perform highlighting on current input file for Ada language elements like comments, keywords and strings
     *
     * @param inputFile current input file
     * @param context sensor context parameter
     */
    private void processFileHighlighting(InputFile inputFile, SensorContext context) {
        final String FILENAME = inputFile.absolutePath();
        try (BufferedReader br = new BufferedReader(new FileReader(FILENAME))) {
            NewHighlighting highlighting = context.newHighlighting().onFile(inputFile);

            String sCurrentLine;
            int lineNb = 0;

            while ((sCurrentLine = br.readLine()) != null) {
                lineNb ++;
                processLine (inputFile, sCurrentLine, lineNb, highlighting);
            }
            highlighting.save();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Processing the current line in order to identify line elements to be highlighted
     *
     * @param inputFile current input file
     * @param line a string parameter containing current line
     * @param lineNb line number in current input file
     * @param highlighting instance of highlighting for the current input file
     */
    private void processLine (InputFile inputFile, String line, int lineNb, NewHighlighting highlighting) {

        if (!line.isEmpty()) {
            // handle Ada comments
            if (line.startsWith("--") || line.contains("--")){
                commentHighlighting (inputFile, line, lineNb, highlighting);
            } else {
                // handle Ada keywords
                if (containsKeywords(line)) {
                    keywordsHighlighting (inputFile, line, lineNb, highlighting);
                }

                // handle Ada strings
                if (line.contains("\"")){
                    stringsHighlighting (inputFile, line, lineNb, highlighting);
                }
            }

        }
    }

    /**
     * Handle Ada strings highlighting for the strings found between " " delimiters in the current line of the given
     * input file
     */
    private void stringsHighlighting(InputFile inputFile, String line, int lineNb, NewHighlighting highlighting) {
        TypeOfText stringType = TypeOfText.STRING;

        // get strings values between "" as a string array
        String[] valuesInQuotes = StringUtils.substringsBetween(line , "\"", "\"");
        if (valuesInQuotes != null) {
            for (String stringVal : valuesInQuotes) {
                if (!stringVal.isEmpty()){
                    int stPos = subStringIndex(line, stringVal);
                    int endPos = stPos + stringVal.length();
                    TextRange txtRange = inputFile.newRange(lineNb, stPos, lineNb, endPos);
                    highlighting.highlight(txtRange, stringType);
                }
            }
        }
    }

    /**
     * Handle Ada keywords highlighting found on the current line of the given input file
     */
    private void keywordsHighlighting(InputFile inputFile, String line, int lineNb, NewHighlighting highlighting) {
        TypeOfText stringType = TypeOfText.KEYWORD;
        int stPos;
        int endPos;
        // split current line using " " as separator in order to have each element and compare with keyword
        // to get full match for a keyword and avoid to highlight strings matching keywords inside others words
        String[] parts = line.split(" ");
        for (String word : KEYWORDS) {
            for (String part : parts) {
                if (part.contains(word)) {
                    if (part.contains(";")) {
                        if (part.trim().replaceFirst(";", "").equals(word)) {
                            stPos = subStringIndex(line, word);
                            endPos = stPos + word.length();
                            TextRange txtRange = inputFile.newRange(lineNb, stPos, lineNb, endPos);
                            highlighting.highlight(txtRange, stringType);
                        }
                    } else {
                        if (part.trim().equals(word)) {
                            stPos = subStringIndex(line, word);
                            endPos = stPos + word.length();
                            TextRange txtRange = inputFile.newRange(lineNb, stPos, lineNb, endPos);
                            highlighting.highlight(txtRange, stringType);
                        }
                    }
                }
            }
        }
    }

    /**
     * Ada keywords presence testing using regexp for the given string
     *
     * @param line current line to analyze
     * @return true if Ada keyword found
     */
    private static boolean containsKeywords (String line){
        Pattern p = Pattern.compile("\\b(" +
                        "abort|abs|abstract|accept|access|aliased|all|and|array|at|" +
                        "begin|body|" +
                        "case|constant|" +
                        "declare|delay|delta|digits|do|" +
                        "else|elsif|end|entry|exception|exit|" +
                        "for|function|" +
                        "generic|goto|" +
                        "if|in|interface|is|" +
                        "limited|loop|" +
                        "mod|" +
                        "new|not|null|" +
                        "others|out|of|or|overriding|" +
                        "package|pragma|private|procedure|protected|" +
                        "raise|range|record|rem|renames|requeue|return|reverse|" +
                        "select|separate|some|subtype|synchronized|" +
                        "tagged|task|terminate|then|type|" +
                        "until|use|" +
                        "when|while|with|" +
                        "xor"
                        +")\\b");

        Matcher m = p.matcher(line);
        return m.find();
        /*while (m.find()) {
            return true;
        }
        return false;*/
    }

    /**
     * Handle Ada comments highlighting for the current line of the given input file
     */
    private void commentHighlighting(InputFile inputFile, String line, int lineNb, NewHighlighting highlighting) {
        TypeOfText stringType = TypeOfText.COMMENT;

        if (line.startsWith("--")) {
            // full line comment
            highlighting.highlight(inputFile.selectLine(lineNb), stringType);
        } else {
            // line contains "--" sequence : it may be an indented comment or an EOL comment in the current line
            if (line.trim().startsWith("--")) {
                // indented comment
                highlighting.highlight(inputFile.selectLine(lineNb), stringType);
            } else {
                // EOL comment
                int stPos = subStringIndex(line, "--");
                int endPos = inputFile.selectLine(lineNb).end().lineOffset();
                TextRange txtRange = inputFile.newRange(lineNb, stPos, lineNb, endPos);
                highlighting.highlight(txtRange, stringType);
            }
        }
    }

    /**
     *  Returns the position of the first char of the given substring in the string
     *
     * @param str initial string value
     * @param substr string value representing a substring of the str
     * @return index position in the string of the first character of the given substring
     */
    private static int subStringIndex(String str, String substr) {
        return str.indexOf(substr);
    }

    @Override
    public void describe(SensorDescriptor sensorDescriptor) {
        sensorDescriptor
                .name("Ada Code Highlighting Sensor")
                .onlyOnLanguages(Ada.KEY);
    }
    @Override
    public void execute(SensorContext sensorContext) {
        for (InputFile file :
                sensorContext.fileSystem().inputFiles(sensorContext.fileSystem().predicates().hasLanguages(Ada.KEY)))
        {
            processFileHighlighting(file, sensorContext);
        }
    }
}
