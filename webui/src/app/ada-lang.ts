import * as hljs from "highlight.js";

/**
 * This file handles the highlighting of Ada source code.
 *
 * It is currently based on the highlight.js (https://highlightjs.org) engine.
 * Other candidates were:
 *   * SyntaxHighlighter (http://alexgorbatchev.com/SyntaxHighlighter/)
 *   * SHJS (http://shjs.sourceforge.net/)
 *   * Google Code Prettify (http://code.google.com/p/google-code-prettify/)
 *
 * We decided to go (for now) with highlight.js for the following reasons:
 *   * It supports arbitrary HTML container for code (very flexible)
 *   * It can detect languages and suppors multi-language code
 *   * It supports (at the time of this writing, ie. v9.1.0) 146 languages and
 *     66 styles
 *   * It's extensible, allowing us to provide a definition for the Ada language
 *
 * @see http://softwaremaniacs.org/blog/2011/05/22/highlighters-comparison/
 */

const KEYWORDS: string[] = [
    "abort", "else", "new", "return", "abs", "elsif", "not", "reverse",
    "abstract", "end", "null", "accept", "entry", "select", "access",
    "exception", "of", "separate", "aliased", "exit", "or", "some", "all",
    "others", "subtype", "and", "for", "out", "synchronized", "array",
    "function", "overriding", "at", "tagged", "generic", "package", "task",
    "begin", "goto", "pragma", "terminate", "body", "private", "then", "if",
    "procedure", "type", "case", "in", "protected", "constant", "interface",
    "until", "is", "raise", "use", "declare", "range", "delay", "limited",
    "record", "when", "delta", "loop", "rem", "while", "digits", "renames",
    "with", "do", "mod", "requeue", "xor"
];

/**
 * Definition of the Ada language for the highlight.js engine.
 *
 * We should consider contributing this part to the highlight.js project at some
 * point.
 *
 * @return A JavaScript object describing the default parsing mode for the
 *      language.
 * @see https://highlightjs.readthedocs.org/en/stable/reference.html
 * @see https://github.com/isagalaev/highlight.js/blob/master/src/languages/
 */
function hljs_ada_language(): hljs.IMode {
    "use strict";

    const COMMENT_MODES: hljs.IMode[] = [
        hljs.COMMENT("--", "$", { relevance: 10 })
    ];
    return {
        case_insensitive: true,
        keywords: KEYWORDS.join(" "),
        contains: [
            hljs.QUOTE_STRING_MODE, hljs.NUMBER_MODE
        ].concat(COMMENT_MODES)
    };
}

// Register the Ada language to the highlight.js engine.
hljs.registerLanguage("ada", hljs_ada_language);

/**
 * Convert an Ada snippet into an HTML string with highlighting markup.
 *
 * @param code The Ada code snippet to highlight.
 * @return The HTML string with highlighting markup.
 */
export function highlightAda(code: string): string {
    "use strict";
    return hljs.highlight("ada", code).value;
}
