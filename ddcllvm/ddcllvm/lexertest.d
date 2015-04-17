module lexertest;

import ddc.lexer.tokenizer;
import ddc.lexer.textsource;
import ddc.lexer.linestream;

import std.file;
import std.string;
import std.stdio;

void testTokenizer(string source, TokId[] expectedTokens ...) {
    TextLines s = new TextLines();
    SourceError res = s.loadFromBytes(cast(ubyte[])source.dup);
    StringCache cache;
    Utf8Tokenizer tokenizer;
    tokenizer.init(&cache, s);
    writeln("parsing source: ------------------------------\n", source, "\n----------------------------\n");
    foreach(tokType; expectedTokens) {
        Tok t = tokenizer.nextToken;
        writeln("    ", t);
        if (t.idForTest != tokType) {
            writeln("error! expected token: ", tokType, " found: ", t);
            break;
        }
        if (t.type == TokType.eof)
            break;
    }
    writeln("done");
}

TextLines parseFile(string fn) {
    TextLines s = new TextLines();
    SourceError res = s.loadFromFile(fn);
    writeln("loaded file ", fn, " error=", res, " bytes=", s.text.length, " lineCount=", s.lineCount, " format=", s.format.toString);
    return s;
}

TextLines[] parseAllFiles(string dir) {
    TextLines[] res;
    foreach(DirEntry e; dirEntries(dir, SpanMode.depth, true)) {
        if (e.isFile && e.name.endsWith(".d")) {
            res ~= parseFile(e.name);
        }
    }
    return res;
}



