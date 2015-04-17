module lexertest;

import ddc.lexer.tokenizer;
import ddc.lexer.textsource;
import ddc.lexer.linestream;

import std.file;
import std.string;
import std.stdio;


TextLines parseFile(string fn) {
    writeln("trying to open file ", fn);
    TextLines s = new TextLines();
    SourceError res = s.loadFromFile(fn);
    writeln("loaded file ", fn, " result=", res, " lineCount=", s.lineCount, " format=", s.format.toString);
    return s;
}

void parseAllFiles(string dir) {
    foreach(DirEntry e; dirEntries(dir, SpanMode.depth, true)) {
        if (e.isFile && e.name.endsWith(".d")) {
            parseFile(e.name);
        }
    }
}



