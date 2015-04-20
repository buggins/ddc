module lexertest;

public import ddc.lexer.tokenizer;
public import ddc.lexer.textsource;
public import ddc.lexer.linestream;

import std.file;
import std.string;
import std.stdio;
public import std.d.lexer;
public import std.lexer;

void testTokenizer(string source, TokId[] expectedTokens ...) {
    TextLines s = new TextLines();
    SourceError res = s.loadFromBytes(cast(ubyte[])source.dup);
    StrCache cache;
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

TextLines parseFile(string fn, StrCache * identCache) {
    TextLines s = new TextLines();
    SourceError res = s.loadFromFile(fn);
    if (res != SourceError.None) {
        writeln("loaded file ", fn, " error=", res, " bytes=", s.text.length, " lineCount=", s.lineCount, " format=", s.format.toString);
    } else {
        Tok[] tokens;
        bool fileInfoShown = false;
        if (identCache) {
            Utf8Tokenizer tokenizer;
            tokenizer.init(identCache, s);
            for(;;) {
                Tok t = tokenizer.nextToken();
                tokens ~= t;
                if (t.id == TokId.eof)
                    break;
                if (t.type == TokType.error) {
                    if (!fileInfoShown) {
                        writeln("loaded file ", fn, " error=", res, " bytes=", s.text.length, " lineCount=", s.lineCount, " format=", s.format.toString);
                        fileInfoShown = true;
                    }
                    writeln("error ", t);
                }
            }
        }
    }
    return s;
}

const(Token)[] parseFileWithDparse(string fn, StringCache * stringCache) {
    const(Token)[] res;
    TextLines s = new TextLines();
    SourceError err = s.loadFromFile(fn);
    if (err != SourceError.None) {
        writeln("loaded file ", fn, " error=", err, " bytes=", s.text.length, " lineCount=", s.lineCount, " format=", s.format.toString);
    } else {
        res = getTokensForParser(cast(ubyte[])s.text, LexerConfig(), stringCache);
    }
    return res;
}

TextLines[] parseAllFiles(string dir, StrCache * identCache) {
    TextLines[] res;
    foreach(DirEntry e; dirEntries(dir, SpanMode.depth, true)) {
        if (e.isFile && e.name.endsWith(".d")) {
            res ~= parseFile(e.name, identCache);
        }
    }
    return res;
}

const(Token)[][] parseAllFilesWithDparse(string dir, StringCache * identCache) {
    const(Token)[][] res;
    foreach(DirEntry e; dirEntries(dir, SpanMode.depth, true)) {
        if (e.isFile && e.name.endsWith(".d")) {
            res ~= parseFileWithDparse(e.name, identCache);
        }
    }
    return res;
}

long benchmarkNewTokenizer() {
    long duration = 0;
    version(Windows) {
        long startTs = currentTimeMillis;
        TextLines[] res;
        StrCache cache;
        res ~= parseAllFiles(`C:\D\dmd2\src\phobos`, &cache);
        res ~= parseAllFiles(`D:\projects\d\dlangui\src`, &cache);
        res ~= parseAllFiles(`D:\projects\d\dlangide\src`, &cache);
        res ~= parseAllFiles(`D:\projects\d\libdparse\src`, &cache);
        long endTs = currentTimeMillis;
        duration = endTs - startTs;
        writeln("new tokenizer, files parsed: ", res.length, " timeElapsedMs=", endTs - startTs);
    }
    return duration;
}
long benchmarkDParseTokenizer() {
    long duration = 0;
    version(Windows) {
        long startTs = currentTimeMillis;
        const(Token)[][] res;
        StringCache* cache = new StringCache(StringCache.defaultBucketCount);
        res ~= parseAllFilesWithDparse(`C:\D\dmd2\src\phobos`, cache);
        res ~= parseAllFilesWithDparse(`D:\projects\d\dlangui\src`, cache);
        res ~= parseAllFilesWithDparse(`D:\projects\d\dlangide\src`, cache);
        res ~= parseAllFilesWithDparse(`D:\projects\d\libdparse\src`, cache);
        long endTs = currentTimeMillis;
        duration = endTs - startTs;
        writeln("DParse tokenizer, files parsed: ", res.length, " timeElapsedMs=", endTs - startTs);
    }
    return duration;
}
