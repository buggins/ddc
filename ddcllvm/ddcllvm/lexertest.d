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

TextLines loadFile(string fn) {
    TextLines s = new TextLines();
    SourceError res = s.loadFromFile(fn);
    if (res != SourceError.None) {
        writeln("loaded file ", fn, " error=", res, " bytes=", s.text.length, " lineCount=", s.lineCount, " format=", s.format.toString);
    }
    return s;
}

Tok[] parseAllFiles(TextLines[] files, StrCache * identCache) {
    Tok[] tokens;
    foreach(s; files) {
        bool fileInfoShown = false;
        if (identCache) {
            Utf8Tokenizer tokenizer;
            tokenizer.init(identCache, s);
            tokens ~= tokenizer.getTokensForParser();
        }
    }
    return tokens;
}

TextLines[] loadAllFiles(string dir) {
    TextLines[] res;
    foreach(DirEntry e; dirEntries(dir, SpanMode.depth, true)) {
        if (e.isFile && e.name.endsWith(".d")) {
            res ~= loadFile(e.name);
        }
    }
    return res;
}

const(Token)[] parseAllFilesWithDparse(TextLines[] files, StringCache * identCache) {
    const(Token)[] res;
    foreach(s; files) {
        res ~= getTokensForParser(cast(ubyte[])s.text, LexerConfig(), identCache);
    }
    return res;
}

long benchmarkNewTokenizer(TextLines[] files, StrCache * cache) {
    long duration = 0;
    version(Windows) {
        long startTs = currentTimeMillis;
        Tok[] res = parseAllFiles(files, cache);
        long endTs = currentTimeMillis;
        duration = endTs - startTs;
        writeln("new tokenizer, files parsed: ", files.length, " timeElapsedMs=", endTs - startTs, " token count:", res.length, " token bytes:", res[0].sizeof * res.length);
    }
    return duration;
}

long benchmarkDParseTokenizer(TextLines[] files, StringCache* cache) {
    long duration = 0;
    version(Windows) {
        long startTs = currentTimeMillis;
        const(Token)[] res = parseAllFilesWithDparse(files, cache);
        long endTs = currentTimeMillis;
        duration = endTs - startTs;
        writeln("DParse tokenizer, files parsed: ", files.length, " timeElapsedMs=", endTs - startTs, " token count:", res.length, " token bytes:", res[0].sizeof * res.length);
    }
    return duration;
}

void runBenchmarks() {
    TextLines[] res;
    version(Windows) {
        long startTs = currentTimeMillis;
        res ~= loadAllFiles(`C:\D\dmd2\src\phobos`);
        res ~= loadAllFiles(`D:\projects\d\dlangui\src`);
        res ~= loadAllFiles(`D:\projects\d\dlangide\src`);
        res ~= loadAllFiles(`D:\projects\d\libdparse\src`);
        long endTs = currentTimeMillis;
        writeln("files loaded: ", res.length, " timeElapsedMs=", endTs - startTs);
    }

    StringCache* cache = new StringCache(StringCache.defaultBucketCount);
    StrCache newcache;

    long totalNew = 0;
    long totalDparse = 0;
    totalNew += benchmarkNewTokenizer(res, &newcache);
    totalDparse += benchmarkDParseTokenizer(res, cache);
    totalDparse += benchmarkDParseTokenizer(res, cache);
    totalNew += benchmarkNewTokenizer(res, &newcache);
    totalDparse += benchmarkDParseTokenizer(res, cache);
    totalNew += benchmarkNewTokenizer(res, &newcache);
    totalDparse += benchmarkDParseTokenizer(res, cache);
    totalNew += benchmarkNewTokenizer(res, &newcache);
    totalDparse += benchmarkDParseTokenizer(res, cache);
    totalNew += benchmarkNewTokenizer(res, &newcache);
    totalDparse += benchmarkDParseTokenizer(res, cache);
    totalNew += benchmarkNewTokenizer(res, &newcache);
    totalDparse += benchmarkDParseTokenizer(res, cache);
    totalNew += benchmarkNewTokenizer(res, &newcache);
    writeln("total time for newTokenizer=", totalNew, " dparse=", totalDparse);
}
