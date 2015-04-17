module fibonacci;

import std.conv : to;
import std.stdio : writefln, writeln, readln;

import llvm.c;

import llvm.util.memory;

import llvmtest;
import lexertest;
import ddc.lexer.linestream;
import ddc.lexer.tokenizer;

int main(string[] args)
{
    
    testTokenizer(q{//some comment
        /******
         some text for multiline comment
        **********/
        /+  /* bla bla */  /+ some nested comment +/ hjk
        ++++++++++++++++++/
        for someIdent
    }, [
        TokId.comment_single, TokId.whitespace, 
        TokId.comment_multi, TokId.whitespace, 
        TokId.comment_nested, TokId.whitespace, 
        TokId.kw_for, TokId.whitespace, 
        TokId.ident, TokId.whitespace, 
        TokId.eof]);

    version(Windows) {
        TextLines[] res;
        res ~= parseAllFiles(`C:\D\dmd2\src\phobos`);
        res ~= parseAllFiles(`D:\projects\d\dlangui\src`);
        res ~= parseAllFiles(`D:\projects\d\dlangide\src`);
        res ~= parseAllFiles(`D:\projects\d\libdparse\src`);
        writeln("files parsed: ", res.length);
    }

    if (!LLVM.loaded) {
		writeln("Cannot load LLVM dll");
        readln();
        return 1;
    }

    return fibo(args);

}
