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
        for (someIdent123=) >>>= 
            "Some\x35\u0036\U00000037\&quot;. \n text" `Bla \n bla`d r"bla bla"w "some\t\ttext тест"c
            'a'
            'b'c
            'c'w
            'd'd
            125
            125u
            125L
            125UL
            0x1234567,0b01010111
            0x1234567U,0b01010111u
            0x1234567L,0b01010111L
            0x1234567uL,0b01010111UL
    }, [
        TokId.comment_single, TokId.whitespace, 
        TokId.comment_multi, TokId.whitespace, 
        TokId.comment_nested, TokId.whitespace, 
        TokId.kw_for, TokId.whitespace, TokId.op_par_open, TokId.ident, TokId.op_eq,
        TokId.op_par_close, TokId.whitespace, TokId.op_asr_eq, TokId.whitespace,
        TokId.str_unknown, TokId.whitespace, 
        TokId.str_32, TokId.whitespace, 
        TokId.str_16, TokId.whitespace, 
        TokId.str_8, TokId.whitespace, 
        TokId.char_unknown, TokId.whitespace, 
        TokId.char_8, TokId.whitespace, 
        TokId.char_16, TokId.whitespace, 
        TokId.char_32, TokId.whitespace, 
        TokId.int_default, TokId.whitespace, 
        TokId.int_unsigned, TokId.whitespace, 
        TokId.int_long, TokId.whitespace, 
        TokId.int_unsigned_long, TokId.whitespace, 
        TokId.int_default, TokId.op_comma, TokId.int_default, TokId.whitespace, 
        TokId.int_unsigned, TokId.op_comma, TokId.int_unsigned, TokId.whitespace, 
        TokId.int_long, TokId.op_comma, TokId.int_long, TokId.whitespace, 
        TokId.int_unsigned_long, TokId.op_comma, TokId.int_unsigned_long, TokId.whitespace, 
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
