module fibonacci;

import std.conv : to;
import std.stdio : writefln, writeln, readln;

import llvm.c;

import llvm.util.memory;

import llvmtest;
import lexertest;

int main(string[] args)
{

    version(Windows) {
        parseAllFiles("C:\\D\\dmd2\\src\\phobos");
    }

    if (!LLVM.loaded) {
		writeln("Cannot load LLVM dll");
        readln();
        return 1;
    }

    return fibo(args);

}
