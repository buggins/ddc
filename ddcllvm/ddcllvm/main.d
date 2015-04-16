module fibonacci;

import std.conv : to;
import std.stdio : writefln, writeln, readln;

import llvm.c;

import llvm.util.memory;

import llvmtest;

int main(string[] args)
{

    if (!LLVM.loaded) {
		writeln("Cannot load LLVM dll");
        readln();
        return 1;
    }

    return fibo(args);

}
