module main;

import std.stdio;

unittest {
    writeln("Hello from unittest");
}

void main(string[] args)
{
    // Prints "Hello World" string in console
    writeln("Hello World!");
    
    // Lets the user press <Return> before program returns
    stdin.readln();
}

