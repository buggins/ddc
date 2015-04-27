module ddc.lexer.ast;

import ddc.lexer.tokenizer;

class ASTVisitor {
    void visit(const ASTNode node) {
        assert(false);
    }
    void visit(const (Tok) * token) {
    }
    void visit(const ExpressionNode node) {
        node.accept(this);
    }
    void visit(const Module node) {
        node.accept(this);
    }
}

class ASTNode {
    void accept(ASTVisitor visitor) const {
        assert(false);
    }
}

class Module : ASTNode {
    const(Tok) * scriptLine;
    override void accept(ASTVisitor visitor) const {
        visitor.visit(this);
    }
}

class ExpressionNode : ASTNode {
}

template visitIfNotNull(fields ...)
{
    static if (fields.length > 1)
        immutable visitIfNotNull = visitIfNotNull!(fields[0]) ~ visitIfNotNull!(fields[1..$]);
    else
    {
        static if (typeof(fields[0]).stringof[$ - 2 .. $] == "[]")
        {
            static if (__traits(hasMember, typeof(fields[0][0]), "classinfo"))
                immutable visitIfNotNull = "foreach (i; " ~ fields[0].stringof ~ ") if (i !is null) visitor.visit(i);\n";
            else
                immutable visitIfNotNull = "foreach (i; " ~ fields[0].stringof ~ ") visitor.visit(i);\n";
        }
        else static if (__traits(hasMember, typeof(fields[0]), "classinfo"))
            immutable visitIfNotNull = "if (" ~ fields[0].stringof ~ " !is null) visitor.visit(" ~ fields[0].stringof ~ ");\n";
        else
            immutable visitIfNotNull = "visitor.visit(" ~ fields[0].stringof ~ ");\n";
    }
}

mixin template BinaryExpressionBody()
{
    ExpressionNode left;
    ExpressionNode right;
    Tok * op;
}

final class Expression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(items));
    }
    /** */ ExpressionNode[] items;
    //mixin OpEquals;
}

final class AddExpression : ExpressionNode {
    override void accept(ASTVisitor visitor) const {
        mixin(visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
}

class FunctionBody {
}
