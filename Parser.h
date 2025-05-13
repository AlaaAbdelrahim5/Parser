#ifndef PARSER_H
#define PARSER_H

#include "Scanner.h"
#include "stable.h"
#include "ast.h"
#include <stdarg.h>

class Parser
{
public:
    Parser(FileDescriptor *fd);
    ast_list *parse();

private:
    Scanner *scanner;     // The scanner object
    STable *table;        // The symbol table
    ast_list *programAST; // The program AST
    int offset;           // The offset for the symbol table entries

    // <program>
    // <decl_list>
    ast_list *decl_list(TOKEN *currentToken); // <decl> ';' <decl_list> | λ

    // <decl>
    // <var_decl>
    AST *var_decl(TOKEN *token);       // var id ':' <type>
    AST *constant_decl(TOKEN *token);  // constant id '=' <expr>
    AST *function_decl(TOKEN *token);  // function id <formal_list> ':' <type> <block>
    AST *procedure_decl(TOKEN *token); // procedure id <formal_list> <block>

    // <type>
    bool matchType(TOKEN *token);                  // integer | string | float | bool
    STE_TYPE getTokenType(LEXEME_TYPE token_type); // Get the type of the token

    // <formal_list>
    ste_list *formal_list(TOKEN *token); // '('')' | '('<formals>')'
    ste_list *formal_list_tail(TOKEN *token);

    // <formals>
    ste_list *formals(TOKEN *token, ste_list *STE_list_x); // id ':' <type> | <formals> ',' id ':' <type>
    ste_list *formals_tail(TOKEN *token, ste_list *STE_list_x);

    // <stmt>
    AST *stmt(TOKEN *token);
    AST *callAssign_stmt(TOKEN *token); // id ':=' <expr> | id <arg-list>
    AST *assign_stmt(TOKEN *token);     // id ':=' <expr>
    AST *call_stmt(TOKEN *token);       // id <arg-list>
    AST *if_stmt(TOKEN *token);         // if <expr> then <stmt> fi | if <expr> then <stmt> else <stmt> fi
    AST *if_tail(TOKEN *token, AST *ast_Exp, AST *ast_conseq);
    AST *while_stmt(TOKEN *token);  // while <expr> do <stmt> od
    AST *for_stmt(TOKEN *token);    // for id ':=' <expr> to <expr> do <stmt> od
    AST *read_stmt(TOKEN *token);   // read '('id')'
    AST *write_stmt(TOKEN *token);  // write '('id')'
    AST *return_stmt(TOKEN *token); // return '('<expr>')'
    // <block>
    AST *block_stmt(TOKEN *token); // begin <var_decl_list> <stmt_list> end

    // <var_decl_list>
    ste_list *var_decl_list(TOKEN *token, ste_list *STE_list_x); // <var_decl> ';' <var_decl_list> | λ
    bool checkFollow_var_decl_list(TOKEN *token);                // read | write | begin | if | for | while | return | end | id

    // <stmt_list>
    ast_list *stmt_list(TOKEN *token, ast_list *AST_list_x); // <stmt> ';' <stmt_list> | λ

    // <arg_list>
    ast_list *arg_list(TOKEN *token); // '('')' | '('<args>')'
    ast_list *arg_list_tail(TOKEN *token);

    // <args>
    ast_list *args(TOKEN *token, ast_list *AST_list_x); // <expr> | <expr> ',' <args>
    ast_list *args_tail(TOKEN *token, ast_list *AST_list_x);

    // <expr>
    AST *parseE(TOKEN *token);
    AST *parseEb(AST *E, TOKEN *token);
    AST *parseF(TOKEN *token);
    AST *parseFb(AST *F, TOKEN *token);
    AST *parseG(TOKEN *token);
    AST *parseGb(AST *G, TOKEN *token);
    AST *parseH(TOKEN *token);
    AST *parseHb(AST *H, TOKEN *token);
    AST *parseI(TOKEN *token);
    AST *parseJ(TOKEN *token);
    bool checkFollowExpr(TOKEN *token);

    // <arith_op>
    bool md_op(TOKEN *token); // '*' | '/'
    bool pm_op(TOKEN *token); // '+' | '-'

    // <rel_op>
    bool rel_op(TOKEN *token); // '=' | '!=' | '<' | '<=' | '>' | '>='

    // <rel_conj>
    bool rel_conj(TOKEN *token); // and | or

    // <unary_op>
    bool unary_op(TOKEN *token); // - | not

    symbol_table_entry *setSTEValues(STE_ENTRY_TYPE type, ...);                     // Set the values of the symbol table entry
    ste_list *addNodeToSteList(symbol_table_entry *ST_Entry, ste_list *STE_list_x); // Add a node to the symbol table entry list
    ast_list *addNodeToAstList(AST *ast, ast_list *AST_list_x);                     // Add a node to the AST list

    bool matchLexeme(TOKEN *token, LEXEME_TYPE type); // match the token with the lexeme type

    void reportError(string message); // Report an error message
    void print_AST(AST *ast, FILE *); // Print the AST
};

#endif // PARSER_H
