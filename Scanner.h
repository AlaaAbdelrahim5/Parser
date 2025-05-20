#ifndef SCANNER_H
#define SCANNER_H

#include <string>
#include "FileDescriptor.h"
#define LETTER_CHAR 1
#define NUMERIC_DIGIT 2
#define SEPARATOR 3
#define OPERATOR 4
#define SPECIAL_CHAR 5
#define COMMENT_MARKER 6

// Token types enumeration
typedef enum
{
    /* Literals */
    lx_identifier,
    lx_integer,
    lx_string,
    lx_float,
    /* Keywords */
    kw_program,
    kw_var,
    kw_constant,
    kw_integer,
    kw_bool,
    kw_string,
    kw_float,
    kw_true,
    kw_false,
    kw_if,
    kw_fi,
    kw_then,
    kw_else,
    kw_while,
    kw_do,
    kw_od,
    kw_and,
    kw_or,
    kw_read,
    kw_write,
    kw_for,
    kw_from,
    kw_to,
    kw_by,
    kw_function,
    kw_procedure,
    kw_return,
    kw_not,
    kw_begin,
    kw_end,
    /* Operators */
    lx_lparen,
    lx_rparen,
    lx_lbracket,
    lx_rbracket,
    lx_lsbracket,
    lx_rsbracket,
    Ix_colon,
    lx_dot,
    lx_semicolon,
    lx_comma,
    Ix_colon_eq,
    lx_plus,
    lx_minus,
    lx_star,
    lx_slash,
    lx_eq,
    lx_neq,
    lx_lt,
    lx_le,
    lx_gt,
    lx_ge,
    lx_eof,
    illegal_token

} LEXEME_TYPE;

class TOKEN
{
public:
    LEXEME_TYPE type;
    int value; // can be used for identifiers and strings instead of the str_ptr
    float float_value;
    char *str_ptr;

    TOKEN()
    {
        str_ptr = new char[1024];
        str_ptr[0] = '\0';
    }
    ~TOKEN()
    {
        delete[] str_ptr;
    }
};

class Scanner
{
public:
    int privousType;
    bool readMore;
    TOKEN *lastToken;
    FileDescriptor *fd;

    Scanner(FileDescriptor *fd);
    ~Scanner();
    TOKEN *Scan();
    TOKEN *getId(char c);
    TOKEN *getInt(char c);
    TOKEN *getString(char c);
    int checkKeyword(char *key);
    void skipComments(char &c);
    void skipSpaces(char &c);
    TOKEN *getLastToken();
    int getLineNum();
    int getClass(char c);
    TOKEN *getOperator(char c);
    FileDescriptor *Get_fd();
};

#endif // SCANNER_H
