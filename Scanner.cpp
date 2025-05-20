#include "Scanner.h"

char *keywords[] =
    {
        "and", "begin", "boolean", "by", "constant", "do", "else", "end",
        "false", "fi", "float", "for", "from", "function", "if", "integer",
        "not", "od", "or", "procedure", "programAST", "read", "return", "string",
        "then", "to", "true", "var", "while", "write"};

LEXEME_TYPE lexTypes[] =
    {
        kw_and, kw_begin, kw_bool, kw_by, kw_constant,
        kw_do, kw_else, kw_end, kw_false, kw_fi, kw_float,
        kw_for, kw_from, kw_function, kw_if, kw_integer, kw_not,
        kw_od, kw_or, kw_procedure, kw_program, kw_read, kw_return,
        kw_string, kw_then, kw_to, kw_true, kw_var, kw_while, kw_write};

Scanner::Scanner(FileDescriptor *fd)
{
    this->fd = fd;
}

Scanner::~Scanner()
{
    delete lastToken;
    delete fd;
    lastToken = nullptr;
    fd = nullptr;
}

TOKEN *Scanner::Scan()
{
    char currentChar = fd->GetChar();

    while (isspace(currentChar) || getClass(currentChar) == COMMENT_MARKER)
    {
        if (isspace(currentChar))
            skipSpaces(currentChar);

        if (getClass(currentChar) == COMMENT_MARKER)
        {
            currentChar = fd->GetChar();

            if (getClass(currentChar) == COMMENT_MARKER)
            {
                skipComments(currentChar);
            }
            else
            {
                fd->ReportError("Incomplete or wrong comment entered.");
                return NULL;
            }
        }
    }

    if (currentChar == EOF)
    {
        readMore = false;
        TOKEN *token = new TOKEN();
        token->type = lx_eof;
        lastToken = token;
        return token;
    }

    if (currentChar == '-')
    {
        TOKEN *minusToken = new TOKEN();
        minusToken->type = lx_minus;
        privousType = OPERATOR;
        lastToken = minusToken;
        return minusToken;
    }
    if (currentChar == ';')
    {
        TOKEN *semicolonToken = new TOKEN();
        semicolonToken->type = lx_semicolon;
        lastToken = semicolonToken;
        return semicolonToken;
    }
    if (getClass(currentChar) == OPERATOR)
    {
        lastToken = getOperator(currentChar);
        return lastToken;
    }
    if (currentChar == '"')
    {
        lastToken = getString(currentChar);
        return lastToken;
    }

    int charType = getClass(currentChar);

    if (charType == LETTER_CHAR || currentChar == '_')
    {
        lastToken = getId(currentChar);
        return lastToken;
    }
    else if (charType == NUMERIC_DIGIT)
    {
        lastToken = getInt(currentChar);
        return lastToken;
    }

    fd->ReportError("Unknown Token");
    lastToken = NULL;
    return NULL;
}

TOKEN *Scanner::getId(char c)
{
    string idStr;
    TOKEN *token = NULL;
    int currentClass = getClass(c); 

    while (currentClass == NUMERIC_DIGIT || currentClass == LETTER_CHAR ||
           currentClass == SPECIAL_CHAR)
    {
        idStr += c;
        c = fd->GetChar();
        currentClass = getClass(c);
    }

    if (currentClass != SEPARATOR && currentClass != OPERATOR && c != EOF)
    {
        fd->ReportError("Invalid identifier");
        return NULL;
    }

    if (currentClass == OPERATOR || c == ';' || c == EOF)
        fd->UngetChar(c);

    // Using const_cast to remove the const qualifier and check if the identifier is a keyword
    int keywordIndex = checkKeyword(const_cast<char *>(idStr.c_str()));
    if (keywordIndex != -1)
    {
        token = new TOKEN();
        token->type = lexTypes[keywordIndex]; // Set token type as keyword type
        privousType = -2;
        return token;
    }
    else
    {
        token = new TOKEN();
        token->type = lx_identifier;
        token->str_ptr = new char[idStr.size() + 1];
        strcpy(token->str_ptr, idStr.data());
        privousType = -2;
        return token;
    }
}

TOKEN *Scanner::getInt(char c)
{
    TOKEN *token = NULL;
    int currentClass = getClass(c);
    string intStr;

    while (currentClass == NUMERIC_DIGIT)
    {
        intStr += c;
        c = fd->GetChar();
        currentClass = getClass(c);
    }

    if (currentClass == SEPARATOR || currentClass == OPERATOR || c == '\n' || currentClass == COMMENT_MARKER || c == EOF)
    {

        if (currentClass == OPERATOR || c == ';' || currentClass == COMMENT_MARKER || c == EOF)
            fd->UngetChar(c);

        token = new TOKEN();
        token->type = lx_integer;
        token->value = stoi(intStr); // Convert the string to an integer
        privousType = -2;
        return token;
    }
    else if (currentClass == lx_dot)
    {

        intStr += c;
        c = fd->GetChar();
        currentClass = getClass(c);

        while (currentClass == NUMERIC_DIGIT)
        {
            intStr += c;
            c = fd->GetChar();
            currentClass = getClass(c);
        }

        if (currentClass == SEPARATOR || currentClass == OPERATOR || currentClass == COMMENT_MARKER || c == EOF)
        {

            if (currentClass == OPERATOR || currentClass == COMMENT_MARKER || c == ';' || c == EOF)
                fd->UngetChar(c);

            token = new TOKEN();
            token->type = lx_float;
            token->float_value = stof(intStr);
            privousType = -2;
        }
        else
        {
            fd->ReportError("Invalid floating-point number");
            return NULL;
        }
    }
    else
    {
        fd->ReportError("Invalid integer number");
        return NULL;
    }
    return token;
}

TOKEN *Scanner::getString(char startChar)
{
    string stringStr;
    char currentChar = fd->GetChar();

    while (currentChar != '\n' && currentChar != EOF)
    {
        if (currentChar == '\\')
        {
            stringStr += currentChar;
            currentChar = fd->GetChar();
            stringStr += currentChar;
        }
        else if (currentChar == '"')
        {
            break;
        }
        else
        {
            stringStr += currentChar;
        }
        currentChar = fd->GetChar();
    }

    if (currentChar == '\n' || currentChar == EOF)
    {
        fd->ReportError("Unterminated string literal");
        return nullptr;
    }

    TOKEN *token = new TOKEN();
    token->type = lx_string;

    // Allocate memory for the token's string value and copy the token value into it
    token->str_ptr = new char[stringStr.size() + 1];
    strcpy(token->str_ptr, stringStr.data());
    privousType = -2;
    return token;
}

int Scanner::checkKeyword(char *word)
{
    for (unsigned int i = 0; i < sizeof(lexTypes) / sizeof(LEXEME_TYPE); i++)
    {
        if (strcmp(word, keywords[i]) == 0)
        {
            return i;
        }
    }

    return -1;
}

void Scanner::skipComments(char &currentChar)
{
    while (true)
    {
        currentChar = fd->GetChar();
        while (currentChar != '#' && currentChar != '\n' && currentChar != EOF)
            currentChar = fd->GetChar();

        if (currentChar == '\n')
            return;

        if (currentChar == EOF)
        {
            return;
        }

        if (currentChar == '#')
        {
            currentChar = fd->GetChar();
            if (currentChar == '#')
            {
                currentChar = fd->GetChar();
                return;
            }
        }
    }
}

void Scanner::skipSpaces(char &currentChar)
{
    while (isspace(currentChar = fd->GetChar()))
        ;
}

TOKEN *Scanner::getLastToken()
{
    return lastToken;
}

int Scanner::getLineNum()
{
    return fd->GetLineNum();
}

int Scanner::getClass(char c)
{
    if (isalpha(c))
    {
        return LETTER_CHAR;
    }
    else if (c >= '0' && c <= '9')
    {
        return NUMERIC_DIGIT;
    }
    else if (c == ';' || c == ' ' || c == '\n' || isspace(c) || c == EOF)
    {
        return SEPARATOR;
    }
    else if (c == '(' || c == ')' || c == '+' || c == '*' ||
             c == '/' || c == '=' || c == '[' || c == ']' ||
             c == '{' || c == '}' || c == ',' || c == ':' ||
             c == '=' || c == '>' || c == '<' || c == '-' ||
             c == '!')
    {
        return OPERATOR;
    }
    else if (c == '.')
    {
        return lx_dot;
    }
    else if (c == '_')
    {
        return SPECIAL_CHAR;
    }
    else if (c == '#')
    {
        return COMMENT_MARKER;
    }

    return 0;
}

TOKEN *Scanner::getOperator(char currentChar)
{
    TOKEN *token = new TOKEN();

    if (currentChar == '+')
    {
        token->type = lx_plus;
    }
    else if (currentChar == '-')
    {
        token->type = lx_minus;
    }
    else if (currentChar == '*')
    {
        token->type = lx_star;
    }
    else if (currentChar == '/')
    {
        token->type = lx_slash;
    }

    if (currentChar == ':')
    {
        currentChar = fd->GetChar();
        if (currentChar == '=')
        {
            token->type = Ix_colon_eq;
        }
        else
        {
            token->type = Ix_colon;
            fd->UngetChar(currentChar);
        }
    }
    else if (currentChar == '=')
    {
        token->type = lx_eq;
    }
    else if (currentChar == '!')
    {
        currentChar = fd->GetChar();
        if (currentChar == '=')
        {
            token->type = lx_neq;
        }
        else
        {
            fd->ReportError("Error: Invalid operator representation: '!' must be followed by '='");
            return nullptr;
        }
    }
    else if (currentChar == '<')
    {
        currentChar = fd->GetChar();
        if (currentChar == '=')
        {
            token->type = lx_le;
        }
        else
        {
            token->type = lx_lt;
            fd->UngetChar(currentChar);
        }
    }
    else if (currentChar == '>')
    {
        currentChar = fd->GetChar();
        if (currentChar == '=')
        {
            token->type = lx_ge;
        }
        else
        {
            token->type = lx_gt;
            fd->UngetChar(currentChar);
        }
    }
    else if (currentChar == '(')
    {
        token->type = lx_lparen;
    }
    else if (currentChar == ')')
    {
        token->type = lx_rparen;
    }
    else if (currentChar == '{')
    {
        token->type = lx_lbracket;
    }
    else if (currentChar == '}')
    {
        token->type = lx_rbracket;
    }
    else if (currentChar == ',')
    {
        token->type = lx_comma;
    }
    else if (currentChar == '[')
    {
        token->type = lx_lsbracket;
    }
    else if (currentChar == ']')
    {
        token->type = lx_rsbracket;
    }

    privousType = OPERATOR;
    return token;
}

FileDescriptor *Scanner::Get_fd()
{
    return fd;
}
