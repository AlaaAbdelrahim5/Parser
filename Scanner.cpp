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
    // Get the next character from the input stream
    char currentChar = fd->GetChar();

    // Skip whitespace and comments
    while (isspace(currentChar) || getClass(currentChar) == COMMENT_MARKER)
    {
        if (isspace(currentChar))
            skipSpaces(currentChar); // Skip over spaces and newline characters

        if (getClass(currentChar) == COMMENT_MARKER)
        {
            currentChar = fd->GetChar();
            if (getClass(currentChar) == COMMENT_MARKER)
            {
                skipComments(currentChar); // Skip over multi-line comments
            }
            else
            {
                // Report an error if an incomplete comment is encountered
                fd->ReportError("Incomplete or wrong comment entered.");
                return NULL;
            }
        }
    }

    // Check if end of file is reached
    if (currentChar == EOF)
    {
        readMore = false;
        TOKEN *token = new TOKEN();
        token->type = lx_eof; // Set token type to end-of-file
        lastToken = token;
        return token;
    }
    // Check for individual characters that represent tokens
    if (currentChar == '-')
    {
        TOKEN *minusToken = new TOKEN();
        minusToken->type = lx_minus; // Set minusToken type to minus operator
        privousType = OPERATOR;
        lastToken = minusToken;
        return minusToken;
    }
    if (currentChar == ';')
    {
        TOKEN *semicolonToken = new TOKEN();
        semicolonToken->type = lx_semicolon; // Set semicolonToken type to semicolon
        lastToken = semicolonToken;
        return semicolonToken;
    }
    if (getClass(currentChar) == OPERATOR)
    {
        lastToken = getOperator(currentChar); // Get operator token
        return lastToken;
    }
    if (currentChar == '"')
    {
        lastToken = getString(currentChar); // Get string literal token
        return lastToken;
    }

    // Determine the class of the character
    int charType = getClass(currentChar);

    // Handle identifiers and keywords
    if (charType == LETTER_CHAR || currentChar == '_')
    {
        lastToken = getId(currentChar); // Get identifier or keyword token
        return lastToken;
    }
    // Handle integer and floating-point literals
    else if (charType == NUMERIC_DIGIT)
    {
        lastToken = getInt(currentChar); // Get integer or floating-point token
        return lastToken;
    }

    // Report an error for unknown tokens
    fd->ReportError("Unknown Token");
    lastToken = NULL;
    return NULL;
}

TOKEN *Scanner::getId(char c)
{
    string idStr;                   // Stores the value of the identifier
    TOKEN *token = NULL;            // Token to be returned
    int currentClass = getClass(c); // Determine the class of the current character

    // Loop to read characters until the identifier is complete
    while (currentClass == NUMERIC_DIGIT || currentClass == LETTER_CHAR ||
           currentClass == SPECIAL_CHAR)
    {
        idStr += c;                 // Add the character to the identifier value
        c = fd->GetChar();          // Read the next character
        currentClass = getClass(c); // Update the class of the next character
    }

    // Check if the identifier is properly terminated
    if (currentClass != SEPARATOR && currentClass != OPERATOR && c != EOF)
    {
        fd->ReportError("Invalid identifier");
        return NULL;
    }

    // Handle special case for operators or delimiter characters
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
        token->type = lx_identifier; // Set token type as identifier
        token->str_ptr = new char[idStr.size() + 1];
        strcpy(token->str_ptr, idStr.data());
        privousType = -2;
        // cout << "Identifier value: " << idStr << endl;
        return token;
    }
}

TOKEN *Scanner::getInt(char c)
{
    TOKEN *token = NULL;
    int currentClass = getClass(c); // Determine the initial class of the character
    string intStr;                  // To store the token intStr

    // Continue scanning while the character belongs to the digit class
    while (currentClass == NUMERIC_DIGIT)
    {
        intStr += c;                // Add the current digit to the intStr
        c = fd->GetChar();          // Get the next character
        currentClass = getClass(c); // Determine the class of the new character
    }

    // If the scanned token is a valid integer
    if (currentClass == SEPARATOR || currentClass == OPERATOR || c == '\n' || currentClass == COMMENT_MARKER || c == EOF)
    {

        if (currentClass == OPERATOR || c == ';' || currentClass == COMMENT_MARKER || c == EOF)
            fd->UngetChar(c); // Push back the character that doesn't belong to the integer

        token = new TOKEN();
        token->type = lx_integer;    // Set the token type to integer
        token->value = stoi(intStr); // Convert the string intStr to an integer
        privousType = -2;
        return token;
    }
    // If the scanned token might be a floating-point number
    else if (currentClass == lx_dot)
    {

        intStr += c;                // Add the dot to the intStr
        c = fd->GetChar();          // Get the next character
        currentClass = getClass(c); // Determine the class of the new character

        // Continue scanning while the character belongs to the digit class
        while (currentClass == NUMERIC_DIGIT)
        {
            intStr += c;                // Add the current digit to the intStr
            c = fd->GetChar();          // Get the next character
            currentClass = getClass(c); // Determine the class of the new character
        }

        // If the scanned token is a valid floating-point number
        if (currentClass == SEPARATOR || currentClass == OPERATOR || currentClass == COMMENT_MARKER || c == EOF)
        {

            if (currentClass == OPERATOR || currentClass == COMMENT_MARKER || c == ';' || c == EOF)
                fd->UngetChar(c); // Push back the character that doesn't belong to the floating-point number

            token = new TOKEN();
            token->type = lx_float;            // Set the token type to float
            token->float_value = stof(intStr); // Convert the string intStr to a float
            privousType = -2;
        }
        // If the scanned token is an invalid floating-point number
        else
        {
            fd->ReportError("Invalid floating-point number");
            return NULL;
        }
    }
    // If the scanned token is an invalid integer number
    else
    {
        fd->ReportError("Invalid integer number");
        return NULL;
    }
    return token;
}

TOKEN *Scanner::getString(char startChar)
{
    string stringStr;                 // To store the value of the string token
    char currentChar = fd->GetChar(); // Get the next character from the file descriptor

    // Loop to read characters until the ending quote or end of line/EOF is encountered
    while (currentChar != '\n' && currentChar != EOF)
    {
        if (currentChar == '\\')
        {
            // Handle escaped characters
            stringStr += currentChar;    // Add the backslash
            currentChar = fd->GetChar(); // Get the escaped character
            stringStr += currentChar;    // Add the escaped character
        }
        else if (currentChar == '"')
        {
            // End of the string
            break;
        }
        else
        {
            stringStr += currentChar; // Add the character to the token value
        }
        currentChar = fd->GetChar(); // Get the next character
    }

    // Check if the loop terminated due to an invalid string representation
    if (currentChar == '\n' || currentChar == EOF)
    {
        fd->ReportError("Unfinished string ");
        return nullptr;
    }

    TOKEN *token = new TOKEN();
    token->type = lx_string;

    // Allocate memory for the token's string value and copy the token value into it
    token->str_ptr = new char[stringStr.size() + 1];
    strcpy(token->str_ptr, stringStr.data());

    privousType = -2; // Update the previous token type
    return token;
}

int Scanner::checkKeyword(char *word)
{
    // Check if the given word is a keyword, and return its index
    for (unsigned int i = 0; i < sizeof(lexTypes) / sizeof(LEXEME_TYPE); i++)
    {
        if (strcmp(word, keywords[i]) == 0)
        {
            return i;
        }
    }

    // Return -1 if the word is not a keyword
    return -1;
}

void Scanner::skipComments(char &currentChar)
{
    while (true)
    {
        currentChar = fd->GetChar();
        while (currentChar != '#' && currentChar != '\n' && currentChar != EOF)
            currentChar = fd->GetChar();

        // Check if a new line was encountered
        if (currentChar == '\n')
            return;

        // Check if the end of the file was reached
        if (currentChar == EOF)
        {
            return;
        }

        // Handle nested comment symbol
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
    if (isalpha(c)) // Check if the character is an alphabetic character
    {
        return LETTER_CHAR;
    }
    else if (c >= '0' && c <= '9') // Check if the character is a digit
    {
        return NUMERIC_DIGIT;
    }
    else if (c == ';' || c == ' ' || c == '\n' || isspace(c) || c == EOF) // Check for delimiters (whitespace, newline, etc.)
    {
        return SEPARATOR;
    }
    else if (c == '(' || c == ')' || c == '+' || c == '*' ||
             c == '/' || c == '=' || c == '[' || c == ']' ||
             c == '{' || c == '}' || c == ',' || c == ':' ||
             c == '=' || c == '>' || c == '<' || c == '-' ||
             c == '!') // Check for operators
    {
        return OPERATOR;
    }
    else if (c == '.') // Check for the dot character
    {
        return lx_dot;
    }
    else if (c == '_') // Check for underscore character
    {
        return SPECIAL_CHAR;
    }
    else if (c == '#') // Check for comment character
    {
        return COMMENT_MARKER;
    }

    return 0; // Default case: return 0 for unknown characters
}

TOKEN *Scanner::getOperator(char currentChar)
{
    TOKEN *token = new TOKEN();

    // Check for various operator characters and assign appropriate token types
    if (currentChar == '+')
    { // Plus operator
        token->type = lx_plus;
    }
    else if (currentChar == '-')
    { // Minus operator
        token->type = lx_minus;
    }
    else if (currentChar == '*')
    { // Multiply operator
        token->type = lx_star;
    }
    else if (currentChar == '/')
    { // Divide operator
        token->type = lx_slash;
    }

    // Handle special cases like colons and comparison operators
    if (currentChar == ':')
    {
        currentChar = fd->GetChar();
        if (currentChar == '=')
        { // Assignment operator
            token->type = Ix_colon_eq;
        }
        else
        { // Colon operator
            token->type = Ix_colon;
            fd->UngetChar(currentChar); // Put back the character into the input stream
        }
    }
    else if (currentChar == '=')
    { // Equal operator
        token->type = lx_eq;
    }
    else if (currentChar == '!')
    {
        currentChar = fd->GetChar();
        if (currentChar == '=')
        { // Not equal operator
            token->type = lx_neq;
        }
        else
        {
            // Report error for invalid not operator representation
            fd->ReportError("Error: Invalid operator representation: '!' must be followed by '='");
            return nullptr;
        }
    }
    else if (currentChar == '<')
    {
        currentChar = fd->GetChar();
        if (currentChar == '=')
        { // Less than or equal operator
            token->type = lx_le;
        }
        else
        { // Less than operator
            token->type = lx_lt;
            fd->UngetChar(currentChar); // Put back the character into the input stream
        }
    }
    else if (currentChar == '>')
    {
        currentChar = fd->GetChar();
        if (currentChar == '=')
        { // Greater than or equal operator
            token->type = lx_ge;
        }
        else
        { // Greater than operator
            token->type = lx_gt;
            fd->UngetChar(currentChar); // Put back the character into the input stream
        }
    }
    else if (currentChar == '(')
    { // Left parenthesis
        token->type = lx_lparen;
    }
    else if (currentChar == ')')
    { // Right parenthesis
        token->type = lx_rparen;
    }
    else if (currentChar == '{')
    { // Left curly brace
        token->type = lx_lbracket;
    }
    else if (currentChar == '}')
    { // Right curly brace
        token->type = lx_rbracket;
    }
    else if (currentChar == ',')
    { // Comma
        token->type = lx_comma;
    }
    else if (currentChar == '[')
    { // Left square bracket
        token->type = lx_lsbracket;
    }
    else if (currentChar == ']')
    { // Right square bracket
        token->type = lx_rsbracket;
    }

    privousType = OPERATOR; // Update the previous token type
    return token;
}

FileDescriptor *Scanner::Get_fd()
{
    return fd;
}
