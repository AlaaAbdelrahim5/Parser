#include "Parser.h"

Parser::Parser(FileDescriptor *fileDescriptor)
{
    scanner = new Scanner(fileDescriptor);
    table = new STable(1);
    programAST = nullptr;
}

ast_list *Parser::parse()
{
    programAST = new ast_list();
    programAST->head = nullptr;
    programAST->tail = nullptr;

    TOKEN *token = scanner->Scan();

    if (token == nullptr)
        return nullptr;

    programAST = decl_list(token);

    FILE *outputFile = fopen("c:\\Users\\Ala'a\\Downloads\\Parser\\output.txt", "w");

    ast_list *currentASTNode = programAST;
    while (currentASTNode != nullptr && currentASTNode->head != nullptr)
    {
        print_ast_node(outputFile, currentASTNode->head);
        currentASTNode = currentASTNode->tail;
    }

    fclose(outputFile);

    return programAST;
}

ast_list *Parser::decl_list(TOKEN *currentToken)
{
    if (currentToken == NULL)
    {
        return NULL;
    }

    AST *ast = nullptr;

    switch (currentToken->type)
    {
    case kw_var:
        ast = var_decl(currentToken);
        if (ast == nullptr)
        {
            delete programAST;
            programAST = nullptr;
            return nullptr;
        }
        programAST = addNodeToAstList(ast, programAST);
        currentToken = scanner->Scan();
        break;

    case kw_constant:
        ast = constant_decl(currentToken);
        if (ast == nullptr)
        {
            delete programAST;
            programAST = nullptr;
            return nullptr;
        }
        programAST = addNodeToAstList(ast, programAST);
        currentToken = scanner->getLastToken();
        break;

    case kw_function:
        ast = function_decl(currentToken);
        if (ast == nullptr)
        {
            delete programAST;
            programAST = nullptr;
            return nullptr;
        }
        programAST = addNodeToAstList(ast, programAST);
        currentToken = scanner->Scan();
        break;

    case kw_procedure:
        ast = procedure_decl(currentToken);
        if (ast == nullptr)
        {
            delete programAST;
            programAST = nullptr;
            return nullptr;
        }
        programAST = addNodeToAstList(ast, programAST);
        currentToken = scanner->Scan();
        break;

    case lx_eof:
        ast = make_ast_node(ast_eof);
        if (ast == nullptr)
        {
            delete programAST;
            programAST = nullptr;
            return nullptr;
        }
        programAST = addNodeToAstList(ast, programAST);
        return programAST;

    default:
        reportError("Invalid declaration type. Expected 'var', 'constant', 'function', or 'procedure'");
        return nullptr;
    }

    if (!matchLexeme(currentToken, lx_semicolon))
    {
        reportError("Missing semicolon (;) after the declaration");
        return nullptr;
    }

    currentToken = scanner->Scan();
    return decl_list(currentToken);
}

AST *Parser::var_decl(TOKEN *token)
{
    TOKEN *identifier_token = scanner->Scan();
    int line_number = scanner->getLineNum();
    if (!matchLexeme(identifier_token, lx_identifier))
    {
        reportError("A valid identifier is required after the 'var' keyword");
        return nullptr;
    }

    TOKEN *colon_token = scanner->Scan();
    if (!matchLexeme(colon_token, Ix_colon))
    {
        reportError("A colon (:) is required after the variable name in the 'var' declaration");
        return nullptr;
    }

    TOKEN *var_type_token = scanner->Scan();
    LEXEME_TYPE var_type = var_type_token->type;
    if (!matchType(var_type_token))
    {
        reportError("A data type must be specified after the colon in the 'var' declaration");
        return nullptr;
    }

    // Check if this identifier is already declared in the current scope only
    STEntry *ST_Entry = table->Get_entry(identifier_token->str_ptr);
    if (ST_Entry != nullptr)
    {
        reportError("Variable '" + string(identifier_token->str_ptr) + "' is already declared in the current scope");
        return nullptr;
    }

    // Create a new symbol table entry
    ST_Entry = table->Put_symbol(identifier_token->str_ptr);
    if (ST_Entry == nullptr)
    {
        reportError("Failed to add a new entry to the Symbol Table. No corresponding token is available");
        return nullptr;
    }

    // Set the values for the symbol table entry
    ST_Entry = setSTEValues(STE_VAR, ST_Entry, line_number, getTokenType(var_type));
    if (ST_Entry->Name == nullptr)
    {
        reportError("Failed to set values for the symbol table entry");
        return nullptr;
    }

    // Create the AST node for the variable declaration
    AST *ast = make_ast_node(ast_var_decl, ST_Entry, ST_Entry->f.var.type);

    return ast;
}

AST *Parser::constant_decl(TOKEN *token)
{
    TOKEN *identifier_token = scanner->Scan();
    int lineNum = scanner->getLineNum();
    if (identifier_token->type != lx_identifier)
    {
        reportError("A valid identifier is required after the 'constant' keyword");
        return nullptr;
    }

    if (table->Get_entry(identifier_token->str_ptr) != nullptr)
    {
        reportError("Multiple declarations of constant: " + string(identifier_token->str_ptr));
        return nullptr;
    }

    TOKEN *Equal_token = scanner->Scan();
    if (Equal_token->type != lx_eq)
    {
        reportError("Expected '=' after the constant identifier");
        return nullptr;
    }

    // Parse the expression and create AST
    TOKEN *constant_token = scanner->Scan();
    AST *ast_exp = parseE(constant_token);
    if (ast_exp == nullptr)
    {
        reportError("Failed to parse the constant expression");
        return nullptr;
    }

    // Evaluate the AST expression to get the constant value
    int value = eval_ast_expr(scanner->Get_fd(), ast_exp);

    // Create a new entry in the Symbol Table for the constant
    STEntry *ST_Entry = table->Put_symbol(identifier_token->str_ptr);
    if (ST_Entry == nullptr)
    {
        return nullptr;
    }

    // Set values for the constant entry
    ST_Entry = setSTEValues(STE_CONST, ST_Entry, lineNum, value);

    // Create the AST node for the constant declaration
    AST *ast = make_ast_node(ast_const_decl, ST_Entry, ST_Entry->f.constant.value);

    return ast;
}

AST *Parser::function_decl(TOKEN *token)
{
    token = scanner->Scan();
    if (token->type != lx_identifier)
    {
        reportError("Expected an identifier after the 'function' keyword");
        return nullptr;
    }

    char *name = token->str_ptr;
    int line = scanner->getLineNum();

    // Check if the function name is already declared in the current scope
    if (table->Get_entry(name) != nullptr)
    {
        reportError("The function or procedure '" + string(name) + "' is already declared");
        return nullptr;
    }

    // Create a symbol table entry for the function in the current scope
    symbol_table_entry *ST_Entry = table->Put_symbol(name);
    ST_Entry = setSTEValues(STE_ROUTINE, ST_Entry, line, STE_NONE); // Initialize the entry

    // Enter a new scope for function parameters and body
    table->enter_scope();

    // Parse the function <formal_list>
    token = scanner->Scan();
    ste_list *formals_list = formal_list(token);

    token = scanner->getLastToken();
    if (!matchLexeme(token, Ix_colon))
    {
        reportError("Expected a colon (:) after the function's formal parameter list");
        table->exit_scope();
        return nullptr;
    }

    token = scanner->Scan();
    if (!matchType(token))
    {
        reportError("A valid return type is required after the colon (:)");
        table->exit_scope();
        return nullptr;
    }

    // Get the return type of the function and set the symbol table entry values
    LEXEME_TYPE type = token->type;
    ST_Entry = setSTEValues(STE_ROUTINE, ST_Entry, line, getTokenType(type));

    token = scanner->Scan();
    AST *ast_block = block_stmt(token);
    if (ast_block == nullptr)
    {
        reportError("Failed to parse the function body");
        table->exit_scope();
        return nullptr;
    }

    // Create the AST node for the function declaration
    AST *func_ast = make_ast_node(ast_routine_decl, ST_Entry, formals_list, ST_Entry->f.routine.result_type, ast_block);

    table->exit_scope();

    return func_ast;
}

AST *Parser::procedure_decl(TOKEN *token)
{
    token = scanner->Scan();
    if (token->type != lx_identifier)
    {
        reportError("Expected an identifier after the 'procedure' keyword");
        return nullptr;
    }

    char *name = token->str_ptr;
    int lineNum = scanner->getLineNum();

    if (table->Get_entry(name) != nullptr)
    {
        reportError("Multiple declarations of procedure: " + string(name));
        return nullptr;
    }

    // Create a symbol table entry for the procedure in the current scope
    symbol_table_entry *st_entry = table->Put_symbol(name);
    st_entry = setSTEValues(STE_ROUTINE, st_entry, lineNum, STE_NONE);

    // Enter a new scope for procedure parameters and body
    table->enter_scope();

    token = scanner->Scan();
    ste_list *formals_list = formal_list(token);
    if (formals_list == nullptr)
    {
        reportError("Failed to parse the formal parameter list");
        table->exit_scope();
        return nullptr;
    }

    // Get the last token of the formal parameter list
    token = scanner->getLastToken();

    AST *ast_block = block_stmt(token);
    if (ast_block == nullptr)
    {
        reportError("Failed to parse the procedure body");
        table->exit_scope();
        return nullptr;
    }

    // Create an AST node for the procedure declaration
    AST *ast_procedure = make_ast_node(ast_routine_decl, st_entry, formals_list, st_entry->f.routine.result_type, ast_block);

    table->exit_scope();

    return ast_procedure;
}

bool Parser::matchType(TOKEN *token)
{
    if (token == nullptr)
    {
        return false;
    }

    switch (token->type)
    {
    case kw_integer:
    case kw_string:
    case kw_float:
    case kw_bool:
        return true;
    default:
        return false;
    }
}

STE_TYPE Parser::getTokenType(LEXEME_TYPE token_type)
{
    switch (token_type)
    {
    case kw_integer:
        return STE_INT;
    case kw_float:
        return STE_FLOAT;
    case kw_string:
        return STE_STRING;
    case kw_bool:
        return STE_BOOL;
    default:
        return STE_NONE;
    }
}

ste_list *Parser::formal_list(TOKEN *token)
{
    if (!matchLexeme(token, lx_lparen))
    {
        reportError("Expected '(' at the beginning of the formal parameter list");
        return nullptr;
    }

    token = scanner->Scan();
    return formal_list_tail(token);
}

ste_list *Parser::formal_list_tail(TOKEN *token)
{
    if (matchLexeme(token, lx_rparen))
    {
        token = scanner->Scan();
        return nullptr;
    }
    else
    {
        // Create a new list to store Formals
        ste_list *formals_list = new ste_list();
        formals_list->head = nullptr;
        formals_list->tail = nullptr;

        return formals(token, formals_list);
    }
}

ste_list *Parser::formals(TOKEN *token, ste_list *STE_list_x)
{
    if (!matchLexeme(token, lx_identifier))
    {
        reportError("Expected an identifier");
        return nullptr;
    }

    char *name = token->str_ptr;

    token = scanner->Scan();
    if (!matchLexeme(token, Ix_colon))
    {
        reportError("Expected a colon (:) after the identifier");
        return nullptr;
    }

    token = scanner->Scan();
    if (!matchType(token))
    {
        reportError("A valid type is required after the colon (:) in the formal parameter list");
        return nullptr;
    }

    // Check if the identifier is already declared in the current scope
    symbol_table_entry *ST_Entry = table->Put_symbol(name);
    int line = scanner->getLineNum();

    // Set the values for the parameter symbol table entry
    ST_Entry = setSTEValues(STE_VAR, ST_Entry, line, getTokenType(token->type)); // Add the symbol table entry to the list
    STE_list_x = addNodeToSteList(ST_Entry, STE_list_x);

    token = scanner->Scan();
    STE_list_x = formals_tail(token, STE_list_x);

    // Return the updated list of symbol table entries
    return STE_list_x;
}

ste_list *Parser::formals_tail(TOKEN *token, ste_list *STE_list_x)
{
    if (matchLexeme(token, lx_rparen))
    {
        token = scanner->Scan();
        return STE_list_x;
    }
    else if (matchLexeme(token, lx_comma))
    {
        token = scanner->Scan();
        STE_list_x = formals(token, STE_list_x);
        return STE_list_x;
    }
    else
    {
        reportError("Expected a right parenthesis ')' or a comma ',' in the formal parameter list");
        return STE_list_x;
    }
}

AST *Parser::stmt(TOKEN *token)
{
    AST *ast;

    if (token == nullptr)
    {
        reportError("Token is null, unexpected error encountered.");
        return nullptr;
    }

    switch (token->type)
    {
    case lx_identifier:
        ast = callAssign_stmt(token);
        break;
    case kw_if:
        ast = if_stmt(token);
        break;
    case kw_while:
        ast = while_stmt(token);
        break;
    case kw_for:
        ast = for_stmt(token);
        break;
    case kw_read:
        ast = read_stmt(token);
        break;
    case kw_write:
        ast = write_stmt(token);
        break;
    case kw_return:
        ast = return_stmt(token);
        break;
    case kw_begin:
        ast = block_stmt(token);
        break;
    default:
        reportError("Invalid or unrecognized statement encountered");
        return nullptr;
    }

    return ast;
}

AST *Parser::callAssign_stmt(TOKEN *token)
{
    if (token == nullptr || !matchLexeme(token, lx_identifier))
    {
        reportError("Expected an identifier");
        return nullptr;
    }

    // Look up the identifier in the symbol table
    char *name = token->str_ptr;
    symbol_table_entry *ST_Entry = table->Get_symbol(name);
    if (ST_Entry == nullptr)
    {
        reportError("Undeclared identifier: " + string(name));
        return nullptr;
    }

    if (ST_Entry->steType == STE_VAR)
    {
        // Parse assignment statement if the entry is a variable
        return assign_stmt(token);
    }
    if (ST_Entry->steType == STE_ROUTINE)
    {
        // Parse function call statement if the entry is a routine
        return call_stmt(token);
    }

    reportError("Invalid statement encountered. Expected a valid identifier or routine");
    return nullptr;
}

AST *Parser::assign_stmt(TOKEN *token)
{
    if (token == nullptr || !matchLexeme(token, lx_identifier))
    {
        reportError("Expected an identifier");
        return nullptr;
    }

    // Look up the identifier in the symbol table
    char *name = token->str_ptr;
    symbol_table_entry *ST_Entry = table->Get_symbol(name);
    if (ST_Entry == nullptr)
    {
        reportError("Undeclared identifier: " + string(name));
        return nullptr;
    }

    // Check if the symbol table entry represents a variable
    if (ST_Entry->steType != STE_VAR)
    {
        reportError("Assignment is only allowed to variables");
        return nullptr;
    }

    token = scanner->Scan();
    if (token == nullptr || !matchLexeme(token, Ix_colon_eq))
    {
        reportError("Expect := ");
        return nullptr;
    }

    // Parse the expression on the right-hand side of the assignment
    token = scanner->Scan();
    AST *ast_Exp = parseE(token);
    if (ast_Exp == nullptr)
    {
        return nullptr;
    }

    // Create an AST node for the assignment statement
    AST *assign_ast = make_ast_node(ast_assign, ST_Entry, ast_Exp);
    return assign_ast;
}

AST *Parser::call_stmt(TOKEN *token)
{
    // Check if the token is nullptr or an identifier
    if (token == nullptr || !matchLexeme(token, lx_identifier))
    {
        reportError("Expect an Identifier");
        return nullptr; // Return nullptr if token is not an identifier
    }
    char *name = token->str_ptr;

    // Look up the identifier in the symbol table
    symbol_table_entry *ST_Entry = table->Get_symbol(name);
    if (ST_Entry == nullptr)
    {
        reportError("Undeclared Variable or Function. ");
        return nullptr; // Return nullptr if the identifier is not found in the symbol table
    }

    // Check if the symbol table entry represents a routine (function or procedure)
    if (ST_Entry->steType != STE_ROUTINE)
    {
        reportError("You can't call anything other than routines ");
        return nullptr; // Return nullptr if the entry is not a routine
    }
    token = scanner->Scan(); // Get the next token

    // Parse the argument list for the function call
    ast_list *AST_list_x = arg_list(token);

    // Create an AST node for the function call statement
    AST *call_ast = make_ast_node(ast_call, ST_Entry, AST_list_x);
    return call_ast;
}

AST *Parser::if_stmt(TOKEN *token)
{
    token = scanner->Scan(); // Get the next token

    // Parse the expression for the 'if' condition
    AST *ast_Exp = parseE(token);
    if (ast_Exp == nullptr)
    {
        return nullptr; // Return NULL if expression parsing fails
    }

    token = scanner->getLastToken(); // Get the last token read

    // Check if 'then' keyword is present after the expression
    if (!matchLexeme(token, kw_then))
    {
        reportError("Expecting then, 'then' keyword is missing"); // Report error if 'then' keyword is missing
        return nullptr;
    }

    token = scanner->Scan(); // Get the next token

    // Parse the consequent block of the 'if' statement
    AST *ast_conseq = stmt(token);
    if (ast_conseq == nullptr)
    {
        return nullptr; // Return NULL if parsing of consequent block fails
    }

    token = scanner->Scan(); // Get the next token

    // Call the function to parse the optional 'else' part of the 'if' statement
    return if_tail(token, ast_Exp, ast_conseq);
}

AST *Parser::if_tail(TOKEN *token, AST *ast_Exp, AST *ast_conseq)
{
    AST *if_ast, *ast_alter;

    // Check if 'fi' keyword is present, indicating the end of the if statement
    if (matchLexeme(token, kw_fi))
    {
        ast_alter = nullptr; // No alternative block (no 'else' part)
    }
    // Check if 'else' keyword is present, indicating the presence of an alternative block
    else if (matchLexeme(token, kw_else))
    {
        token = scanner->Scan(); // Get the next token
        ast_alter = stmt(token); // Parse the alternative block
        if (ast_alter == nullptr)
        {
            return nullptr; // Return NULL if parsing of alternative block fails
        }

        token = scanner->Scan(); // Get the next token

        // Check if 'fi' keyword is present after the alternative block
        if (!matchLexeme(token, kw_fi))
        {
            reportError("Expecting fi keyword");
            return nullptr;
        }
    }
    else
    {
        reportError("Expecting fi keyword or else keyword");
        return nullptr;
    }

    // Create an AST node for the if statement, including its condition, consequent block, and alternative block
    if_ast = make_ast_node(ast_if, ast_Exp, ast_conseq, ast_alter);
    return if_ast; // Return the constructed AST node for the if statement
}

AST *Parser::while_stmt(TOKEN *token)
{
    // Check if the token matches the 'while' keyword
    if (token == nullptr || !matchLexeme(token, kw_while))
    {
        return nullptr;
    }
    token = scanner->Scan();

    // Parse the loop condition expression
    AST *ast_Exp = parseE(token);
    if (ast_Exp == nullptr)
    {
        return nullptr;
    }
    token = scanner->getLastToken();

    // Check if the next token is the 'do' keyword
    if (token == nullptr || !matchLexeme(token, kw_do))
    {
        reportError("Expect 'do' keyword");
        return nullptr;
    }
    token = scanner->Scan();

    // Parse the loop body statement
    AST *ast_stmnt = stmt(token);
    if (ast_stmnt == nullptr)
    {
        return nullptr;
    }

    // Check for the 'od' keyword
    token = scanner->Scan();
    if (token == nullptr || !matchLexeme(token, kw_od))
    {
        reportError("Expect 'od' keyword. ");
        return nullptr;
    }

    // Create an AST node for the while loop
    AST *while_ast = make_ast_node(ast_while, ast_Exp, ast_stmnt);
    return while_ast;
}

AST *Parser::for_stmt(TOKEN *token)
{
    // Check if the token matches the 'for' keyword
    if (!matchLexeme(token, kw_for))
    {
        reportError("Invalid for loop");
        return nullptr;
    }
    delete token;
    token = scanner->Scan();

    // Check if the next token is an identifier
    if (token == nullptr || !matchLexeme(token, lx_identifier))
    {
        reportError("Expect an identifier. ");
        return nullptr;
    }

    // Parse the assignment statement for the loop variable
    AST *ast_assign = assign_stmt(token);
    token = scanner->getLastToken();
    if (ast_assign == nullptr)
    {
        return nullptr;
    }

    // Check if the next token is the 'to' keyword
    if (token == nullptr || !matchLexeme(token, kw_to))
    {
        reportError("Expect 'to' keyword");
        return nullptr;
    }

    token = scanner->Scan();

    // Parse the loop condition expression
    AST *ast_Exp = parseE(token);
    if (ast_Exp == nullptr)
    {
        return nullptr;
    }
    token = scanner->getLastToken();

    // Check if the next token is the 'do' keyword
    if (token == nullptr || !matchLexeme(token, kw_do))
    {
        reportError("Expect 'do' keyword");
        return nullptr;
    }
    token = scanner->Scan();

    // Parse the loop body statement
    AST *ast_stmnt = stmt(token);

    // Check for the 'od' keyword
    token = scanner->Scan();
    if (ast_stmnt == nullptr)
    {
        return nullptr;
    }
    if (token == nullptr || !matchLexeme(token, kw_od))
    {
        reportError("Expected 'od' keyword ");
        return nullptr;
    }

    // Create an AST node for the for loop
    AST *for_ast = make_ast_node(ast_for, ast_assign->f.a_var.var, ast_assign, ast_Exp, ast_stmnt);
    return for_ast;
}

AST *Parser::read_stmt(TOKEN *token)
{
    // Check if the token matches the 'read' keyword
    if (!matchLexeme(token, kw_read))
    {
        return nullptr; // Return NULL if the token is not 'read'
    }
    delete token;            // Delete the used token
    token = scanner->Scan(); // Get the next token

    // Check if the next token is '('
    if (!matchLexeme(token, lx_lparen))
    {
        reportError("Expect '(' after Read keyword");
        return nullptr; // Return NULL if '(' is not found
    }
    delete token;            // Delete the used token
    token = scanner->Scan(); // Get the next token

    // Check if the next token is an identifier
    if (!matchLexeme(token, lx_identifier))
    {
        reportError("Expect identifier after '(' in Read statement");
        return nullptr; // Return NULL if an identifier is not found
    }
    char *name = token->str_ptr;

    // Look up the identifier in the symbol table
    symbol_table_entry *ST_Entry = table->Get_symbol(name);
    if (ST_Entry == nullptr)
    {
        reportError("Undefined Variable: " + string(name));
        return nullptr; // Return NULL if the variable is not found in the symbol table
    }

    token = scanner->Scan(); // Get the next token

    // Check if the next token is ')'
    if (!matchLexeme(token, lx_rparen))
    {
        reportError("Expect ')' after Read");
        return nullptr; // Return NULL if ')' is not found
    }

    // Create an AST node for the Read statement
    AST *ast = make_ast_node(ast_read, ST_Entry);
    return ast; // Return the constructed AST node
}

AST *Parser::write_stmt(TOKEN *token)
{
    // Check if the token matches the 'write' keyword
    if (!matchLexeme(token, kw_write))
    {
        return nullptr; // Return nullptr if the token is not 'write'
    }
    delete token;            // Delete the used token
    token = scanner->Scan(); // Get the next token

    // Check if the next token is '('
    if (!matchLexeme(token, lx_lparen))
    {
        reportError("Expect '(' in Write");
        return nullptr; // Return nullptr if '(' is not found
    }
    delete token;            // Delete the used token
    token = scanner->Scan(); // Get the next token

    // Check if the next token is an identifier
    if (!matchLexeme(token, lx_identifier))
    {
        reportError("Expect identifier in Write");
        return nullptr; // Return nullptr if an identifier is not found
    }
    char *name = token->str_ptr;

    // Look up the identifier in the symbol table
    symbol_table_entry *ST_Entry = table->Get_symbol(name);
    if (ST_Entry == nullptr)
    {
        reportError("Undefined Variable " + string(name));
        return nullptr; // Return nullptr if the variable is not found in the symbol table
    }
    token = scanner->Scan(); // Get the next token

    // Check if the next token is ')'
    if (!matchLexeme(token, lx_rparen))
    {
        reportError("Expect ')' in Write");
        return nullptr; // Return nullptr if ')' is not found
    }

    // Create an AST node for the Write statement
    AST *ast = make_ast_node(ast_write, ST_Entry);
    return ast; // Return the constructed AST node
}

AST *Parser::return_stmt(TOKEN *token)
{
    // Check if the token matches 'kw_return'
    if (!matchLexeme(token, kw_return))
    {
        return nullptr;
    }
    delete token;
    token = scanner->Scan();

    // Check for opening parenthesis after 'return'
    if (!matchLexeme(token, lx_lparen))
    {
        reportError("Expect '(' in Return");
        return nullptr;
    }
    delete token;
    token = scanner->Scan();

    // Parse the expression following 'return'
    AST *ast_Exp = parseE(token);
    if (ast_Exp == nullptr)
    {
        return nullptr;
    }
    token = scanner->getLastToken();

    // Check for closing parenthesis after the expression
    if (!matchLexeme(token, lx_rparen))
    {
        reportError("Expect ')' in Return");
        return nullptr;
    }

    // Create an AST node for the return statement
    AST *ast = make_ast_node(ast_return, ast_Exp);
    return ast;
}

AST *Parser::block_stmt(TOKEN *token)
{
    // Initialize offset for variable declaration offsets
    offset = 0;

    // Check if the token matches the 'begin' keyword
    if (token == nullptr || !matchLexeme(token, kw_begin))
    {
        reportError("Expected begin");
        return nullptr;
    }

    // Enter a new scope for this block
    table->enter_scope();

    token = scanner->Scan();

    // Initialize a linked list for variable declarations
    ste_list *var_dec_list;
    var_dec_list = new ste_list();
    var_dec_list->head = nullptr;
    var_dec_list->tail = nullptr; // Parse variable declaration list and assign offsets
    var_dec_list = var_decl_list(token, var_dec_list);
    ste_list *p = var_dec_list;
    while (p != nullptr)
    {
        if (p->head == nullptr)
        {
            break;
        }
        p->head->offset = offset;
        offset++;
        p = p->tail;
    }

    // Return if no variable declarations are found
    if (var_dec_list == nullptr)
    {
        table->exit_scope(); // Exit the scope before returning
        return nullptr;
    }

    token = scanner->getLastToken();

    // Initialize a linked list for statement parsing
    ast_list *stmnt_list;
    stmnt_list = new ast_list();
    stmnt_list->head = nullptr;
    stmnt_list->tail = nullptr;

    // Parse statement list
    stmnt_list = stmt_list(token, stmnt_list);

    // Return if no statements are found
    if (stmnt_list == nullptr)
    {
        table->exit_scope(); // Exit the scope before returning
        return nullptr;
    }

    token = scanner->getLastToken();

    // Check if the token matches the 'end' keyword
    if (token == nullptr || !matchLexeme(token, kw_end))
    {
        reportError("Expect 'end' keyword");
        table->exit_scope(); // Exit the scope before returning
        return nullptr;
    }

    // Set lists to nullptr if they contain no elements
    if ((var_dec_list != nullptr) && (var_dec_list->head == nullptr) && (var_dec_list->tail == nullptr))
    {
        var_dec_list = nullptr;
    }

    if ((stmnt_list != nullptr) && (stmnt_list->head == nullptr) && (stmnt_list->tail == nullptr))
    {
        stmnt_list = nullptr;
    }

    // Create an AST node for the block
    AST *block_ast = make_ast_node(ast_block, var_dec_list, stmnt_list);

    // Exit the block scope
    table->exit_scope();

    return block_ast;
}

ste_list *Parser::var_decl_list(TOKEN *token, ste_list *STE_list_x)
{
    // If token is NULL, report an error
    if (token == nullptr)
    {
        reportError("Invalid syntax in variable declaration list.");
        return nullptr;
    }
    // If token matches 'kw_var', parse variable declaration
    else if (matchLexeme(token, kw_var))
    {
        // Parse var declaration
        AST *var_decl_ast = var_decl(token);
        if (var_decl_ast == nullptr)
        {
            return nullptr;
        }
        // Retrieve symbol table entry from the AST
        symbol_table_entry *ST_Entry = var_decl_ast->f.a_var_decl.name;
        // Add the symbol table entry to STE_list_x
        STE_list_x = addNodeToSteList(ST_Entry, STE_list_x);

        // Move to the next token (expecting ';')
        token = scanner->Scan();
        if (token == nullptr || !matchLexeme(token, lx_semicolon))
        {
            reportError("Expected a semicolon (;) after the variable declaration.");
            return nullptr;
        }
        token = scanner->Scan();

        // Recursively call varDeclList with the next token
        return var_decl_list(token, STE_list_x);
    }

    // If token is in the FOLLOW set of varDeclList, return currentSTEList
    else if (checkFollow_var_decl_list(token))
    {
        return STE_list_x;
    }

    // If token is not in FOLLOW set and not matching 'kw_var', report an error
    else if (!checkFollow_var_decl_list(token))
    {
        reportError("The current token is not expected in the context of a variable declaration list.");
        return nullptr;
    }
    return nullptr;
}

bool Parser::checkFollow_var_decl_list(TOKEN *token)
{
    if (token == nullptr)
    {
        return false;
    }
    // Get the type of the token
    LEXEME_TYPE type = token->type;
    // Check if the token type matches any of the expected follow tokens for a variable declaration list
    return ((type == kw_read) || (type == kw_write) || (type == kw_begin) || (type == kw_if) || (type == kw_for) || (type == kw_while) || (type == kw_return) || (type == kw_end) || (type == lx_identifier));
}

ast_list *Parser::stmt_list(TOKEN *token, ast_list *AST_list_x)
{
    // Check if the token is NULL, indicating an invalid statement list
    if (token == nullptr)
    {
        reportError("Invalid Statement List");
        return nullptr;
    }
    // Check if the token matches the 'end' keyword, indicating the end of the statement list
    else if (matchLexeme(token, kw_end))
    {
        return AST_list_x;
    }
    else
    {
        // Parse the current statement
        AST *ast_stmnt = stmt(token);
        if (ast_stmnt == nullptr)
        {
            return nullptr;
        }
        // Add the parsed statement to the AST list
        AST_list_x = addNodeToAstList(ast_stmnt, AST_list_x);

        // Update the token based on the type of the parsed statement
        if (ast_stmnt->type == ast_assign)
        {
            token = scanner->getLastToken();
        }
        else
        {
            token = scanner->Scan();
        }

        // Check if the token matches the ';' symbol
        if (token == nullptr || !matchLexeme(token, lx_semicolon))
        {
            reportError("Expects ';' in parse statement list");
            return nullptr;
        }
        token = scanner->Scan();

        // Recursively parse the next statement in the list
        return stmt_list(token, AST_list_x);
    }
    return nullptr;
}

ast_list *Parser::arg_list(TOKEN *token)
{
    // Check if the token is NULL or doesn't matchLexeme '('
    if (token == nullptr || !matchLexeme(token, lx_lparen))
    {
        reportError("Expect '(' ");
        return nullptr;
    }
    // Move to the next token
    token = scanner->Scan();

    // Call the function to parse the argument tail
    return arg_list_tail(token);
}

ast_list *Parser::arg_list_tail(TOKEN *token)
{
    // Check if the token matches ')'
    if (matchLexeme(token, lx_rparen))
    {
        // Move to the next token and return NULL indicating the end of arguments
        token = scanner->Scan();
        return nullptr;
    }
    else
    {
        // Create a new list to hold the arguments
        ast_list *args_list = new ast_list();
        args_list->head = nullptr;
        args_list->tail = nullptr;

        // Call the function to parse the actual arguments
        return args(token, args_list);
    }
}

ast_list *Parser::args(TOKEN *token, ast_list *AST_list_x)
{
    // Parse an expression as an argument
    AST *ast_Exp = parseE(token);
    if (ast_Exp == nullptr)
    {
        return nullptr;
    }

    // Add the parsed expression to the argument list
    AST_list_x = addNodeToAstList(ast_Exp, AST_list_x);

    // Get the last token from the scanner
    token = scanner->getLastToken();

    // Continue parsing the remaining arguments
    AST_list_x = args_tail(token, AST_list_x);

    return AST_list_x;
}

ast_list *Parser::args_tail(TOKEN *token, ast_list *AST_list_x)
{
    // Check for null token
    if (token == nullptr)
    {
        reportError("Null args_end token");
        return nullptr;
    }

    // If a comma is found, parse more arguments
    if (matchLexeme(token, lx_comma))
    {
        delete token;
        token = scanner->Scan();

        // Parse additional arguments and update the AST_list_x
        AST_list_x = args(token, AST_list_x);

        // Check if parsing arguments was successful
        if (AST_list_x == nullptr)
        {
            return nullptr;
        }

        // Get the last token from the scanner
        token = scanner->getLastToken();

        // Continue parsing remaining arguments
        AST_list_x = args_tail(token, AST_list_x);
        return AST_list_x;
    }
    // If a closing parenthesis is found, return the argument list
    if (matchLexeme(token, lx_rparen))
    {
        token = scanner->Scan();
        return AST_list_x;
    }

    // Return null for any other cases
    return nullptr;
}

AST *Parser::parseE(TOKEN *token)
{
    AST *E, *F;
    // Parse the term F
    F = parseF(token);
    E = F;
    token = scanner->getLastToken();
    // Parse the rest of the expression using extendedeExpression
    E = parseEb(E, token);
    return E;
}

AST *Parser::parseEb(AST *E, TOKEN *token)
{
    AST *Eb, *F;
    Eb = new AST();
    // Check if either E or token is NULL
    if (token == nullptr || E == nullptr)
    {
        return nullptr;
    }
    // Check if token matches any AND or OR operator
    if (rel_conj(token))
    {
        switch (token->type)
        {
        case kw_and:
            Eb->type = ast_and;
            Eb->f.a_binary_op.type = ast_and;
            break;
        case kw_or:
            Eb->type = ast_or;
            Eb->f.a_binary_op.type = ast_or;
            break;
        default:
            break;
        }
        token = scanner->Scan();
        // Parse the term F
        F = parseF(token);
        // Set left and right arguments for the binary operation
        Eb->f.a_binary_op.larg = E;
        Eb->f.a_binary_op.rarg = F;
        token = scanner->getLastToken();
        // Recursively parse the rest of the expression using extendedeExpression
        E = parseEb(Eb, token);
    }
    // Check if token is not in the FOLLOW set of extendedeExpression
    else if (checkFollowEb(token) == false)
    {
        return nullptr;
    }
    return E;
}

bool Parser::checkFollowEb(TOKEN *token)
{
    if (token == nullptr)
    {
        return false;
    }
    // Get the type of the token
    LEXEME_TYPE type = token->type;
    // Check if the token type matches any of the expected follow tokens for an Extended E (E tail) production
    return ((type == lx_rparen) || (type == kw_do) || (type == kw_then) || (type == kw_to) || (type == lx_semicolon) || (type == lx_comma));
}

AST *Parser::parseF(TOKEN *token)
{
    AST *F, *G;
    // Parse the factor G
    G = parseG(token);
    F = G;
    token = scanner->getLastToken();
    // Parse the rest of the factor using extendedfExpression
    F = parseFb(F, token);
    return F;
}

AST *Parser::parseFb(AST *F, TOKEN *token)
{
    AST *Fb, *G;
    Fb = new AST();
    // Check if token matches any Comparison Operations
    if (rel_op(token))
    {
        switch (token->type)
        {
        case lx_eq:
            Fb->type = ast_eq;
            Fb->f.a_binary_op.type = ast_eq;
            break;
        case lx_neq:
            Fb->type = ast_neq;
            Fb->f.a_binary_op.type = ast_neq;
            break;
        case lx_gt:
            Fb->type = ast_gt;
            Fb->f.a_binary_op.type = ast_gt;
            break;
        case lx_ge:
            Fb->type = ast_ge;
            Fb->f.a_binary_op.type = ast_ge;
            break;
        case lx_lt:
            Fb->type = ast_lt;
            Fb->f.a_binary_op.type = ast_lt;
            break;
        case lx_le:
            Fb->type = ast_le;
            Fb->f.a_binary_op.type = ast_le;
            break;
        default:
            break;
        }
        token = scanner->Scan();
        // Parse the factor G
        G = parseG(token);
        // Set left and right arguments for the binary operation
        Fb->f.a_binary_op.larg = F;
        Fb->f.a_binary_op.rarg = G;
        token = scanner->getLastToken();
        // Recursively parse the rest of the factor using extendedfExpression
        F = parseFb(Fb, token);
    }
    return F;
}

AST *Parser::parseG(TOKEN *token)
{
    AST *G, *H;
    // Parse the term H
    H = parseH(token);
    G = H;
    token = scanner->getLastToken();
    // Parse the rest of the term using extendedgExpression
    G = parseGb(G, token);
    return G;
}

AST *Parser::parseGb(AST *G, TOKEN *token)
{
    AST *Gb, *H;
    Gb = new AST();
    // Check if token matches any Arithmetic Operations Plus/Minus
    if (pm_op(token))
    {
        switch (token->type)
        {
        case lx_plus:
            Gb->type = ast_plus;
            Gb->f.a_binary_op.type = ast_plus;
            break;
        case lx_minus:
            Gb->type = ast_minus;
            Gb->f.a_binary_op.type = ast_minus;
            break;
        default:
            break;
        }
        token = scanner->Scan();

        // Parse the term H
        H = parseH(token);
        // Set left and right arguments for the binary operation
        Gb->f.a_binary_op.larg = G;
        Gb->f.a_binary_op.rarg = H;
        token = scanner->getLastToken();
        // Recursively parse the rest of the term using extendedgExpression
        G = parseGb(Gb, token);
    }
    return G;
}

AST *Parser::parseH(TOKEN *token)
{
    AST *H, *I;
    // Parse the factor I
    I = parseI(token);
    H = I;
    token = scanner->getLastToken();
    // Parse the rest of the factor using extendedhExpression
    H = parseHb(H, token);
    return H;
}

AST *Parser::parseHb(AST *H, TOKEN *token)
{
    AST *Hb, *I;
    Hb = new AST();
    // Check if token matches any Arithmetic Operations MULT/DIV
    if (md_op(token))
    {
        switch (token->type)
        {
        case lx_star:
            Hb->type = ast_times;
            Hb->f.a_binary_op.type = ast_times;
            break;
        case lx_slash:
            Hb->type = ast_divide;
            Hb->f.a_binary_op.type = ast_divide;
            break;
        default:
            break;
        }
        token = scanner->Scan();
        // Parse the factor I
        I = parseI(token);
        // Set left and right arguments for the binary operation
        Hb->f.a_binary_op.larg = H;
        Hb->f.a_binary_op.rarg = I;
        token = scanner->getLastToken();
        // Recursively parse the rest of the factor using extendedhExpression
        H = parseHb(Hb, token);
    }
    return H;
}

AST *Parser::parseI(TOKEN *token)
{
    AST *I;
    I = new AST();

    // Check if token matches any UNARY Operations
    if (unary_op(token))
    {
        switch (token->type)
        {
        case kw_not:
            I->type = ast_not;
            break;
        case lx_minus:
            I->type = ast_uminus;
            break;
        default:
            break;
        }
        token = scanner->Scan();
        // Parse the argument I of the unary operation
        I->f.a_unary_op.arg = parseI(token);
    }
    // Check if token matches Integer, Left parentheses , or Identifier
    else if (matchLexeme(token, lx_integer) || matchLexeme(token, lx_lparen) || matchLexeme(token, lx_identifier))
    {
        // Parse the expression J
        I = parseJ(token);
    }
    else
    {
        return nullptr; // Invalid token, return nullptr
    }
    return I;
}

AST *Parser::parseJ(TOKEN *token)
{
    AST *J;
    J = new AST();

    // Check if token matches '(' indicating the start of an expression
    if (matchLexeme(token, lx_lparen))
    {
        token = scanner->Scan();
        // Parse the expression E inside the parentheses
        J = parseE(token);
        token = scanner->getLastToken();
        // Check if the closing ')' token is present
        if (token->type != lx_rparen)
        {
            reportError("Expecting ')' ");
            return nullptr;
        }
        token = scanner->Scan();
        return J;
    }
    // Check if token matches an integer literal
    else if (matchLexeme(token, lx_integer))
    {
        J->type = ast_integer;
        J->f.a_integer.value = token->value;
        token = scanner->Scan();
        return J;
    }
    // Check if token matches a floating-point literal
    else if (matchLexeme(token, lx_float))
    {
        J->type = ast_float;
        J->f.a_float.value = token->float_value;
        token = scanner->Scan();
        return J;
    }
    // Check if token matches a string literal
    else if (matchLexeme(token, lx_string))
    {
        J->type = ast_string;
        J->f.a_string.string = token->str_ptr;
        token = scanner->Scan();
        return J;
    }
    // Check if token matches the 'true' keyword
    else if (matchLexeme(token, kw_true))
    {
        J->type = ast_boolean;
        J->f.a_boolean.value = 1;
        token = scanner->Scan();
        return J;
    }
    // Check if token matches the 'false' keyword
    else if (matchLexeme(token, kw_false))
    {
        J->type = ast_boolean;
        J->f.a_boolean.value = 0;
        token = scanner->Scan();
        return J;
    }
    // Check if token matches an identifier
    else if (matchLexeme(token, lx_identifier))
    {
        symbol_table_entry *ST_Entry = table->Get_symbol(token->str_ptr);
        if (ST_Entry == nullptr)
        {
            reportError("Undefined Variable");
            return nullptr;
        }
        if (ST_Entry->steType == STE_VAR)
        {
            J->type = ast_var;
            J->f.a_var.var = table->Get_symbol(token->str_ptr);
            token = scanner->Scan();
            return J;
        }
        else if (ST_Entry->steType == STE_ROUTINE)
        {
            // Parse the call statement and return the corresponding AST node
            J = call_stmt(token);
            token = scanner->Scan();
            return J;
        }
    }
    // None of the expected patterns matched, report an error
    {
        reportError(" Expecting ( or int ");
        return nullptr;
    }
}

bool Parser::md_op(TOKEN *token)
{
    LEXEME_TYPE type = token->type;
    return ((type == lx_star) || (type == lx_slash));
}

bool Parser::pm_op(TOKEN *token)
{
    LEXEME_TYPE type = token->type;
    return ((type == lx_plus) || (type == lx_minus));
}

bool Parser::rel_op(TOKEN *token)
{
    LEXEME_TYPE type = token->type;
    return ((type == lx_eq) || (type == lx_neq) || (type == lx_lt) || (type == lx_le) || (type == lx_gt) || (type == lx_ge));
}

bool Parser::rel_conj(TOKEN *token)
{
    LEXEME_TYPE type = token->type;
    return ((type == kw_and) || (type == kw_or));
}

bool Parser::unary_op(TOKEN *token)
{
    LEXEME_TYPE type = token->type;
    return ((type == lx_minus) || (type == kw_not));
}

symbol_table_entry *Parser::setSTEValues(STE_ENTRY_TYPE type, ...)
{
    // Start a variable argument list
    va_list ap;
    va_start(ap, type);

    // Get the symbol table entry pointer from the variable arguments
    symbol_table_entry *ST_Entry = va_arg(ap, symbol_table_entry *);
    ST_Entry->steType = type;

    // Based on the entry type, set the values accordingly
    switch (type)
    {
    case STE_CONST:
        ST_Entry->f.constant.line = va_arg(ap, int);
        ST_Entry->f.constant.value = va_arg(ap, int);
        break;
    case STE_VAR:
        ST_Entry->f.var.line = va_arg(ap, int);
        ST_Entry->f.var.type = (STE_TYPE)(va_arg(ap, int));
        break;
    case STE_ROUTINE:
        ST_Entry->f.routine.line = va_arg(ap, int);
        ST_Entry->f.routine.result_type = (STE_TYPE)(va_arg(ap, int));
        break;
    default:
        break;
    }

    // End the variable argument list
    va_end(ap);

    return ST_Entry;
}

ste_list *Parser::addNodeToSteList(symbol_table_entry *ST_Entry, ste_list *STE_list_x)
{
    if (STE_list_x == nullptr || ST_Entry == nullptr)
    {
        return STE_list_x;
    }
    ste_list *p = STE_list_x;

    // Traverse to the end of the STE list
    while (p->head != nullptr && p->tail != nullptr)
    {
        p = p->tail;
    }
    // If the list is empty, add the ST_Entry as the head
    if (p->head == nullptr)
    {
        p->head = ST_Entry;
    }
    else
    { // Create a new node and add the ST_Entry as the head of the new node
        p->tail = new ste_list();
        p->tail->head = ST_Entry;
        p->tail->tail = nullptr;
    }
    return STE_list_x;
}

ast_list *Parser::addNodeToAstList(AST *ast, ast_list *AST_list_x)
{
    if (AST_list_x == nullptr || ast == nullptr)
    {
        return AST_list_x;
    }

    ast_list *p = AST_list_x;

    // Traverse to the end of the AST list
    while (p->head != nullptr && p->tail != nullptr)
    {
        p = p->tail;
    }

    // If the list is empty, add the AST node as the head
    if (p->head == nullptr)
    {
        p->head = ast;
    }
    else
    {
        // Create a new node and add the AST node as the head of the new node
        p->tail = new ast_list();
        p->tail->head = ast;
        p->tail->tail = nullptr;
    }
    return AST_list_x;
}

bool Parser::matchLexeme(TOKEN *token, LEXEME_TYPE type)
{
    if (token == nullptr)
    {
        token = new TOKEN();
    }
    return (token->type == type);
}

void Parser::reportError(string message)
{
    scanner->Get_fd()->ReportError(message);
}

void Parser::print_AST(AST *ast, FILE *file)
{
    if (ast == nullptr)
    {
        return;
    }
    print_ast_node(file, ast);
}
