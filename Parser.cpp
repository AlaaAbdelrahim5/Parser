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
        reportError("Token is null, unexpected error encountered");
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
        reportError("Only variables can be assigned values");
        return nullptr;
    }

    token = scanner->Scan();
    if (token == nullptr || !matchLexeme(token, Ix_colon_eq))
    {
        reportError("Expected ':=' in assignment statement");
        return nullptr;
    }

    // Parse the expression on the right-hand side of the assignment
    token = scanner->Scan();
    AST *ast_Exp = parseE(token);
    if (ast_Exp == nullptr)
    {
        reportError("Failed to parse the right-hand side expression in the assignment statement");
        return nullptr;
    }

    // Create an AST node for the assignment statement
    AST *assign_ast = make_ast_node(ast_assign, ST_Entry, ast_Exp);
    return assign_ast;
}

AST *Parser::call_stmt(TOKEN *token)
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

    // Check if the symbol table entry represents a routine (function or procedure)
    if (ST_Entry->steType != STE_ROUTINE)
    {
        reportError("You can't call anything other than routines ");
        return nullptr;
    }
    token = scanner->Scan();
    ast_list *AST_list_x = arg_list(token);

    // Create an AST node for the function call statement
    AST *call_ast = make_ast_node(ast_call, ST_Entry, AST_list_x);
    return call_ast;
}

AST *Parser::if_stmt(TOKEN *token)
{
    token = scanner->Scan();
    AST *ast_Exp = parseE(token);
    if (ast_Exp == nullptr)
    {
        reportError("Failed to parse the expression");
        return nullptr;
    }

    token = scanner->getLastToken();
    if (!matchLexeme(token, kw_then))
    {
        reportError("Missing 'then' keyword in if statement");
        return nullptr;
    }

    token = scanner->Scan();
    AST *ast_conseq = stmt(token);
    if (ast_conseq == nullptr)
    {
        reportError("Failed to parse the consequent block in the if statement");
        return nullptr;
    }

    // Call the function to parse the optional 'else' part of the 'if' statement
    token = scanner->Scan();
    return if_tail(token, ast_Exp, ast_conseq);
}

AST *Parser::if_tail(TOKEN *token, AST *ast_Exp, AST *ast_conseq)
{
    AST *if_ast, *ast_alter;

    if (matchLexeme(token, kw_fi))
    {
        ast_alter = nullptr;
    }
    else if (matchLexeme(token, kw_else))
    {
        token = scanner->Scan();
        ast_alter = stmt(token);
        if (ast_alter == nullptr)
        {
            reportError("Failed to parse the alternative block in the if statement");
            return nullptr;
        }

        token = scanner->Scan();
        if (!matchLexeme(token, kw_fi))
        {
            reportError("Expecting 'fi' keyword to close the if statement");
            return nullptr;
        }
    }
    else
    {
        reportError("Expecting 'fi' keyword to close the if statement");
        return nullptr;
    }

    // Create an AST node for the if statement, including its condition, consequent block, and alternative block
    if_ast = make_ast_node(ast_if, ast_Exp, ast_conseq, ast_alter);
    return if_ast;
}

AST *Parser::while_stmt(TOKEN *token)
{
    if (token == nullptr || !matchLexeme(token, kw_while))
    {
        reportError("Expected 'while' keyword");
        return nullptr;
    }

    token = scanner->Scan();
    AST *ast_Exp = parseE(token);
    if (ast_Exp == nullptr)
    {
        reportError("Failed to parse the expression");
        return nullptr;
    }

    token = scanner->getLastToken();
    if (token == nullptr || !matchLexeme(token, kw_do))
    {
        reportError("Expected 'do' keyword in while statement");
        return nullptr;
    }

    token = scanner->Scan();
    AST *ast_stmnt = stmt(token);
    if (ast_stmnt == nullptr)
    {
        reportError("Failed to parse the statement inside the while loop");
        return nullptr;
    }

    token = scanner->Scan();
    if (token == nullptr || !matchLexeme(token, kw_od))
    {
        reportError("Expected 'od' keyword in while statement");
        return nullptr;
    }

    // Create an AST node for the while loop
    AST *while_ast = make_ast_node(ast_while, ast_Exp, ast_stmnt);
    return while_ast;
}

AST *Parser::for_stmt(TOKEN *token)
{
    if (!matchLexeme(token, kw_for))
    {
        reportError("Invalid syntax in 'for' loop statement");
        return nullptr;
    }

    token = scanner->Scan();
    if (token == nullptr || !matchLexeme(token, lx_identifier))
    {
        reportError("Expected an identifier");
        return nullptr;
    }

    AST *ast_assign = assign_stmt(token);
    if (ast_assign == nullptr)
    {
        reportError("Failed to parse the assignment statement in the 'for' loop");
        return nullptr;
    }

    token = scanner->getLastToken();
    if (token == nullptr || !matchLexeme(token, kw_to))
    {
        reportError("Expected 'to' keyword in 'for' loop");
        return nullptr;
    }

    token = scanner->Scan();
    AST *ast_Exp = parseE(token);
    if (ast_Exp == nullptr)
    {
        reportError("Failed to parse the expression in the 'for' loop");
        return nullptr;
    }

    token = scanner->getLastToken();
    if (token == nullptr || !matchLexeme(token, kw_do))
    {
        reportError("Expected 'do' keyword in 'for' loop");
        return nullptr;
    }

    token = scanner->Scan();
    AST *ast_stmnt = stmt(token);
    if (ast_stmnt == nullptr)
    {
        reportError("Failed to parse the statement inside the 'for' loop");
        return nullptr;
    }

    token = scanner->Scan();
    if (token == nullptr || !matchLexeme(token, kw_od))
    {
        reportError("Expected 'od' keyword in 'for' loop");
        return nullptr;
    }

    // Create an AST node for the for loop
    AST *for_ast = make_ast_node(ast_for, ast_assign->f.a_var.var, ast_assign, ast_Exp, ast_stmnt);
    return for_ast;
}

AST *Parser::read_stmt(TOKEN *token)
{
    if (!matchLexeme(token, kw_read))
    {
        reportError("Expected 'read' keyword");
        return nullptr;
    }

    token = scanner->Scan();
    if (!matchLexeme(token, lx_lparen))
    {
        reportError("Expected '(' after 'read' keyword");
        return nullptr;
    }

    token = scanner->Scan();
    if (!matchLexeme(token, lx_identifier))
    {
        reportError("Expected an identifier after '(' in the 'read' statement");
        return nullptr;
    }

    // Look up the identifier in the symbol table
    char *name = token->str_ptr;
    symbol_table_entry *ST_Entry = table->Get_symbol(name);
    if (ST_Entry == nullptr)
    {
        reportError("Undefined Variable: " + string(name));
        return nullptr;
    }

    token = scanner->Scan();
    if (!matchLexeme(token, lx_rparen))
    {
        reportError("Expected ')' in the 'read' statement");
        return nullptr;
    }

    // Create an AST node for the Read statement
    AST *ast = make_ast_node(ast_read, ST_Entry);
    return ast;
}

AST *Parser::write_stmt(TOKEN *token)
{
    if (!matchLexeme(token, kw_write))
    {
        reportError("Expected 'write' keyword");
        return nullptr;
    }

    token = scanner->Scan();
    if (!matchLexeme(token, lx_lparen))
    {
        reportError("Expected '(' after 'write' keyword");
        return nullptr;
    }

    token = scanner->Scan();
    if (!matchLexeme(token, lx_identifier))
    {
        reportError("Expected an identifier after '(' in the 'write' statement");
        return nullptr;
    }

    // Look up the identifier in the symbol table
    char *name = token->str_ptr;
    symbol_table_entry *ST_Entry = table->Get_symbol(name);
    if (ST_Entry == nullptr)
    {
        reportError("Undefined Variable: " + string(name));
        return nullptr;
    }

    token = scanner->Scan();
    if (!matchLexeme(token, lx_rparen))
    {
        reportError("Expected ')' in the 'write' statement");
        return nullptr;
    }

    // Create an AST node for the Write statement
    AST *ast = make_ast_node(ast_write, ST_Entry);
    return ast;
}

AST *Parser::return_stmt(TOKEN *token)
{
    if (!matchLexeme(token, kw_return))
    {
        reportError("Expected 'return' keyword");
        return nullptr;
    }

    token = scanner->Scan();
    if (!matchLexeme(token, lx_lparen))
    {
        reportError("Expected '(' after 'return' keyword");
        return nullptr;
    }

    token = scanner->Scan();
    AST *ast_Exp = parseE(token);
    if (ast_Exp == nullptr)
    {
        reportError("Failed to parse the expression in the 'return' statement");
        return nullptr;
    }

    token = scanner->getLastToken();
    if (!matchLexeme(token, lx_rparen))
    {
        reportError("Expected ')' in the 'return' statement");
        return nullptr;
    }

    // Create an AST node for the return statement
    AST *ast = make_ast_node(ast_return, ast_Exp);
    return ast;
}

AST *Parser::block_stmt(TOKEN *token)
{
    if (token == nullptr || !matchLexeme(token, kw_begin))
    {
        reportError("Expected 'begin' keyword");
        return nullptr;
    }

    // Initialize offset for variable declaration offsets
    offset = 0;

    // Enter a new scope for this block
    table->enter_scope();

    // Initialize a linked list for variable declarations
    token = scanner->Scan();
    ste_list *var_dec_list;
    var_dec_list = new ste_list();
    var_dec_list->head = nullptr;
    var_dec_list->tail = nullptr;
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

    if (var_dec_list == nullptr)
    {
        reportError("Variable declaration list is null");
        table->exit_scope();
        return nullptr;
    }

    // Initialize a linked list for statement parsing
    token = scanner->getLastToken();
    ast_list *stmnt_list;
    stmnt_list = new ast_list();
    stmnt_list->head = nullptr;
    stmnt_list->tail = nullptr;
    stmnt_list = stmt_list(token, stmnt_list);
    if (stmnt_list == nullptr)
    {
        reportError("Statement list is null");
        table->exit_scope();
        return nullptr;
    }

    token = scanner->getLastToken();
    if (token == nullptr || !matchLexeme(token, kw_end))
    {
        reportError("Expected 'end' keyword");
        table->exit_scope();
        return nullptr;
    }

    // Check if the variable declaration list and statement list are empty
    // If they are empty, set them to nullptr
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
    table->exit_scope();
    return block_ast;
}

ste_list *Parser::var_decl_list(TOKEN *token, ste_list *STE_list_x)
{
    if (token == nullptr)
    {
        reportError("Unexpected token in variable declaration list.");
        return nullptr;
    }
    else if (matchLexeme(token, kw_var))
    {
        AST *var_decl_ast = var_decl(token);
        if (var_decl_ast == nullptr)
        {
            reportError("Failed to parse variable declaration");
            return nullptr;
        }

        // Add the variable declaration AST node to the list
        symbol_table_entry *ST_Entry = var_decl_ast->f.a_var_decl.name;
        STE_list_x = addNodeToSteList(ST_Entry, STE_list_x);

        token = scanner->Scan();
        if (token == nullptr || !matchLexeme(token, lx_semicolon))
        {
            reportError("A semicolon (;) is required after the variable declaration");
            return nullptr;
        }

        // Recursively call varDeclList with the next token
        token = scanner->Scan();
        return var_decl_list(token, STE_list_x);
    }
    else if (checkFollow_var_decl_list(token))
    {
        return STE_list_x;
    }
    else if (!checkFollow_var_decl_list(token))
    {
        reportError("Unexpected token encountered in the variable declaration list.");
        return nullptr;
    }

    return nullptr;
}

bool Parser::checkFollow_var_decl_list(TOKEN *token)
{
    if (token == nullptr)
    {
        reportError("Token is null, unexpected error encountered");
        return false;
    }

    switch (token->type)
    {
    case lx_identifier:
    case kw_if:
    case kw_while:
    case kw_for:
    case kw_read:
    case kw_write:
    case kw_return:
    case kw_begin:
    case kw_end:
        return true;
    default:
        return false;
    }
}

ast_list *Parser::stmt_list(TOKEN *token, ast_list *AST_list_x)
{
    if (token == nullptr)
    {
        reportError("Invalid statement list encountered");
        return nullptr;
    }
    else if (matchLexeme(token, kw_end))
    {
        return AST_list_x;
    }
    else
    {
        AST *ast_stmnt = stmt(token);
        if (ast_stmnt == nullptr)
        {
            reportError("Failed to parse the statement in the statement list");
            return nullptr;
        }

        // Add the parsed statement to the AST list
        AST_list_x = addNodeToAstList(ast_stmnt, AST_list_x);

        // Update the token based on the type of the parsed statement
        if (ast_stmnt->type == ast_assign || ast_stmnt->type == ast_call)
        {
            token = scanner->getLastToken();
        }
        else
        {
            token = scanner->Scan();
        }

        if (token == nullptr || !matchLexeme(token, lx_semicolon))
        {
            reportError("Expected ';' in the statement list");
            return nullptr;
        }

        // Recursively parse the next statement in the list
        token = scanner->Scan();
        return stmt_list(token, AST_list_x);
    }
}

ast_list *Parser::arg_list(TOKEN *token)
{
    if (token == nullptr || !matchLexeme(token, lx_lparen))
    {
        reportError("Expected '(' in the argument list");
        return nullptr;
    }

    // Call the function to parse the argument tail
    token = scanner->Scan();
    return arg_list_tail(token);
}

ast_list *Parser::arg_list_tail(TOKEN *token)
{
    if (matchLexeme(token, lx_rparen))
    {
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
    AST *ast_Exp = parseE(token);
    if (ast_Exp == nullptr)
    {
        reportError("Failed to parse the expression in the statement.");
        return nullptr;
    }

    // Add the parsed expression to the argument list
    AST_list_x = addNodeToAstList(ast_Exp, AST_list_x);

    // Continue parsing the remaining arguments
    token = scanner->getLastToken();
    AST_list_x = args_tail(token, AST_list_x);

    return AST_list_x;
}

ast_list *Parser::args_tail(TOKEN *token, ast_list *AST_list_x)
{
    if (token == nullptr)
    {
        reportError("Unexpected null token encountered in args_tail");
        return nullptr;
    }

    if (matchLexeme(token, lx_comma))
    {
        token = scanner->Scan();
        AST_list_x = args(token, AST_list_x);

        if (AST_list_x == nullptr)
        {
            reportError("Failed to parse the argument list");
            return nullptr;
        }

        // Continue parsing remaining arguments
        token = scanner->getLastToken();
        return args_tail(token, AST_list_x);
    }
    else if (matchLexeme(token, lx_rparen))
    {
        token = scanner->Scan();
        return AST_list_x;
    }
    else
    {
        // This happens after we've finished parsing the arguments
        // It's not necessarily an error - we might just be looking at the next token
        return AST_list_x;
    }
}

AST *Parser::parseE(TOKEN *token)
{
    AST *E, *F;
    // Parse the factor F
    F = parseF(token);
    E = F;
    // Parse the rest of the expression using parseEb
    token = scanner->getLastToken();
    E = parseEb(E, token);
    return E;
}

AST *Parser::parseEb(AST *E, TOKEN *token)
{
    AST *Eb, *F;
    Eb = new AST();

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

        // Parse the factor F
        token = scanner->Scan();
        F = parseF(token);
        // Set left and right arguments for the operation
        Eb->f.a_binary_op.larg = E;
        Eb->f.a_binary_op.rarg = F;
        // Recursively parse the rest of the expression using parseEb
        token = scanner->getLastToken();
        E = parseEb(Eb, token);
    }
    else if (checkFollowExpr(token) == false)
    {
        reportError("Unexpected FOLLOW token encountered");
        return nullptr;
    }

    return E;
}

AST *Parser::parseF(TOKEN *token)
{
    AST *F, *G;
    // Parse the factor G
    G = parseG(token);
    F = G;
    // Parse the rest of the expression using parseFb
    token = scanner->getLastToken();
    F = parseFb(F, token);
    return F;
}

AST *Parser::parseFb(AST *F, TOKEN *token)
{
    AST *Fb, *G;
    Fb = new AST();

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

        // Parse the factor G
        token = scanner->Scan();
        G = parseG(token);
        // Set left and right arguments for the operation
        Fb->f.a_binary_op.larg = F;
        Fb->f.a_binary_op.rarg = G;
        // Recursively parse the rest of the factor using parseFb
        token = scanner->getLastToken();
        F = parseFb(Fb, token);
    }

    return F;
}

AST *Parser::parseG(TOKEN *token)
{
    AST *G, *H;
    // Parse the factor H
    H = parseH(token);
    G = H;
    // Parse the rest of the expression using parseGb
    token = scanner->getLastToken();
    G = parseGb(G, token);
    return G;
}

AST *Parser::parseGb(AST *G, TOKEN *token)
{
    AST *Gb, *H;
    Gb = new AST();

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

        // Parse the factor H
        H = parseH(token);
        // Set left and right arguments for the operation
        Gb->f.a_binary_op.larg = G;
        Gb->f.a_binary_op.rarg = H;
        // Recursively parse the rest of the term using parseGb
        token = scanner->getLastToken();
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
    // Parse the rest of the expression using parseHb
    token = scanner->getLastToken();
    H = parseHb(H, token);
    return H;
}

AST *Parser::parseHb(AST *H, TOKEN *token)
{
    AST *Hb, *I;
    Hb = new AST();

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

        // Parse the factor I
        token = scanner->Scan();
        I = parseI(token);
        // Set left and right arguments for the operation
        Hb->f.a_binary_op.larg = H;
        Hb->f.a_binary_op.rarg = I;
        // Recursively parse the rest of the factor using parseHb
        token = scanner->getLastToken();
        H = parseHb(Hb, token);
    }

    return H;
}

AST *Parser::parseI(TOKEN *token)
{
    AST *I;
    I = new AST();

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

        // Parse the argument I of the unary operation
        token = scanner->Scan();
        I->f.a_unary_op.arg = parseI(token);
    }
    else if (matchLexeme(token, lx_integer) || matchLexeme(token, lx_lparen) || matchLexeme(token, lx_identifier) || matchLexeme(token, kw_true) || matchLexeme(token, kw_false) || matchLexeme(token, lx_float) || matchLexeme(token, lx_string))
    {
        // Parse the factor J
        I = parseJ(token);
    }
    else if (matchLexeme(token, lx_identifier))
    {
        // Parse the factor J
        I = parseJ(token);
    }
    else
    {
        reportError("Expecting an identifier, integer, or '(' in the expression");
        return nullptr;
    }

    return I;
}

AST *Parser::parseJ(TOKEN *token)
{
    AST *J;
    J = new AST();
    
    if (matchLexeme(token, lx_identifier))
    {
        symbol_table_entry *ST_Entry = table->Get_symbol(token->str_ptr);
        if (ST_Entry == nullptr)
        {
            reportError("Undefined Variable: " + string(token->str_ptr));
            return nullptr;
        }

        if (ST_Entry->steType == STE_VAR)
        {
            J->type = ast_var;
            J->f.a_var.var = table->Get_symbol(token->str_ptr);
            token = scanner->Scan();
            return J;
        }
        else if (ST_Entry->steType == STE_CONST)
        {
            // Create a constant AST node using the value from the symbol table
            J->type = ast_integer; // Constants are stored as integers in the symbol table
            J->f.a_integer.value = ST_Entry->f.constant.value;
            token = scanner->Scan();
            return J;
        }
        else if (ST_Entry->steType == STE_ROUTINE)
        {
            // Parse the call statement and return the corresponding AST node
            J = call_stmt(token);
            return J;
        }
    }
    else if (matchLexeme(token, lx_integer))
    {
        J->type = ast_integer;
        J->f.a_integer.value = token->value;
        token = scanner->Scan();
        return J;
    }
    else if (matchLexeme(token, lx_float))
    {
        J->type = ast_float;
        J->f.a_float.value = token->float_value;
        token = scanner->Scan();
        return J;
    }
    else if (matchLexeme(token, lx_string))
    {
        J->type = ast_string;
        J->f.a_string.string = token->str_ptr;
        token = scanner->Scan();
        return J;
    }
    else if (matchLexeme(token, kw_true))
    {
        J->type = ast_boolean;
        J->f.a_boolean.value = 1;
        token = scanner->Scan();
        return J;
    }
    else if (matchLexeme(token, kw_false))
    {
        J->type = ast_boolean;
        J->f.a_boolean.value = 0;
        token = scanner->Scan();
        return J;
    }
    else if (matchLexeme(token, lx_lparen))
    {
        // Parse the expression E inside the parentheses
        token = scanner->Scan();
        J = parseE(token);

        token = scanner->getLastToken();
        if (token->type != lx_rparen)
        {
            reportError("Expected ')' in the expression");
            return nullptr;
        }

        token = scanner->Scan();
        return J;
    }
    else
    {
        reportError("Expecting an identifier, integer, or '(' in the expression");
        return nullptr;
    }
}

bool Parser::checkFollowExpr(TOKEN *token)
{
    if (token == nullptr)
    {
        reportError("Token is null, unexpected error encountered");
        return false;
    }

    switch (token->type)
    {
    case lx_rparen:
    case kw_do:
    case kw_then:
    case kw_to:
    case lx_semicolon:
    case lx_comma:
        return true;
    default:
        return false;
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
