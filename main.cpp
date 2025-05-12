#include <iostream>
#include "Parser.h"
using namespace std;

int main()
{
        FileDescriptor *fd = new FileDescriptor("c:\\Users\\Ala'a\\Downloads\\Parser\\Input.txt");
        Parser *parser = new Parser(fd);
        ast_list *program = parser->parse();
        return 0;
}
