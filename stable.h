#ifndef STABLE_H
#define STABLE_H

#include "stlist.h"
#include <iostream>
#include <cctype> // tolower()
#define DEFAULT_SIZE 19
using namespace std;

class STable
{
private:
    unsigned long Size;
    STList *Table; // Dynamic Array of size = Size
    void init(int size);
    unsigned long ElfHash(char *str);
    int case_flag;        // Upper to lower case
    STEntry **entryLists; // Pointer to hash table array of entry lists
                          // linkedlist of symbol tables to handle scoping
    STable *next;         // To be used to create a stack of symbol table
    STable *head;

public:
    STable();
    ~STable();
    STable(unsigned long size);
    void Reset(unsigned long size);

    bool AddEntry(char *name, STE_TYPE type);
    void FindAndPrintEntry(char *name, FILE *fp); // finds and prints the Entry if it exist
    void PrintAll(FILE *fp);
    void Clear();

    STable(int fold_case_flag);
    STEntry *Get_symbol(char *str);
    STEntry *Put_symbol(char *str);
    STEntry *Get_entry(char *key);
    void ClearSymbolTable();
    void PrintSymbolStats();
    void enter_scope(); // Inform the symbol table that the parser is entering a new scope
    void exit_scope();  // Inform the symbol table that the parser is leaving a scope.
};

#endif // STABLE_H
