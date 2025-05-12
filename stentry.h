#ifndef STENTRY_H
#define STENTRY_H

#include <stdio.h>
#include <string.h>

#define TYPE_SIZE 7

typedef enum
{
    STE_NONE, // 0
    STE_INT,
    STE_STRING,
    STE_FLOAT,
    STE_DOUBLE,
    STE_CHAR,
    STE_BOOL,
} STE_TYPE;

typedef enum
{
    STE_VAR,       // a variable
    STE_CONST,     // A constant
    STE_ROUTINE,   // A routine
    STE_UNDEFINED, // ste_entry
} STE_ENTRY_TYPE;

static char *STE_TYPE_STR[TYPE_SIZE] = {"None", "integer", "string", "float", "double", "char", "boolean"}; // It is not a good idea to put this here
static char str[128];

class STEntry
{
public:
    char Name[64];
    STE_TYPE Type;
    STEntry *Next;
    STE_ENTRY_TYPE steType;
    int offset;

    union
    {
        // for a variable record its type
        struct
        {
            STE_TYPE type;
            int line;
        } var;
        // for a constant record its value
        struct
        {
            int value;
            int line;
        } constant;
        /* for a routine, record formal parameters and result type */
        struct
        {
            // SteListCelll	 *formals;// will be defined later
            STE_TYPE result_type;
            int line;
        } routine;
    } f;

    STEntry()
    {
        Next = NULL;
        Type = STE_NONE;
        Name[0] = 0; // empty String
    }

    STEntry(char *name, STE_TYPE type)
    {
        Next = NULL;
        Type = type;
        strcpy(Name, name);
    }

    char *toString()
    {
        if ((Type < STE_NONE) || Type >= TYPE_SIZE)
            Type = STE_NONE;
        sprintf(str, "(%s,%s)", Name, STE_TYPE_STR[Type]);
        return str;
    }

    void print(FILE *fp)
    {
        fprintf(fp, "%s ", toString());
    }

    static STE_TYPE getType(char *str)
    {
        int i;
        for (i = 0; i < TYPE_SIZE; i++)
        {
            if (strcmp(STE_TYPE_STR[i], str) == 0)
                return ((STE_TYPE)i);
        }
        return STE_NONE;
    }
};

#endif // STENTRY_H
