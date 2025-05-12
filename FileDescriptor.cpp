#include "FileDescriptor.h"

FileDescriptor::FileDescriptor()
{
    file = NULL;
    fileName = "";
    line_number = 0;
    char_number = 0;
}

FileDescriptor::FileDescriptor(string fileName)
{
    this->fileName = fileName;
    file = fopen(fileName.c_str(), "r");
    line_number = 0;
    char_number = 0;
    buffer = NULL;
}

FileDescriptor::FileDescriptor(char *fileName)
{
    fp.open(fileName, ios::in);
    line_number = 1;
    char_number = 0;
    buffer = new char[BUFFER_SIZE];

    fp.getline(buffer, BUFFER_SIZE);
    lineSize = strlen(buffer);
}

FileDescriptor::~FileDescriptor()
{
    fp.close();
    delete[] buffer;
}

std::string FileDescriptor::GetFileName()
{
    return fileName;
}

bool FileDescriptor::IsOpen()
{
    if (file == NULL)
        return false;

    else
        return true;
}

char *FileDescriptor::GetCurrLine()
{
    return buffer;
}

int FileDescriptor::GetLineNum()
{
    return line_number;
}

int FileDescriptor::GetCharNum()
{
    return char_number;
}

void FileDescriptor::Close()
{
    if (file != nullptr)
    {
        fclose(file);
        file = nullptr;
    }
}

char FileDescriptor::GetChar()
{
    char c = buffer[char_number];

    // If we reached the end of the line, we need to read the next line from the file
    if (lineSize == char_number)
    {
        if (isEOF())
            return EOF;
        else
        {
            fp.getline(buffer, BUFFER_SIZE);

            lineSize = strlen(buffer);
            char_number = 0;

            line_number++;

            return '\n';
        }
    }

    char_number++;

    return c;
}

void FileDescriptor::ReportError(string message)
{
    if (count == 0)
    {
        cout << "Error: " << message << " at line " << line_number << "." << endl;
        cout << "Current file position: line " << line_number << ", char " << char_number << endl;
        count++;
    }
    else
        count = 0;

    return;
}

void FileDescriptor::UngetChar(char c)
{
    char_number--;
}

bool FileDescriptor::isEOF()
{
    return fp.eof();
}
