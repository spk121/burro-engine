#pragma once

#include <map>
#include <string>
#include <cdio++/iso9660.hpp>
#include <algorithm>

using namespace std;

class ResourceFile
{
private:
    ISO9660::IFS iso_file;
    map<unsigned int, string> xiso_file_hash;

    static unsigned int Quick_hash (const string& str);
    static bool Check_file_exists (const string& name);
    static int Round_up (int x, int base);
    static bool String_match_suffix(const string& s, const string& suffix);

    void Initialize_hash (void);
    void Build_hash (const string& path);

public:
    void Open (const string& iso_name);
    void Close (void);
    vector<char> Get_data(const string& resource_name);
    vector<string> Get_resource_names_matching_suffix(const string& suffix);

    ResourceFile ();
    ~ResourceFile ();
};

extern ResourceFile resource_file;
