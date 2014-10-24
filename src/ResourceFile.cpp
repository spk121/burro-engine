#include <iostream>
#include <fstream>
#include "xsdl.hpp"
#include "ResourceFile.hpp"

ResourceFile resource_file {};


unsigned int ResourceFile::Quick_hash (const string& str)
{
    unsigned int val = 0;
    for (size_t i = 0; i < str.size(); i ++) {
        char c = str[i];
        if (c == '\0')
            break;
        val = val * 101 + (unsigned char) c;
        val = val & 0xFFFF;
    }
    SDL_LogDebug(SDL_LOG_CATEGORY_APPLICATION, "Resource File Quick Hash: \"%s\" = %x", str.c_str(), val);
    return val;
}

bool ResourceFile::Check_file_exists (const string& name)
{
    ifstream f {name, fstream::in};
    auto ret = f.good();
    f.close();
    return ret;
    // return ( access( name.c_str(), F_OK ) != -1 );
}

int ResourceFile::Round_up (int x, int base)
{
    if(base == 0)
        return x;
    
    int remainder = x % base;
    if (remainder == 0)
        return x;
    return x + base - remainder;
}

bool ResourceFile::String_match_suffix(const string& s, const string& suffix)
{
    auto i = s.rfind(suffix);
    if (i == string::npos)
        return false;
    return (i == s.length() - suffix.length());
}

void ResourceFile::Initialize_hash (void)
{
    xiso_file_hash.empty();
    Build_hash ("");
}

// Builds a filename-to-ID hash map for a given ISO file.  This
// function is recursive, and the top-level call should have a
// path == "".
void ResourceFile::Build_hash (const string& path)
{
    uint8_t i_joliet_level = iso_file.get_joliet_level();
    stat_vector_t entlist;

    if (!iso_file.readdir(path.c_str(), entlist)) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                     "Error getting above directory information: %s\n", path.c_str());
        return;
    }

    for (auto i: entlist) {
        iso9660_stat_t* p_statbuf = i->p_stat;
        string full_name = p_statbuf->filename;

        if (p_statbuf->rr.b3_rock != yep) {
            // Overwrite the name with a translated name.  Note that
            // we're using the assumption that the translated name
            // will be shorter, and we're overwriting it in memory.
            iso9660_name_translate_ext(full_name.c_str(),
                                       (char *) full_name.c_str(),
                                       i_joliet_level);
        }
       full_name = full_name.insert(0, path);

        // Recursively descend into subdirectories
        if (p_statbuf->type == 2 /* _STAT_DIR */
            && strcmp (p_statbuf->filename, ".")
            && strcmp (p_statbuf->filename, "..")) {
            full_name.append("/");
            Build_hash(full_name);
        }

        // For regular files, add them to the hash table
        if (p_statbuf->type == 1) {
            // There is an initial slash in this filename that needs to go
            // full_name.erase(0,1);
            printf("%s\n", full_name.c_str());
            xiso_file_hash[Quick_hash(full_name)] = full_name;
            SDL_LogDebug(SDL_LOG_CATEGORY_APPLICATION,
                         "Resource Hash: adding \"%s\" as %x",
                         full_name.c_str(),
                         Quick_hash(full_name));
        }
    }
}

void ResourceFile::Open (const string& iso_name)
{
    if (!Check_file_exists (iso_name)) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                     "Can't access ISO file '%s'", iso_name.c_str());
        exit (1);
    }

    bool ret = iso_file.open(iso_name.c_str());
    if (ret == false) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                     "Can't open ISO file '%s'", iso_name.c_str());
        exit (1);
    }

    Initialize_hash ();
}

void ResourceFile::Close (void)
{
    iso_file.close();
}

vector<char> ResourceFile::Get_data(const string& resource_name)
{
    ISO9660::Stat * s = iso_file.stat(resource_name.c_str());
    if (s->p_stat == NULL) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                     "Can't open ISO file member '%s'", resource_name.c_str());
        exit (1);
    }

    size_t true_length = s->p_stat->size;
    size_t block_length = Round_up(s->p_stat->size, ISO_BLOCKSIZE);

    vector<char> buffer (block_length);

    // Read the data from the buffer in 2K blocks
    for (size_t i = 0; i < block_length; i += ISO_BLOCKSIZE) {
        iso_file.seek_read(buffer.data() + i,
                           s->p_stat->lsn + (i / ISO_BLOCKSIZE));
        // FIXME: maybe warn if bytes read != ISO_BLOCKSIZE
    }

    // The last block may need to be trimmed to the true file length.
    buffer.erase (buffer.begin() + true_length, buffer.end());
    return buffer;
}

ResourceFile::ResourceFile () : iso_file {}, xiso_file_hash {}
    {
    }

ResourceFile::~ResourceFile ()
{
    
}

vector<string> ResourceFile::Get_resource_names_matching_suffix(const string& suffix)
{
    vector<string> output {};

    for(auto& e: xiso_file_hash)
    {
        if (String_match_suffix (e.second, suffix))
            output.push_back (e.second);
    }
    return move(output);
}
