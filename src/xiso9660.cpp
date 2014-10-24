//#include <vector>
#include <unistd.h>
#include <cdio/iso9660.h>
#include <cdio++/iso9660.hpp>
#include <SDL.h>
#include "xiso9660.hpp"
#include <map>

using namespace std;

map<unsigned int, string> xiso_file_hash {};

static void
build_hash (ISO9660::IFS& iso, const char psz_path[]);

void
xiso9660_initialize_hash (ISO9660::IFS& iso)
{
    xiso_file_hash.empty();
    build_hash(iso, "");
}

unsigned int quick_hash(const string& str)
{
    unsigned int val = 0;
    for (size_t i = 0; i < str.size(); i ++) {
        val = val * 101 + (unsigned char) (str[i]);
        val = val & 0xFFFF;
    }
    return val;
}

static bool exists_test (const std::string& name) {
    return ( access( name.c_str(), F_OK ) != -1 );
}

static int round_up(int numToRound, int multiple)
{
    if(multiple == 0)
        return numToRound;

    int remainder = numToRound % multiple;
    if (remainder == 0)
        return numToRound;
    return numToRound + multiple - remainder;
}

ISO9660::IFS xiso9660_open(const string& name)
{
    ISO9660::IFS iso {};

    if (!exists_test(name)) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                     "Can't access ISO file '%s'", name.c_str());
        exit (1);
    }

    bool ret = iso.open(name.c_str());
    if (ret == false) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                     "Can't open ISO file '%s'", name.c_str());
        exit (1);
    }
    return iso;
}

vector<char> xiso9660_get_data(ISO9660::IFS& iso, const string& name)
{
    ISO9660::Stat * s = iso.stat(name.c_str());
    if (s->p_stat == NULL) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                     "Can't open ISO file member '%s'", name.c_str());
        exit (1);
    }

    size_t true_length = s->p_stat->size;
    size_t block_length = round_up(s->p_stat->size, ISO_BLOCKSIZE);

    vector<char> buffer (block_length);

    // Read the data from the buffer in 2K blocks
    for (size_t i = 0; i < block_length; i += ISO_BLOCKSIZE) {
        iso.seek_read(buffer.data() + i,
                                            s->p_stat->lsn + (i / ISO_BLOCKSIZE));
        
        // FIXME: maybe warn if bytes read != ISO_BLOCKSIZE
    }

    // The last block may need to be trimmed to the true file length.
    buffer.erase (buffer.begin() + true_length, buffer.end());
    return buffer;
}

void xiso9660_close(ISO9660::IFS& iso)
{
    iso.close();
}

// Builds a filename-to-ID hash map for a given ISO file.  This
// function is recursive, and the top-level call should have a
// psz_path == "".
static void
build_hash (ISO9660::IFS& iso, const char psz_path[])
{
    uint8_t i_joliet_level = iso.get_joliet_level();
    stat_vector_t entlist;

    if (!iso.readdir(psz_path, entlist)) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                     "Error getting above directory information: %s\n", psz_path);
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
                                       (char *)full_name.c_str(),
                                       i_joliet_level);
        }
        full_name.insert(0, psz_path);

        // Recursively descend into subdirectories
        if (p_statbuf->type == 2 /* _STAT_DIR */
            && strcmp (p_statbuf->filename, ".")
            && strcmp (p_statbuf->filename, "..")) {
            full_name.append("/");
            build_hash(iso, full_name.c_str());
        }

        // For regular files, add them to the hash table
        if (p_statbuf->type == 1) {
            // There is an initial slash in this filename that needs to go
            // full_name.erase(0,1);
            printf("%s\n", full_name.c_str());
            xiso_file_hash[quick_hash(full_name)] = full_name;
            SDL_LogDebug(SDL_LOG_CATEGORY_APPLICATION,
                         "Resource Hash: adding %s as %u",
                         full_name.c_str(),
                         quick_hash(full_name));
        }
    }
}
