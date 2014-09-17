#ifndef BURRO_RESCACHE_H
#define BURRO_RESCACHE_H


#include <map>
#include <list>
#include <algorithm>
#include "ResHandle.hpp"
#include "xiso9660.hpp"

using namespace std;

struct ResCache {
    list<ResHandle*> m_lru;
    map<int, ResHandle*> m_resources;
    
    // The resource file interface
    ISO9660::IFS m_file;
    
    // Max bytes in cache
    unsigned int m_cache_size;
    // Bytes allocated currently
    unsigned int m_allocated;
    
    // Return a resource from the cache by ID
    void* Load(unsigned int ID);
    ResHandle* Find(unsigned int ID);
    void* Update(ResHandle* handle);
    
    // Delete the least important resource
    bool Maybe_free_old_resources(size_t size);
    void Free_oldest_resource();
    
    ResCache(const unsigned int size_in_MB, string pack_filename);
    ~ResCache();
    void* Get(unsigned int ID);

};


#endif

