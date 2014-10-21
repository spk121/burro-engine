#include "SDL.h"
#include "ResCache.hpp"
#include "xiso9660.hpp"
#include "ResourceFile.hpp"

ResCache resource_cache {10};

ResCache::ResCache(const unsigned int size_in_MB)
    : m_lru {},
    m_resources {},
    m_cache_size {size_in_MB * 1024 * 1024},
    m_allocated {0}
{

}

ResCache::~ResCache()
{
    while (m_lru.size() > 0)
        Free_oldest_resource();
}

// Get the contents of a resource by ID, while, at the same time,
// marking the resource as recently used.
void* ResCache::Get(unsigned int ID)
{
    ResHandle* handle = Find(ID);
    if (handle == NULL)
        return Load(ID);
    return Update(handle);
}


// Look for resource by ID.
// Return a handle to the resource, or NULL on failure.
ResHandle* ResCache::Find(unsigned int ID)
{
    auto i = m_resources.find(ID);
    if (i == m_resources.end())
        return NULL;
    return (*i).second;
}

// Returns true when there is cache space for the resource.
// May free old resource
bool ResCache::Maybe_free_old_resources(size_t size)
{
    if (size > m_cache_size)
        return false;

    while (size > m_cache_size - m_allocated) {
        if (m_lru.empty())
            return false;

        Free_oldest_resource();
    }

    return true;
}

// Given a handle to a resource, mark it as the most recently used.
void* ResCache::Update(ResHandle* handle)
{
    m_lru.remove(handle);
    m_lru.push_front(handle);
    return handle->get();
}

void* ResCache::Load(unsigned int ID)
{
    // Look of the filename in the strings-to-ID table
    SDL_LogDebug(SDL_LOG_CATEGORY_APPLICATION, "Resource Cache: Loading %x", ID);
    auto i = resource_file.xiso_file_hash.find(ID);
    if (i == resource_file.xiso_file_hash.end()) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                     "Resource Cache: no such ID: %x", ID);
        return NULL;
    }

    // Get the data in its file format
    string name = (*i).second;
    // ISO9660::IFS iso_file = xiso9660_open("data.iso");
    // vector<char> data = xiso9660_get_data(m_file, name);
    vector<char> data = resource_file.Get_data(name);
    // xiso9660_close(iso_file);

    // Unpack the data from its file format to its resource
    // format.
    ResHandle* handle = new ResHandle(ID, name, data);

    // Delete old resources if the cache is getting too big.
    bool ok = Maybe_free_old_resources(handle->get_size());
    if (!ok) {
        // Out of space in cache
        // Do something here.
    }

    // Push the new resource into the cache as the newest resource.
    m_lru.push_front(handle);
    m_resources[ID] = handle;

    return handle->get();
}

void ResCache::Free_oldest_resource()
{
    if (m_lru.size() == 0)
        return;

    auto victim = m_lru.back();

    ResHandle handle = *victim;

    // Get rid of it from the least-recently-used list
    m_lru.pop_back();
    m_resources.erase(handle.get_id());

    // Update the cache size
    m_allocated -= handle.get_size();
}
