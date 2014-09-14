#include <vector>
#include <string>
#include "ResHandle.hpp"

using namespace std;
// Given a vector that contains the data from a file from our resource pack,
// the ResHandle creates a vector that contains the "guts" of that file.
// For PNGs, the "guts" are Cairo image surfaces

vector<char> backend_data;

ResHandle::ResHandle(unsigned int id, string name, vector<char>& data)
    : m_ID {id},
    m_type {HANDLE_RAW},
    m_size {0}
{
    // The parsing depends on the suffix of the filename.
    if (name.rfind(".png") != string::npos) {
        Extract_png_data(data);
    }
}

static cairo_status_t fake_read(void *closure, unsigned char *data, unsigned int length)
{
    auto p = backend_data.begin();
    unsigned int i = 0;
    while(p != backend_data.end() && i < length) {
        data[i] = backend_data[i];
        i ++;
    }
    backend_data.erase(backend_data.begin(), backend_data.begin()+i);;
    if (i == length)
        return CAIRO_STATUS_SUCCESS;
    return CAIRO_STATUS_READ_ERROR;
}

// This is a bit wonky, because it uses a module level variable
// 'backend_data' and the 'fake_read' function to make a png stream.
// Probalby not thread safe.
void ResHandle::Extract_png_data(vector<char>& data)
{
    backend_data = data;
    cairo_surface_t* surf = cairo_image_surface_create_from_png_stream(fake_read, NULL);
    backend_data.clear();
    if (surf == NULL) {
        m_buffer = NULL;
        m_size = 0;
    }
    else {
        m_cairo_surface = surf;
        int height = cairo_image_surface_get_height(surf);
        int stride = cairo_image_surface_get_stride(surf);
        m_size = height * stride;
    }
}

void* ResHandle::get()
{
    return m_buffer;
}
