#pragma once

#include <string>
#include <vector>
#include <cairo.h>

using namespace std;

class ResHandle
{
private:
    unsigned int m_ID;
    
    enum ha {
        HANDLE_RAW,
        HANDLE_CAIRO_SURFACE
    } m_type;
    
    
    union {
        void *m_buffer;
        cairo_surface_t *m_cairo_surface;
    };

    unsigned int m_size;

    void Extract_png_data(vector<char>& data);

public:    
    ResHandle(unsigned int id, string name, vector<char>& data);
    unsigned int get_id() { return m_ID; }
    void *get();
    unsigned int get_size() {return m_size; }
};

