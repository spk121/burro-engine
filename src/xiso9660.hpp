#ifndef XISO9660_HPP
#define XISO9660_HPP

#include <string>
#include <map>
#include <cdio++/iso9660.hpp>

ISO9660::IFS xiso9660_open(const std::string& name);
vector<char> xiso9660_get_data(ISO9660::IFS& iso, const string& name);
void         xiso9660_close(ISO9660::IFS& iso);
void         xiso9660_initialize_hash (ISO9660::IFS& iso);
unsigned int quick_hash(string str);

extern map<unsigned int, string> xiso_file_hash;

#endif
