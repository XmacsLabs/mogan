#ifndef WASM_LOCAL_FILE_HPP
#define WASM_LOCAL_FILE_HPP
#include "new_buffer.hpp"
#include "new_view.hpp"
#include "url.hpp"
#include "Qt/qt_utilities.hpp"
#include <QtWidgets>
#include <QByteArray>

bool buffer_load_from_local();
bool buffer_save_to_local();

#endif // WASM_LOCAL_FILE_HPP
