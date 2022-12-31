#include "wasm_local_file.hpp"
#include "convert.hpp"
#include "new_document.hpp"
#include "tm_window.hpp"

bool buffer_load_from_local(){
    auto fileContentReady = [](const QString &fileName, const QByteArray &fileContent) {
        if (fileName.isEmpty()) {
            // No file was selected
        } else {
            // Use fileName and fileContent
            const char* cstr= fileContent.constData ();
            string dataAsString = string ((char*) cstr);
            url fileUrl = url(from_qstring_utf8(fileName));
            tree t = import_loaded_tree (dataAsString, fileUrl, "generic");
            set_buffer_tree (fileUrl, t);
            switch_to_buffer(fileUrl);
        }
    };
    QFileDialog::getOpenFileContent("*.*",  fileContentReady);
    return true;
}

bool buffer_save_to_local(){
    string fm = "texmacs";
    url currentBuf = get_current_buffer_safe();
    tm_view vw= concrete_view (get_recent_view (currentBuf));
    ASSERT (vw != NULL, "view expected");
    tree body= subtree (the_et, vw->buf->rp);
    vw->ed->get_data (vw->buf->data);
    tree doc= attach_data (body, vw->buf->data, !vw->ed->get_save_aux());
    tree aux= doc;
    string s= tree_to_generic (aux, fm * "-document");
    const char* a = &s[0];
    QByteArray data = QByteArray::fromRawData(a, N(s));
    QFileDialog::saveFileContent(data, to_qstring(as_string(currentBuf)));
    return true;
}
