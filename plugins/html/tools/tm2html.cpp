
#include <stdio.h>
#include <unistd.h>
#include "scheme.hpp"
#include "string.hpp"
#include "file.hpp"
#include "object.hpp"

int main (int argc, char *argv[]) {
    printf("argc: %d\n", argc);
    ASSERT (argc == 4, "argc must be 2");
    printf ("this is a tm2html tool");
    printf ("argv[0]: %s\n", argv[0]);
    printf ("argv[1]: %s\n", argv[1]);
    printf ("argv[2]: %s\n", argv[2]);
    printf ("argv[3]: %s\n", argv[3]);
    if (string(argv[1]) == "-d") {
        printf("Converting from directory\n");
        char* from = argv[2];
        char* to = argv[3];
        printf("from_dir: %s\n", from);
        printf("to_dir: %s\n", to);
        char buffer[1024];
        auto cwd = getcwd(buffer, sizeof(buffer));
        if (cwd == NULL) {
            printf("getcwd failed\n");
            return 1;
        }
        printf("cwd: %s\n", cwd);
        auto from_dir = url(cwd) * url_unix("../../../..") * url_unix(from);
        cout << "from_dir: " << as_system_string(from_dir) << "\n";
        auto to_dir = url(cwd) * url_unix("../../../..") * url_unix(to);
        cout << "to_dir: " << as_system_string(to_dir) << "\n";

        // check is directory
        if (!is_directory(from_dir)) {
            printf("from_dir is not a directory\n");
            return 1;
        }
        if (!is_directory(to_dir)) {
            mkdir(to_dir);
        }

        // call scheme function
        (void) call ("tmweb-convert-dir", object (as_string(from_dir)), object (as_string(to_dir)));

    }
    
}
