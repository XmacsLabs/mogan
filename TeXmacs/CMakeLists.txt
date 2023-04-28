if (WIN32)
  install (DIRECTORY . DESTINATION ${CMAKE_INSTALL_PREFIX}
    FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ
    PATTERN "include/" EXCLUDE
    PATTERN "plugins/" EXCLUDE
    PATTERN "doc/about/xmacs" EXCLUDE
    PATTERN "doc/devel/xmacs" EXCLUDE
    PATTERN "CMakeLists.txt" EXCLUDE)
else (WIN32)
  install(DIRECTORY . DESTINATION share/Xmacs
    FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ
    PATTERN "include/" EXCLUDE
    PATTERN "plugins/" EXCLUDE
    PATTERN "doc/about/xmacs" EXCLUDE
    PATTERN "doc/devel/xmacs" EXCLUDE
    PATTERN "CMakeLists.txt" EXCLUDE)
endif (WIN32)

add_subdirectory (plugins)
