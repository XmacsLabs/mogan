if (WIN32)
  install (DIRECTORY . DESTINATION ${CMAKE_INSTALL_PREFIX}
    FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ
    PATTERN "include" EXCLUDE
    PATTERN "doc/about/xmacs" EXCLUDE
    PATTERN "doc/devel/xmacs" EXCLUDE
    PATTERN "CMakeLists.txt" EXCLUDE)
  install (FILES doc/about/xmacs/mogan_v${XMACS_VERSION}.en.tm
           DESTINATION ${CMAKE_INSTALL_PREFIX}/doc/about/mogan.en.tm)
  install (FILES doc/about/xmacs/mogan_v${XMACS_VERSION}.zh.tm
           DESTINATION ${CMAKE_INSTALL_PREFIX}/doc/about/mogan.zh.tm)
else (WIN32)
  install(DIRECTORY . DESTINATION share/TeXmacs
    FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ
    PATTERN "include" EXCLUDE
    PATTERN "doc/about/xmacs" EXCLUDE
    PATTERN "doc/devel/xmacs" EXCLUDE
    PATTERN "CMakeLists.txt" EXCLUDE)
  install (FILES doc/about/xmacs/mogan_v${XMACS_VERSION}.en.tm
           RENAME mogan.en.tm
           DESTINATION share/TeXmacs/doc/about/)
  install (FILES doc/about/xmacs/mogan_v${XMACS_VERSION}.zh.tm
           RENAME mogan.zh.tm
           DESTINATION share/TeXmacs/doc/about/)
endif (WIN32)
