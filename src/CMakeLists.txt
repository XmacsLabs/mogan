### --------------------------------------------------------------------
### Set the executable target name and other stuff
### --------------------------------------------------------------------

set (TeXmacs_binary_name "Mogan")

add_library(texmacs_body STATIC ${TeXmacs_All_SRCS})
target_link_libraries(texmacs_body PUBLIC ${TeXmacs_Libraries} ${CMAKE_DL_LIBS})

add_executable (${TeXmacs_binary_name} Texmacs/Texmacs/texmacs.cpp)
target_link_libraries (${TeXmacs_binary_name} PRIVATE texmacs_body)

if (WIN32)
  set_target_properties(${TeXmacs_binary_name} PROPERTIES WIN32_EXECUTABLE TRUE)
  target_link_libraries (${TeXmacs_binary_name} PRIVATE secur32)
endif (WIN32)

if (APPLE)
  set(MACOSX_BUNDLE_DISPLAY_NAME TeXmacs)
  if (${CMAKE_BUILD_TYPE} STREQUAL "Release")
    set_target_properties (${TeXmacs_binary_name}
      PROPERTIES
      MACOSX_BUNDLE TRUE
      MACOSX_BUNDLE_INFO_PLIST ${TEXMACS_SOURCE_DIR}/packages/macos/Info.plist.in
      DEBUG_OUTPUT_NAME ${TeXmacs_binary_name}${EXE_DEBUG_EXTENSION}
      RELEASE_OUTPUT_NAME ${TeXmacs_binary_name}
      RUNTIME_OUTPUT_DIRECTORY ${TEXMACS_BINARY_DIR}
    )
  else ()
    install (TARGETS ${TeXmacs_binary_name} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
  endif ()
elseif (WIN32)
  install (TARGETS ${TeXmacs_binary_name} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
else (APPLE)
  install (TARGETS ${TeXmacs_binary_name} DESTINATION ${tmbin}/bin)
endif (APPLE)

