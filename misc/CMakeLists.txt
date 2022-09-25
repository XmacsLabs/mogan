if (APPLE)
  configure_file(scripts/mogan.sh.in ${TEXMACS_BINARY_DIR}/misc/scripts/mogan.sh @ONLY)
else (APPLE)
  configure_file(scripts/mogan.sh.in ${TEXMACS_BINARY_DIR}/misc/scripts/mogan @ONLY)
endif (APPLE)
configure_file (man/mogan.1.in ${TEXMACS_SOURCE_DIR}/misc/man/mogan.1 @ONLY)

### share/man
set (TOOLS "mogan")
set (MAN_SECTION "1")
foreach (TOOL ${TOOLS})
  set (TOOL_MANFILE ${CMAKE_CURRENT_SOURCE_DIR}/man/${TOOL}.${MAN_SECTION})
  set (TOOL_MANFILE_GZ ${CMAKE_CURRENT_BINARY_DIR}/${TOOL}.${MAN_SECTION}.gz)
  add_custom_command (
    OUTPUT ${TOOL_MANFILE_GZ}
    COMMAND gzip -n -c ${TOOL_MANFILE} > ${TOOL_MANFILE_GZ}
    MAIN_DEPENDENCY ${TOOL_MANFILE}
    COMMENT "Building ${TOOL} man page"
    VERBATIM
  )
  add_custom_target (${TOOL}.${MAN_SECTION} ALL DEPENDS ${TOOL_MANFILE_GZ})
  install (FILES ${TOOL_MANFILE_GZ} DESTINATION share/man/man${MAN_SECTION})
endforeach (TOOL)
