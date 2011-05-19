set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}")

find_package (LCMS REQUIRED)
find_package (Compiz REQUIRED)

include (CompizPlugin)

compiz_plugin (cms
    PLUGINDEPS composite opengl
    LIBRARIES ${LCMS_LIBRARIES}
    INCDIRS ${LCMS_INCLUDE_DIR}
  )
