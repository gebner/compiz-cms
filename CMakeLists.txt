cmake_minimum_required(VERSION 2.8)

find_package (Compiz REQUIRED)

include (CompizPlugin)

compiz_plugin (cms
    PLUGINDEPS composite opengl
    PKGDEPS lcms xrandr colord
  )
