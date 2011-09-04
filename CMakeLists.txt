find_package (Compiz REQUIRED)

include (CompizPlugin)

compiz_plugin (cms
    PLUGINDEPS composite opengl
    PKGDEPS lcms xrandr colord
  )
