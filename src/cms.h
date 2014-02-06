/* vim:sts=4 sw=4:
 *
 * Copyright (c) 2006 Darryll Truchan <moppsy@comcast.net>
 *	     (C) 2011 Gabriel Ebner <gebner@2b7e.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *
 */

#include <core/core.h>
#include <core/pluginclasshandler.h>

#include <composite/composite.h>
#include <opengl/opengl.h>

#include <X11/extensions/Xrandr.h>
#include <boost/ptr_container/ptr_vector.hpp>
#include <memory>

#include <lcms.h>
#include <colord.h>

#include "cms_options.h"

struct CmsLut {
    CmsLut(cmsHPROFILE profile);
    ~CmsLut();

    GLuint texture_id;

    std::string getFragmentShader(bool alpha);

    static CmsLut *fromFile(const char *filename);
    static CmsLut *fromMemory(unsigned char *icc, int len);
};

struct CmsOutput {
    CmsOutput(CompScreen *screen, RROutput output);
    ~CmsOutput();

    CompScreen *screen;
    RROutput output;
    CompRect rect;
    bool connected;
    CmsLut *lut;
    bool hasPerOutputProfile;
    std::string name;

    void setLUT(CmsLut *lut);

    void updateRect();
    void updateLUT();

    CdDevice *device;
    void setDevice(CdDevice *);
    char *getColordProfileFilename();
    static void onDeviceChange(CdDevice *dev, CmsOutput *out);
    gulong onDeviceChangeId;
};

class CmsScreen :
    public PluginClassHandler<CmsScreen, CompScreen>,
    public ScreenInterface,
    public CmsOptions
{
public:

    CmsScreen(CompScreen *);
    virtual ~CmsScreen();

    GLScreen *gScreen;

    boost::ptr_vector<CmsOutput> cmsOutputs;

    Atom _ICC_PROFILE;
    int randrEvent, randrError;

    CdClient *cd_client;
    static void onCdConnectFinish(CdClient *, GAsyncResult *, CmsScreen *);
    template <bool unrefAfterUse> void connectToDevice(CdDevice *);
    static void onCdDeviceAdded(CdClient *, CdDevice *, CmsScreen *);
    static void onCdDeviceRemoved(CdClient *, CdDevice *, CmsScreen *);
    template <bool unrefAfterUse> static void onCdDeviceConnectFinish(CdDevice *, GAsyncResult *, CmsScreen *);
    static void onCdClientFindDeviceByPropertyFinish(CdClient *, GAsyncResult *, CmsScreen *);

    void optionChanged(CompOption *opt, CmsOptions::Options num);
    void handleEvent(XEvent *event);

    void setupOutputs();

    bool hasPerOutputProfiles();
};

class CmsWindow :
    public PluginClassHandler<CmsWindow, CompWindow>,
    public GLWindowInterface
{
public:

    CmsWindow(CompWindow *);
    ~CmsWindow();

    CompWindow      *window;
    CompositeWindow *cWindow;
    GLWindow        *gWindow;

    bool isCms;

    void updateMatch();

    void glDrawTexture(GLTexture *texture, const GLMatrix &transform,
	const GLWindowPaintAttrib &attrib, unsigned int mask);
};

#define CMS_SCREEN(s)							      \
    CmsScreen *cs = CmsScreen::get(s);

#define CMS_WINDOW(w)							      \
    CmsWindow *cw = CmsWindow::get(w);

class CmsPluginVTable :
    public CompPlugin::VTableForScreenAndWindow<CmsScreen, CmsWindow>
{
public:

    bool init();
};
