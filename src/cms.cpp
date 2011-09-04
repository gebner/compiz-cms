/* vim:sts=4 sw=4:
 *
 * Copyright (c) 2006 Darryll Truchan <moppsy@comcast.net>
 *	     (C) 2008 Gerhard Fürnkranz
 *	     (C) 2008 Tomas Carnecky
 *	     (C) 2009-2011 Kai-Uwe Behrmann
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

#include "cms.h"
#include <lcms.h>
#include <GL/glext.h>
#include <X11/Xatom.h>
#include <limits.h>

#define compWarning(msg...) \
    compLogMessage("cms", CompLogLevelWarn, msg)

using namespace GLFragment;

COMPIZ_PLUGIN_20090315 (cms, CmsPluginVTable);

CmsFunction::CmsFunction (int	  target,
			  bool    alpha,
			  int	  param,
			  int	  unit)
    : id(0), alpha(alpha), target(target), param(param), unit(unit)
{
    FunctionData data;

    if (alpha)
    {
      data.addTempHeaderOp ("temp");
    }

    data.addFetchOp ("output", NULL, target);

    if (alpha)
    {
	data.addDataOp ("MUL output.rgb, output.a, output;");
	data.addDataOp ("MUL temp.a, output.a, output.a;");
    }

    data.addDataOp ("MAD output, output, program.env[%d], program.env[%d];", param, param + 1);

    data.addDataOp ("TEX output, output, texture[%d], 3D;", unit);

    if (alpha)
    {
	data.addDataOp ("MUL output, temp.a, output;");
    }

    data.addColorOp ("output", "output");

    if (!data.status ()) {
	id = 0;
    } else {
	id = data.createFragmentFunction ("cms");
    }
}

CmsFunction::~CmsFunction() {
    if (id) {
	GL::deletePrograms(1, &id);
	id = 0;
    }
}

const int GRIDSIZE = 64;
struct memlut {
    GLushort a[GRIDSIZE][GRIDSIZE][GRIDSIZE][3];
};

CmsLut::CmsLut (cmsHPROFILE monitorProfile)
    : texture_id(0)
{
    // populate sampling grid
    std::auto_ptr<memlut> in(new memlut),
			  out(new memlut);

    for (int r = 0; r < GRIDSIZE; r++) {
	for (int g = 0; g < GRIDSIZE; g++) {
	    for (int b = 0; b < GRIDSIZE; b++) {
		in->a[b][g][r][0] = ((0xffff-1)*r)/GRIDSIZE;
		in->a[b][g][r][1] = ((0xffff-1)*g)/GRIDSIZE;
		in->a[b][g][r][2] = ((0xffff-1)*b)/GRIDSIZE;
	    }
	}
    }

    // transform
    cmsHPROFILE inputProfile = cmsCreate_sRGBProfile();
    cmsHTRANSFORM transf = cmsCreateTransform(
	inputProfile, TYPE_RGB_16,
	monitorProfile, TYPE_RGB_16,
	INTENT_PERCEPTUAL,
	cmsFLAGS_NOTPRECALC);

    cmsDoTransform(transf, in.get(), out.get(), GRIDSIZE*GRIDSIZE*GRIDSIZE);

    cmsDeleteTransform(transf);
    cmsCloseProfile(inputProfile);

    // save into a texture
    glGenTextures(1, &texture_id);
    glBindTexture(GL_TEXTURE_3D, texture_id);

    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glTexImage3D(GL_TEXTURE_3D, 0, GL_RGB16, GRIDSIZE,GRIDSIZE,GRIDSIZE,
	0, GL_RGB, GL_UNSIGNED_SHORT, out->a);
}

CmsLut::~CmsLut() {
    if (texture_id) {
	glDeleteTextures(1, &texture_id);
	texture_id = 0;
    }
}

CmsLut *CmsLut::fromFile(const char *filename) {
    cmsHPROFILE profile = cmsOpenProfileFromFile(filename, "r");
    if (!profile) return 0;

    CmsLut *lut = new CmsLut(profile);

    cmsCloseProfile(profile);
    
    return lut;
}

CmsLut *CmsLut::fromMemory(unsigned char *icc, int len) {
    cmsHPROFILE profile = cmsOpenProfileFromMem(icc, len);
    if (!profile) return 0;

    CmsLut *lut = new CmsLut(profile);

    cmsCloseProfile(profile);
    
    return lut;
}

CmsOutput::CmsOutput(CompScreen *screen, RROutput output)
	: screen(screen), output(output), rect(), connected(false), lut(0), hasPerOutputProfile(true), name(), device(0) {
    updateRect();
}

void CmsOutput::setLUT(CmsLut *newLut) {
    if (lut) {
	delete lut;
    }
    lut = newLut;

    foreach (CompWindow *window, screen->windows()) {
	CompositeWindow::get(window)->addDamage();
    }
}

void CmsOutput::updateRect() {
    XRRScreenResources *res = XRRGetScreenResources(
	screen->dpy(), screen->root());
    if (!res) {
	return;
    }

    XRROutputInfo *outputInfo = XRRGetOutputInfo(
	screen->dpy(), res, output);
    if (!outputInfo) {
	XRRFreeScreenResources(res);
	return;
    }
	
    name = outputInfo->name;
    connected = outputInfo->connection != RR_Disconnected;

    XRRCrtcInfo *crtcInfo = XRRGetCrtcInfo(
	screen->dpy(), res, outputInfo->crtc);
    if (!crtcInfo) {
	XRRFreeOutputInfo(outputInfo);
	XRRFreeScreenResources(res);
	return;
    }

    rect = CompRect(crtcInfo->x, crtcInfo->y,
	crtcInfo->width, crtcInfo->height);

    XRRFreeCrtcInfo(crtcInfo);
    XRRFreeOutputInfo(outputInfo);
    XRRFreeScreenResources(res);
}

void CmsOutput::updateLUT() {
    CmsLut *newLut;
    unsigned char *icc;
    Atom type;
    int format, res;
    unsigned long len, bytes_left;

    // fetch ICC profile from colord
    char *filename = getColordProfileFilename();
    if (filename) {
	newLut = CmsLut::fromFile(filename);
	free(filename);
	if (newLut) {
	    setLUT(newLut);
	    return;
	}
    }

    // fetch ICC profile from output _ICC_PROFILE property
    CMS_SCREEN (screen);

    res = XRRGetOutputProperty(screen->dpy(), output,
	cs->_ICC_PROFILE,
	0, LONG_MAX,
	false,
	false,
	XA_CARDINAL,
	&type,
	&format,
	&len, &bytes_left,
	&icc);
    if (res == Success && len != 0) {
	CmsLut *newLut = CmsLut::fromMemory(icc, len);
	XFree(icc);

	if (newLut) {
	    setLUT(newLut);
	    return;
	}
    }
    
    // fetch ICC profile from root _ICC_PROFILE property
    res = XGetWindowProperty(screen->dpy(), screen->root(),
	cs->_ICC_PROFILE,
	0, LONG_MAX,
	false,
	XA_CARDINAL,
	&type,
	&format,
	&len, &bytes_left,
	&icc);
    if (res == Success && len != 0) {
	CmsLut *newLut = CmsLut::fromMemory(icc, len);
	XFree(icc);

	if (newLut) {
	    hasPerOutputProfile = false;
	    setLUT(newLut);
	    return;
	}
    }

    // no profile found, so remove lut
    setLUT(0);
}

void CmsOutput::setDevice(CdDevice *d) {
    if (device) {
	g_signal_handler_disconnect(device, onDeviceChangeId);
	g_object_unref(device);
    }
    device = d;

    if (device) {
	onDeviceChangeId = g_signal_connect(device, "changed", G_CALLBACK(onDeviceChange), this);
    }
}

void CmsOutput::onDeviceChange(CdDevice *dev, CmsOutput *out) {
    out->updateLUT();
}

char *CmsOutput::getColordProfileFilename() {
    if (!device) return 0;

    CdProfile *profile = 0;
    GError *error = 0;
    const char *filename;
    char *ret = 0;

    profile = cd_device_get_default_profile(device);
    if (!profile) {
	//compWarning("cannot find profile for colord device with xrandr name '%s'", outputInfo->name);
	goto cleanup;
    }

    if (!cd_profile_connect_sync(profile, 0, &error)) {
	//compWarning("cannot connect to profile for colord device with xrandr name '%s': %s", outputInfo->name, error->message);
	g_error_free(error);
	goto cleanup;
    }

    filename = cd_profile_get_filename(profile);
    if (!filename) {
	//compWarning("cannot get filename for profile for colord device with xrandr name '%s'", outputInfo->name);
	goto cleanup;
    }

    ret = strdup(filename);

cleanup:
    if (profile) g_object_unref(profile);

    return ret;
}

CmsOutput::~CmsOutput() {
    setLUT(0);
    setDevice(0);
}

CmsScreen::CmsScreen (CompScreen *screen) :
    PluginClassHandler <CmsScreen, CompScreen> (screen),
    CmsOptions (),
    gScreen (GLScreen::get (screen))
{
    ScreenInterface::setHandler (screen, false);

    optionSetExcludeMatchNotify (
	boost::bind (&CmsScreen::optionChanged, this, _1, _2));
    optionSetDecorationsNotify (
	boost::bind (&CmsScreen::optionChanged, this, _1, _2));

    _ICC_PROFILE = XInternAtom(screen->dpy(), "_ICC_PROFILE", false);
    XRRQueryExtension(screen->dpy(), &randrEvent, &randrError);

    XRRSelectInput(screen->dpy(), screen->root(),
	RROutputPropertyNotifyMask | RROutputChangeNotifyMask);
    screen->handleEventSetEnabled (this, true);

    GError *error = 0;
    cd_client = cd_client_new();
    if (!cd_client_connect_sync(cd_client, 0, &error)) {
	compWarning("cannot connect to colord: %s", error->message);
	g_error_free(error);
	g_object_unref(cd_client);
	cd_client = 0;
    } else {
	g_signal_connect(cd_client, "device-added", G_CALLBACK(onCdDeviceAdded), this);
	g_signal_connect(cd_client, "device-removed", G_CALLBACK(onCdDeviceRemoved), this);
    }

    setupOutputs();
}

CmsScreen::~CmsScreen ()
{
    if (cd_client) g_object_unref(cd_client);
}

void
CmsScreen::optionChanged (CompOption          *opt,
			  CmsOptions::Options num)
{
    switch (num)
    {
    case CmsOptions::Decorations:
    case CmsOptions::ExcludeMatch:
	{
	    foreach (CompWindow *w, screen->windows ())
	    {
		CMS_WINDOW (w);

		cw->updateMatch ();
	    }
	}
	break;
    default:
	break;
    }
}

void
CmsScreen::handleEvent (XEvent *event)
{
    screen->handleEvent (event);

    if (event->type == PropertyNotify
	    && event->xproperty.window == screen->root()
	    && event->xproperty.atom == _ICC_PROFILE) {
	if (!hasPerOutputProfiles()) {
	    foreach (CmsOutput& output, cmsOutputs) {
		output.updateLUT();
	    }
	}
    } else if (event->type == randrEvent + RRNotify
	    && ((XRRNotifyEvent *) event)->subtype == RRNotify_OutputProperty) {
	XRROutputPropertyNotifyEvent *ev =
	    (XRROutputPropertyNotifyEvent *) event;
	if (ev->property == _ICC_PROFILE) {
	    foreach (CmsOutput& output, cmsOutputs) {
		if (output.output == ev->output) {
		    output.updateRect();
		    output.updateLUT();
		}
	    }
	}
    } else if (event->type == randrEvent + RRNotify
	    && ((XRRNotifyEvent *) event)->subtype == RRNotify_OutputChange) {
	XRROutputChangeNotifyEvent *ev =
	    (XRROutputChangeNotifyEvent *) event;
	
	foreach (CmsOutput& output, cmsOutputs) {
	    if (output.output == ev->output) {
		output.updateRect();
		output.updateLUT();
	    }
	}
    }
}

void CmsScreen::onCdDeviceAdded(CdClient *, CdDevice *device, CmsScreen *cs) {
    GError *error;
    if (!cd_device_connect_sync(device, 0, &error)) {
	//compWarning("cannot connect to profile for colord device with xrandr name '%s': %s", outputInfo->name, error->message);
	g_error_free(error);
	g_object_unref(device);
	return;
    }

    const char *name = cd_device_get_metadata_item(device, CD_DEVICE_METADATA_XRANDR_NAME);
    foreach (CmsOutput& output, cs->cmsOutputs) {
	if (output.name == name) {
	    output.setDevice(device);
	    return;
	}
    }

    compWarning("could not find output for colord device: %s",
	cd_device_get_object_path(device));
    g_object_unref(device);
}

void CmsScreen::onCdDeviceRemoved(CdClient *, CdDevice *device, CmsScreen *cs) {
    foreach (CmsOutput& output, cs->cmsOutputs) {
	if (output.device) {
	    if (!strcmp(cd_device_get_object_path(device),
		    cd_device_get_object_path(output.device))) {
		output.setDevice(0);
		return;
	    }
	}
    }

    compWarning("could not find output for removed colord device: %s",
	cd_device_get_object_path(device));
    g_object_unref(device);
}

void
CmsScreen::setupOutputs () {
    if (!screen->XRandr()) return;

    XRRScreenResources *res =
	XRRGetScreenResources(screen->dpy(), screen->root());
    if (!res) return;

    for (int i = 0; i < res->noutput; i++) {
	CmsOutput *output = new CmsOutput(screen, res->outputs[i]);
	setupCdDevice(output);
	output->updateLUT();
	cmsOutputs.push_back(output);
    }

    XRRFreeScreenResources(res);
}

void
CmsScreen::setupCdDevice(CmsOutput *output) {
    if (!cd_client) return;

    if (!output->connected) return;

    GError *error = 0;
    CdDevice *device = cd_client_find_device_by_property_sync(
	cd_client, CD_DEVICE_METADATA_XRANDR_NAME, output->name.c_str(),
	0, &error);
    if (!device) {
	return;
    }

    if (!cd_device_connect_sync(device, 0, &error)) {
	compWarning("could not connect to colord device %s: %s",
	    cd_device_get_object_path(device), error->message);
	g_object_unref(device);
	return;
    }

    output->setDevice(device);
}

bool
CmsScreen::hasPerOutputProfiles () {
    foreach (CmsOutput& output, cmsOutputs) {
	if (!output.hasPerOutputProfile) {
	    return false;
	}
    }
    return true;
}

GLuint
CmsScreen::getFragmentFunction (int	  target,
				bool      alpha,
				int	  param,
				int	  unit)
{
    foreach (CmsFunction& f, cmsFunctions)
    {
	if (f.alpha == alpha && f.target == target && f.param == param && f.unit == unit)
	{
	    return f.id;
	}
    }

    CmsFunction *f = new CmsFunction(target, alpha, param, unit);

    cmsFunctions.push_back(f);

    return f->id;
}

CmsWindow::CmsWindow (CompWindow *window) :
    PluginClassHandler <CmsWindow, CompWindow> (window),
    window (window),
    cWindow (CompositeWindow::get (window)),
    gWindow (GLWindow::get (window)),
    isCms (false)
{
    GLWindowInterface::setHandler (gWindow, true);

    updateMatch ();
}

CmsWindow::~CmsWindow ()
{
}

void
CmsWindow::updateMatch ()
{
    CMS_SCREEN (screen);

    isCms = !cs->optionGetExcludeMatch ().evaluate (window);

    cWindow->addDamage ();
}

void
CmsWindow::glDrawTexture (GLTexture          *texture,
			  GLFragment::Attrib &attrib,
			  unsigned int       mask)
{
    CMS_SCREEN (screen);

    bool isDecoration = true;
    foreach (GLTexture *tex, gWindow->textures ())
    {
	if (texture == tex)
	{
	    isDecoration = false;
	    break;
	}
    }

    bool doCms = isDecoration ? cs->optionGetDecorations () : isCms;
    CmsLut *lut = 0;

    if (doCms) {
	CompRect borderRect = window->borderRect();
	foreach (CmsOutput& out, cs->cmsOutputs) {
	    if (borderRect.intersects(out.rect) && out.lut) {
		lut = out.lut;
		break;
	    }
	}
    }

    if (lut && doCms && GL::fragmentProgram)
    {
	GLFragment::Attrib fa = attrib;

	bool alpha = isDecoration || window->alpha();

	int target;
	if (texture->target () == GL_TEXTURE_2D)
	    target = COMP_FETCH_TARGET_2D;
	else
	    target = COMP_FETCH_TARGET_RECT;

	int param = fa.allocParameters(2);
	int unit = fa.allocTextureUnits(1);
	GLuint function = cs->getFragmentFunction (target, alpha, param, unit);
	fa.addFunction (function);

	GLfloat scale = (GLfloat) (GRIDSIZE - 1) / GRIDSIZE;
	GLfloat offset = (GLfloat) 1.0 / (2 * GRIDSIZE);

	GL::programEnvParameter4f( GL_FRAGMENT_PROGRAM_ARB, param + 0,
		scale, scale, scale, 1.0);
	GL::programEnvParameter4f( GL_FRAGMENT_PROGRAM_ARB, param + 1,
		offset, offset, offset, 0.0);

	GL::activeTexture (GL_TEXTURE0_ARB + unit);
	glBindTexture(GL_TEXTURE_3D, lut->texture_id);
	GL::activeTexture (GL_TEXTURE0_ARB);

	gWindow->glDrawTexture (texture, fa, mask);
    }
    else
    {
	/* no cms */
	gWindow->glDrawTexture (texture, attrib, mask);
    }
}


bool
CmsPluginVTable::init ()
{
    if (!CompPlugin::checkPluginABI ("core", CORE_ABIVERSION) ||
	!CompPlugin::checkPluginABI ("composite", COMPIZ_COMPOSITE_ABI) ||
	!CompPlugin::checkPluginABI ("opengl", COMPIZ_OPENGL_ABI))
	return false;

    return true;
}
