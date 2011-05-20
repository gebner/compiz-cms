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

using namespace GLFragment;

COMPIZ_PLUGIN_20090315 (cms, CmsPluginVTable);

void
CmsWindow::updateMatch ()
{
    CMS_SCREEN (screen);

    isCms = !ns->optionGetExcludeMatch ().evaluate (window);

    cWindow->addDamage ();
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

    if (!data.status ())
	return 0;

    CmsFunction f;
    f.id = data.createFragmentFunction ("cms");
    f.alpha = alpha;
    f.target = target;
    f.param = param;
    f.unit = unit;

    cmsFunctions.push_back(f);

    return f.id;
}

const int GRIDSIZE = 64;
struct memlut {
    GLushort a[GRIDSIZE][GRIDSIZE][GRIDSIZE][3];
};

void
CmsScreen::setupLUT ()
{
    // free existing lut
    if (lut != 0) {
	glDeleteTextures(1, &lut);
	lut = 0;
    }

    // fetch ICC profile
    unsigned char *icc;
    Atom type;
    int format;
    unsigned long len, bytes_left;
    XGetWindowProperty(screen->dpy(), screen->root(),
	_ICC_PROFILE,
	0, 0,
	false,
	XA_CARDINAL,
	&type,
	&format,
	&len, &bytes_left,
	&icc);
    if (type == None) {
	return;
    }
    XFree(icc);
    XGetWindowProperty(screen->dpy(), screen->root(),
	_ICC_PROFILE,
	0, bytes_left,
	false,
	XA_CARDINAL,
	&type,
	&format,
	&len, &bytes_left,
	&icc);
    
    cmsHPROFILE outputProfile = cmsOpenProfileFromMem(icc, len);
    if (!outputProfile) {
      XFree(icc);
      return;
    }

    // populate sampling grid
    memlut *input = new memlut;
    memlut *output = new memlut;

    for (int r = 0; r < GRIDSIZE; r++) {
	for (int g = 0; g < GRIDSIZE; g++) {
	    for (int b = 0; b < GRIDSIZE; b++) {
		input->a[b][g][r][0] = ((0xffff-1)*r)/GRIDSIZE;
		input->a[b][g][r][1] = ((0xffff-1)*g)/GRIDSIZE;
		input->a[b][g][r][2] = ((0xffff-1)*b)/GRIDSIZE;
	    }
	}
    }

    // transform
    cmsHPROFILE inputProfile = cmsCreate_sRGBProfile();
    cmsHTRANSFORM transf = cmsCreateTransform(
	inputProfile, TYPE_RGB_16,
	outputProfile, TYPE_RGB_16,
	INTENT_PERCEPTUAL,
	cmsFLAGS_NOTPRECALC);

    cmsDoTransform(transf, input, output, GRIDSIZE*GRIDSIZE*GRIDSIZE);

    cmsDeleteTransform(transf);
    XFree(icc);

    delete input;

    // save into a texture
    glGenTextures(1, &lut);
    glBindTexture(GL_TEXTURE_3D, lut);

    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    
    glTexImage3D(GL_TEXTURE_3D, 0, GL_RGB16, GRIDSIZE,GRIDSIZE,GRIDSIZE,
	0, GL_RGB, GL_UNSIGNED_SHORT, output->a);

    
    delete output;
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

    bool doCms = isDecoration ? ns->optionGetDecorations () : isCms;

    if (ns->lut && doCms && GL::fragmentProgram)
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
	GLuint function = ns->getFragmentFunction (target, alpha, param, unit);
	fa.addFunction (function);

	GLfloat scale = (GLfloat) (GRIDSIZE - 1) / GRIDSIZE;
	GLfloat offset = (GLfloat) 1.0 / (2 * GRIDSIZE);

	GL::programEnvParameter4f( GL_FRAGMENT_PROGRAM_ARB, param + 0,
		scale, scale, scale, 1.0);
	GL::programEnvParameter4f( GL_FRAGMENT_PROGRAM_ARB, param + 1,
		offset, offset, offset, 0.0);

	GL::activeTexture (GL_TEXTURE0_ARB + unit);
	glBindTexture(GL_TEXTURE_3D, ns->lut);
	GL::activeTexture (GL_TEXTURE0_ARB);

	gWindow->glDrawTexture (texture, fa, mask);
    }
    else
    {
	/* no cms */
	gWindow->glDrawTexture (texture, attrib, mask);
    }
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

		nw->updateMatch ();
	    }
	}
	break;
    default:
	break;
    }
}

CmsScreen::CmsScreen (CompScreen *screen) :
    PluginClassHandler <CmsScreen, CompScreen> (screen),
    CmsOptions (),
    gScreen (GLScreen::get (screen)),
    lut (0)
{
    optionSetExcludeMatchNotify (
	boost::bind (&CmsScreen::optionChanged, this, _1, _2));
    optionSetDecorationsNotify (
	boost::bind (&CmsScreen::optionChanged, this, _1, _2));

    _ICC_PROFILE = XInternAtom(screen->dpy(), "_ICC_PROFILE", false);

    setupLUT();
}

CmsScreen::~CmsScreen () {
    if (lut) {
	glDeleteTextures(1, &lut);
	lut = 0;
    }

    foreach (CmsFunction& f, cmsFunctions)
    {
	GL::deletePrograms(1, &f.id);
    }
    cmsFunctions.clear();
}

void
CmsWindow::postLoad ()
{
    updateMatch ();
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

bool
CmsPluginVTable::init ()
{
    if (!CompPlugin::checkPluginABI ("core", CORE_ABIVERSION) ||
	!CompPlugin::checkPluginABI ("composite", COMPIZ_COMPOSITE_ABI) ||
	!CompPlugin::checkPluginABI ("opengl", COMPIZ_OPENGL_ABI))
	return false;

    return true;
}
