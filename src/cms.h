/*
 * Copyright (c) 2006 Darryll Truchan <moppsy@comcast.net>
 *	     (C) 2011 Gabriel Ebner <gebner@2b7e.org>
 *
 * Pixel shader cmsating by Dennis Kasprzyk <onestone@beryl-project.org>
 * Usage of matches by Danny Baumann <maniac@beryl-project.org>
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
#include <core/serialization.h>

#include <composite/composite.h>
#include <opengl/opengl.h>


#include "cms_options.h"

class CmsScreen :
    public PluginClassHandler <CmsScreen, CompScreen>,
    public CmsOptions
{
    public:

	CmsScreen (CompScreen *);
	virtual ~CmsScreen ();

	GLuint cmsFunction;
	int cmsFunctionParam, cmsFunctionUnit;
	
	bool
	checkStateTimeout ();

	void
	optionChanged (CompOption          *opt,
		       CmsOptions::Options num);

	GLuint
	getFragmentFunction (GLTexture *texture,
			     bool      alpha,
			     int       param,
			     int       unit);

	GLScreen *gScreen;

	GLuint lut;

    private:
	Atom _ICC_PROFILE;

	void
	setupLUT ();
};

class CmsWindow :
    public PluginClassHandler <CmsWindow, CompWindow>,
    public PluginStateWriter <CmsWindow>,
    public GLWindowInterface
{
    public:
    
	template <class Archive>
	void serialize (Archive &ar, const unsigned int version)
	{
	    ar & isCms;
	}
	
	void postLoad ();

	CmsWindow (CompWindow *);
	~CmsWindow ();

	CompWindow      *window;
	CompositeWindow *cWindow;
	GLWindow        *gWindow;

	bool isCms;

	void
	updateMatch ();

	void
	glDrawTexture(GLTexture          *texture,
		      GLFragment::Attrib &attrib,
		      unsigned int       mask);
};

#define CMS_SCREEN(s)							      \
    CmsScreen *ns = CmsScreen::get (s);

#define CMS_WINDOW(w)							      \
    CmsWindow *nw = CmsWindow::get (w);

class CmsPluginVTable :
    public CompPlugin::VTableForScreenAndWindow <CmsScreen, CmsWindow>
{
    public:

	bool init ();
};
