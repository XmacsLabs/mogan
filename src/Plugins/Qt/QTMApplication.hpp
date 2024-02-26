
/******************************************************************************
 * MODULE     : QTMApplication.hpp
 * DESCRIPTION:
 * COPYRIGHT  :
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QTMAPPLICATION_HPP
#define QTMAPPLICATION_HPP

#include "renderer.hpp"
#include "string.hpp"
#include "sys_utils.hpp"
#include "tm_url.hpp"
#include "url.hpp"
#include <QApplication>
#include <QIcon>
#include <QStyle>

#include <QApplication>
#include <QScreen>

void init_palette (QApplication* app);
void init_style_sheet (QApplication* app);
void set_standard_style_sheet (QWidget* w);

/*
 FIXME: We would like to do the following

 #ifdef USE_EXCEPTIONS
 class QTMApplication... blah blah

 #else

 typedef QApplication QTMApplication;

 #endif

 But MOC has trouble with conditional compilation.
 */

/*! QTMApplication

 Reimplements notify() in order to catch exceptions thrown from event handlers
 and slots.

 NOTE: see http://qt-project.org/forums/viewthread/17731 for the reason why
 the constructor takes an int&
 */
class QTMApplication : public QApplication {
  Q_OBJECT

public:
  QTMApplication (int& argc, char** argv) : QApplication (argc, argv) {
    init_palette (this);
    init_style_sheet (this);
    QGuiApplication::setHighDpiScaleFactorRoundingPolicy (
        Qt::HighDpiScaleFactorRoundingPolicy::RoundPreferFloor);
    if (!retina_manual) {
      qreal ratio  = QApplication::primaryScreen ()->devicePixelRatio ();
      retina_factor= qRound (ratio - 0.1);
    }
  }

  void set_window_icon (string icon_path) {
    url icon_url= url_system (get_env ("TEXMACS_PATH") * icon_path);
    if (exists (icon_url)) {
      const c_string _icon (as_string (icon_url));
      setWindowIcon (QIcon ((const char*) _icon));
    }
    else
      std_warning << "Could not find TeXmacs icon file: "
                  << as_string (icon_url) << LF;
  }

  /*
  bool event(QEvent *event) {
    if (event->type() == QEvent::TabletEnterProximity ||
        event->type() == QEvent::TabletLeaveProximity) {
      cout << "Set tablet device\n";
      //tm_canvas->setTabletDevice(static_cast<QTabletEvent *>(event));
      return true;
    }
    return QApplication::event(event);
  }
  */

  virtual bool notify (QObject* receiver, QEvent* event) {
    try {
      return QApplication::notify (receiver, event);
    } catch (string s) {
      // c_string cs (s);
      // tm_failure (cs);
      // qt_error << "Thrown " << s << LF;
      the_exception= s;
    }
    return false;
  }
};

class QTMCoreApplication : public QCoreApplication {
  Q_OBJECT

public:
  QTMCoreApplication (int& argc, char** argv) : QCoreApplication (argc, argv) {}

  void set_window_icon (string icon_path) { (void) icon_path; }

  virtual bool notify (QObject* receiver, QEvent* event) {
    try {
      return QCoreApplication::notify (receiver, event);
    } catch (string s) {
      qt_error << "Thrown " << s << LF;
      the_exception= s;
    }
    return false;
  }
};

#endif // QTMAPPLICATION_HPP
