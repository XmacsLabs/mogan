/******************************************************************************
 * MODULE     : bootstrap_worker.cpp
 * DESCRIPTION: Background initialization worker implementation
 * COPYRIGHT  : (C) 2025
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "bootstrap_worker.hpp"
#include "boot.hpp"
#include "data_cache.hpp"
#include "file.hpp"
#include "preferences.hpp"
#include "sys_utils.hpp"
#include "tm_file.hpp"
#include "tm_ostream.hpp"
#include "tm_sys_utils.hpp"
#include "tm_timer.hpp"
#include <QDateTime>
#include <QDebug>
#include <QElapsedTimer>

// External declarations from init_texmacs.cpp
extern int  install_status;
extern void setup_texmacs ();
extern void init_upgrade ();
extern void setup_tex ();
extern void init_tex ();
extern void init_main_paths ();
extern void init_user_dirs ();
extern void acquire_boot_lock ();
extern void init_texmacs ();

BootstrapWorker::BootstrapWorker (QObject* parent)
    : QThread (parent), m_stopRequested (false), m_running (false),
      m_progress (0), m_success (false), m_currentStep (Step_NotStarted),
      m_startTime (0), m_stepStartTime (0) {
  // Initialize step durations with reasonable defaults (in milliseconds)
  // These will be updated based on actual performance
  m_stepDurations.resize (Step_Count);
  m_stepDurations[Step_FileSystemCheck]     = 500;  // 0.5 seconds
  m_stepDurations[Step_ConfigurationLoad]   = 1000; // 1 second
  m_stepDurations[Step_PluginInitialization]= 3000; // 3 seconds
  m_stepDurations[Step_SchemeEnvironment]   = 2000; // 2 seconds
}

BootstrapWorker::~BootstrapWorker () {
  requestStop ();
  if (isRunning ()) {
    wait (1000); // Wait up to 1 second for thread to finish
  }
}

void
BootstrapWorker::requestStop () {
  m_stopRequested= true;
}

void
BootstrapWorker::run () {
  m_running      = true;
  m_stopRequested= false;
  m_success      = false;
  m_errorMessage.clear ();
  m_startTime= QDateTime::currentMSecsSinceEpoch ();

  qDebug () << "BootstrapWorker: Starting Mogan initialization...";

  try {
    m_success= performInitialization ();

    if (m_stopRequested) {
      qDebug () << "BootstrapWorker: Initialization stopped by user request";
      m_success     = false;
      m_errorMessage= "Initialization stopped by user";
    }
  } catch (const std::exception& e) {
    m_success     = false;
    m_errorMessage= QString ("Initialization error: %1").arg (e.what ());
    qWarning () << "BootstrapWorker: Exception:" << m_errorMessage;
  } catch (...) {
    m_success     = false;
    m_errorMessage= "Unknown error during initialization";
    qWarning () << "BootstrapWorker: Unknown exception";
  }

  m_running= false;

  if (m_success) {
    qDebug () << "BootstrapWorker: Initialization completed successfully";
    updateProgress (Step_Complete, "Initialization complete");
    emit initializationComplete (true);
  }
  else {
    qWarning () << "BootstrapWorker: Initialization failed:" << m_errorMessage;
    emit errorOccurred (m_errorMessage);
    emit initializationComplete (false);
  }
}

bool
BootstrapWorker::performInitialization () {
  // Step 1: File system check and preparation
  updateProgress (Step_FileSystemCheck, "Checking file system...");
  if (m_stopRequested) return false;
  if (!initializeFileSystem ()) {
    m_errorMessage= "File system initialization failed";
    return false;
  }

  // Step 2: Configuration loading
  updateProgress (Step_ConfigurationLoad, "Loading configuration...");
  if (m_stopRequested) return false;
  if (!initializeConfiguration ()) {
    m_errorMessage= "Configuration initialization failed";
    return false;
  }

  // Step 3: Plugin initialization (original init_plugins() logic)
  updateProgress (Step_PluginInitialization, "Initializing plugins...");
  if (m_stopRequested) return false;
  if (!initializePlugins ()) {
    m_errorMessage= "Plugin initialization failed";
    return false;
  }

  // Step 4: Scheme environment setup
  updateProgress (Step_SchemeEnvironment, "Preparing Scheme environment...");
  if (m_stopRequested) return false;
  if (!initializeSchemeEnvironment ()) {
    m_errorMessage= "Scheme environment initialization failed";
    return false;
  }

  return true;
}

bool
BootstrapWorker::initializeFileSystem () {
  try {
    // // Initialize main paths (sets TEXMACS_HOME_PATH)
    // init_main_paths ();

    // // Create user directories structure
    // init_user_dirs ();

    // acquire_boot_lock ();

    // init_texmacs();

    return true;
  } catch (const std::exception& e) {
    qWarning () << "BootstrapWorker: File system initialization failed:" << e.what ();
    return false;
  } catch (...) {
    qWarning () << "BootstrapWorker: Unknown error during file system initialization";
    return false;
  }
}

bool
BootstrapWorker::initializeConfiguration () {
  // TODO: Implement configuration loading
  // This should include:
  // - Loading user preferences
  // - Setting up environment variables
  // - Initializing application settings

  // For now, simulate some work
  QThread::msleep (300);
  return true;
}

bool
BootstrapWorker::initializePlugins () {
  // This function performs time-consuming plugin initialization tasks
  // Note: Core initialization (install_status setup) should be done in main
  // thread This function focuses on background tasks that can run concurrently

  qDebug () << "BootstrapWorker: Starting background plugin initialization...";

  // TODO: Implement actual background initialization tasks
  // These might include:
  // - Loading plugin metadata and dependencies
  // - Building plugin caches
  // - Initializing plugin-specific resources
  // - Pre-warming plugin functionality

  // For demonstration, simulate plugin initialization time
  // In real implementation, this would call actual initialization functions
  int totalSteps= 10;
  for (int i= 1; i <= totalSteps && !m_stopRequested; ++i) {
    QThread::msleep (100); // Simulate work
    QString message=
        QString ("Loading plugin module %1/%2").arg (i).arg (totalSteps);

    // Calculate progress within this step
    int stepProgress= (i * 100) / totalSteps;
    int overallProgress=
        60 + (stepProgress * 20 / 100); // 60-80% range for plugin init

    emit progressUpdated (static_cast<int> (Step_PluginInitialization), message,
                          overallProgress);
  }

  if (m_stopRequested) {
    return false;
  }

  qDebug () << "BootstrapWorker: Background plugin initialization complete";
  return true;
}

bool
BootstrapWorker::initializeSchemeEnvironment () {
  // TODO: Implement Scheme environment initialization
  // This should include:
  // - Setting up Scheme load paths
  // - Initializing Scheme runtime
  // - Loading core Scheme modules

  // For now, simulate some work
  QThread::msleep (500);
  return true;
}

void
BootstrapWorker::updateProgress (InitializationStep step,
                                 const QString&     message) {
  m_currentStep= step;

  // Calculate progress percentage based on step
  // Simple linear progression for now, will be enhanced with dynamic
  // calculation
  int baseProgress= 0;
  switch (step) {
  case Step_FileSystemCheck:
    baseProgress= 10;
    break;
  case Step_ConfigurationLoad:
    baseProgress= 30;
    break;
  case Step_PluginInitialization:
    baseProgress= 60;
    break;
  case Step_SchemeEnvironment:
    baseProgress= 85;
    break;
  case Step_Complete:
    baseProgress= 100;
    break;
  default:
    baseProgress= 0;
  }

  m_progress= baseProgress;

  // Emit progress signal
  emit progressUpdated (static_cast<int> (step), message, m_progress);

  // Update time estimation
  updateTimeEstimation ();

  qDebug () << "BootstrapWorker: Progress update - Step" << step << ":"
            << message << "(" << m_progress << "%)";
}

void
BootstrapWorker::calculateDynamicProgress () {
  // This method will implement intelligent progress calculation
  // based on actual time spent vs estimated time for each step

  if (m_currentStep == Step_NotStarted || m_currentStep == Step_Complete) {
    return;
  }

  qint64 currentTime  = QDateTime::currentMSecsSinceEpoch ();
  qint64 stepElapsed  = currentTime - m_stepStartTime;
  qint64 stepEstimated= estimateStepTime (m_currentStep);

  if (stepEstimated > 0) {
    // Calculate intra-step progress (0-100 within current step)
    int intraStepProgress=
        qMin (100, static_cast<int> ((stepElapsed * 100) / stepEstimated));

    // Map to overall progress
    int stepIndex=
        static_cast<int> (m_currentStep) - 1; // Convert to 0-based index
    int stepsCompleted= stepIndex;
    int totalSteps    = Step_Count - 2; // Exclude NotStarted and Complete

    if (totalSteps > 0) {
      int progressPerStep= 100 / totalSteps;
      m_progress         = (stepsCompleted * progressPerStep) +
                  (intraStepProgress * progressPerStep / 100);
      m_progress= qMin (100, m_progress);
    }
  }
}

void
BootstrapWorker::updateTimeEstimation () {
  qint64 currentTime= QDateTime::currentMSecsSinceEpoch ();
  qint64 elapsed    = currentTime - m_startTime;

  // Store actual duration for current step
  if (m_currentStep > Step_NotStarted && m_currentStep < Step_Complete) {
    if (m_stepStartTime > 0) {
      qint64 stepDuration= currentTime - m_stepStartTime;
      m_stepDurations[static_cast<int> (m_currentStep)]= stepDuration;
    }
    m_stepStartTime= currentTime;
  }

  // Calculate estimated total time based on completed steps
  qint64 estimatedTotal= 0;
  for (int i= 0; i < Step_Count; ++i) {
    if (i < static_cast<int> (m_currentStep)) {
      // Use actual duration for completed steps
      estimatedTotal+= m_stepDurations[i];
    }
    else {
      // Use estimated duration for remaining steps
      estimatedTotal+= estimateStepTime (static_cast<InitializationStep> (i));
    }
  }

  emit timeEstimationUpdated (elapsed, estimatedTotal);
}

qint64
BootstrapWorker::estimateStepTime (InitializationStep step) const {
  if (step >= 0 && step < Step_Count) {
    return m_stepDurations[static_cast<int> (step)];
  }
  return 1000; // Default 1 second
}