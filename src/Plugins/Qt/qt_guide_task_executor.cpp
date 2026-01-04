/******************************************************************************
 * MODULE     : qt_guide_task_executor.cpp
 * DESCRIPTION: Single-threaded initialization task executor implementation
 * COPYRIGHT  : (C) 2025 yinyu
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_guide_task_executor.hpp"
#include "boot.hpp"
#include "data_cache.hpp"
#include "file.hpp"
#include "preferences.hpp"
#include "sys_utils.hpp"
#include "tm_file.hpp"
#include "tm_ostream.hpp"
#include "tm_sys_utils.hpp"
#include "tm_timer.hpp"
#include "language.hpp"
#include <QDateTime>
#include <QDebug>
#include <QElapsedTimer>
#include <QThread>

#include <moebius/drd/drd_std.hpp>
#include <moebius/data/scheme.hpp>
using moebius::data::block_to_scheme_tree;
using moebius::data::scheme_tree_to_block;
using moebius::data::scm_quote;
using moebius::drd::init_std_drd;

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
extern bool g_startup_login_executed;

BootstrapTaskExecutor::BootstrapTaskExecutor (QObject* parent)
    : QObject (parent), m_running (false), m_cancelled (false),
      m_completed (false), m_success (false), m_progress (0),
      m_errorMessage (""), m_currentStatus (""),
      m_currentStep (TaskStep::NotStarted), m_startTime (0),
      m_stepStartTime (0), m_executionTimer (nullptr) {

  // Initialize step durations with reasonable defaults (in milliseconds)
  m_stepDurations.resize (static_cast<int> (TaskStep::Complete) + 1);
  m_stepDurations[static_cast<int> (TaskStep::FileSystemCheck)]     = 500;  // 0.5 seconds
  m_stepDurations[static_cast<int> (TaskStep::ConfigurationLoad)]   = 1000; // 1 second
  m_stepDurations[static_cast<int> (TaskStep::PluginInitialization)]= 3000; // 3 seconds
  m_stepDurations[static_cast<int> (TaskStep::SchemeEnvironment)]   = 2000; // 2 seconds
}

BootstrapTaskExecutor::~BootstrapTaskExecutor () {
  cancel ();
  if (m_executionTimer) {
    m_executionTimer->stop ();
    delete m_executionTimer;
  }
}

void
BootstrapTaskExecutor::start () {
  if (m_running || m_completed) {
    return;
  }

  m_running      = true;
  m_cancelled    = false;
  m_completed    = false;
  m_success      = false;
  m_progress     = 0;
  m_errorMessage.clear ();
  m_currentStatus.clear ();
  m_currentStep  = TaskStep::FileSystemCheck;
  m_startTime    = QDateTime::currentMSecsSinceEpoch ();
  m_stepStartTime= m_startTime;

  qDebug () << "BootstrapTaskExecutor: Starting Mogan initialization in main thread...";

  // Create execution timer if not exists
  if (!m_executionTimer) {
    m_executionTimer= new QTimer (this);
    m_executionTimer->setSingleShot (true);
    connect (m_executionTimer, &QTimer::timeout, this,
             &BootstrapTaskExecutor::executeNextSegment);
  }

  // Start execution with a small delay to allow UI to update
  QTimer::singleShot (10, this, &BootstrapTaskExecutor::executeNextSegment);
}

void
BootstrapTaskExecutor::cancel () {
  if (m_running && !m_cancelled) {
    m_cancelled= true;
    qDebug () << "BootstrapTaskExecutor: Cancellation requested";
  }
}

void
BootstrapTaskExecutor::executeNextSegment () {
  if (m_cancelled || !m_running) {
    m_running  = false;
    m_completed= true;
    m_success  = false;
    if (m_cancelled) {
      m_errorMessage= "Initialization cancelled by user";
    }
    emit initializationComplete (false);
    return;
  }

  try {
    // Execute current step
    bool stepSuccess= false;
    QString stepMessage;

    TaskStep currentStepBeforeUpdate= m_currentStep;

    switch (m_currentStep) {
    case TaskStep::FileSystemCheck:
      stepSuccess = performFileSystemCheck ();
      stepMessage = "Checking file system...";
      if (stepSuccess) {
        // Update progress for current step before moving to next
        updateProgress (currentStepBeforeUpdate, stepMessage);
        m_currentStep= TaskStep::ConfigurationLoad;
      }
      break;

    case TaskStep::ConfigurationLoad:
      stepSuccess = performConfigurationLoad ();
      stepMessage = "Loading configuration...";
      if (stepSuccess) {
        // Update progress for current step before moving to next
        updateProgress (currentStepBeforeUpdate, stepMessage);
        m_currentStep= TaskStep::PluginInitialization;
      }
      break;

    case TaskStep::PluginInitialization:
      stepSuccess = performPluginInitialization ();
      stepMessage = "Initializing plugins...";
      if (stepSuccess) {
        // Update progress for current step before moving to next
        updateProgress (currentStepBeforeUpdate, stepMessage);
        m_currentStep= TaskStep::SchemeEnvironment;
      }
      break;

    case TaskStep::SchemeEnvironment:
      stepSuccess = performSchemeEnvironmentSetup ();
      stepMessage = "Preparing Scheme environment...";
      if (stepSuccess) {
        // Update progress for current step before moving to next
        updateProgress (currentStepBeforeUpdate, stepMessage);
        m_currentStep= TaskStep::Complete;
      }
      break;

    case TaskStep::Complete:
      // All steps completed successfully
      m_running  = false;
      m_completed= true;
      m_success  = true;
      updateProgress (TaskStep::Complete, "Initialization complete");
      emit initializationComplete (true);
      return;

    default:
      // Should not reach here
      m_errorMessage= QString ("Unexpected step: %1")
                          .arg (static_cast<int> (m_currentStep));
      m_running  = false;
      m_completed= true;
      m_success  = false;
      emit errorOccurred (m_errorMessage);
      emit initializationComplete (false);
      return;
    }

    if (!stepSuccess) {
      // Step failed
      m_running  = false;
      m_completed= true;
      m_success  = false;
      m_errorMessage= QString ("Step failed: %1").arg (stepMessage);
      emit errorOccurred (m_errorMessage);
      emit initializationComplete (false);
      return;
    }

    // Schedule next execution segment with a small delay
    // to keep UI responsive (10ms delay between segments)
    if (m_executionTimer) {
      m_executionTimer->start (10);
    } else {
      // Fallback: use singleShot
      QTimer::singleShot (10, this, &BootstrapTaskExecutor::executeNextSegment);
    }

  } catch (const std::exception& e) {
    m_running  = false;
    m_completed= true;
    m_success  = false;
    m_errorMessage= QString ("Exception: %1").arg (e.what ());
    qWarning () << "BootstrapTaskExecutor: Exception:" << m_errorMessage;
    emit errorOccurred (m_errorMessage);
    emit initializationComplete (false);
  } catch (...) {
    m_running  = false;
    m_completed= true;
    m_success  = false;
    m_errorMessage= "Unknown exception during initialization";
    qWarning () << "BootstrapTaskExecutor: Unknown exception";
    emit errorOccurred (m_errorMessage);
    emit initializationComplete (false);
  }
}

bool
BootstrapTaskExecutor::performFileSystemCheck () {
  try {
    cout << "Step 1 start....\n";

    cout << "performFileSystemCheck -- Main paths\n";
    init_main_paths ();
    cout << "performFileSystemCheck -- User dirs\n";
    init_user_dirs ();

    cout << "performFileSystemCheck -- init_texmacs\n";
    init_texmacs ();
    // 引导弹窗中，init_texmacs执行后设置
    g_startup_login_executed= true;

    return true;
  } catch (const std::exception& e) {
    qWarning () << "BootstrapTaskExecutor: File system check failed:" << e.what ();
    return false;
  } catch (...) {
    qWarning () << "BootstrapTaskExecutor: Unknown error during file system check";
    return false;
  }
}

bool
BootstrapTaskExecutor::performConfigurationLoad () {
  // TODO: Implement actual configuration loading
  // This should include:
  // - Loading user preferences
  // - Setting up environment variables
  // - Initializing application settings

  // Small delay to simulate work and allow UI updates
  QThread::msleep (60);
  return true;
}

bool
BootstrapTaskExecutor::performPluginInitialization () {
  // This function performs time-consuming plugin initialization tasks
  // Note: Core initialization (install_status setup) should be done in main
  // thread This function focuses on background tasks that can run concurrently

  qDebug () << "BootstrapTaskExecutor: Performing plugin initialization...";

  

  qDebug () << "BootstrapTaskExecutor: Plugin initialization complete";
  QThread::msleep (50);
  return true;
}

bool
BootstrapTaskExecutor::performSchemeEnvironmentSetup () {
  // TODO: Implement Scheme environment setup
  // This should include:
  // - Setting up Scheme load paths
  // - Initializing Scheme runtime
  // - Loading core Scheme modules

  // Small delay to simulate work and allow UI updates
  QThread::msleep (40);
  return true;
}

void
BootstrapTaskExecutor::updateProgress (TaskStep step, const QString& message) {
  m_currentStep  = step;
  m_currentStatus= message;

  // Calculate progress percentage based on step
  int baseProgress= 0;
  switch (step) {
  case TaskStep::FileSystemCheck:
    baseProgress= 10;
    break;
  case TaskStep::ConfigurationLoad:
    baseProgress= 30;
    break;
  case TaskStep::PluginInitialization:
    baseProgress= 60;
    break;
  case TaskStep::SchemeEnvironment:
    baseProgress= 85;
    break;
  case TaskStep::Complete:
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

  qDebug () << "BootstrapTaskExecutor: Progress update - Step"
            << static_cast<int> (step) << ":" << message << "(" << m_progress
            << "%)";
}

void
BootstrapTaskExecutor::updateTimeEstimation () {
  qint64 currentTime= QDateTime::currentMSecsSinceEpoch ();
  qint64 elapsed    = currentTime - m_startTime;

  // Store actual duration for current step
  if (m_currentStep > TaskStep::NotStarted && m_currentStep < TaskStep::Complete) {
    if (m_stepStartTime > 0) {
      qint64 stepDuration= currentTime - m_stepStartTime;
      m_stepDurations[static_cast<int> (m_currentStep)]= stepDuration;
    }
    m_stepStartTime= currentTime;
  }

  // Calculate estimated total time based on completed steps
  qint64 estimatedTotal= 0;
  for (int i= 0; i <= static_cast<int> (TaskStep::Complete); ++i) {
    if (i < static_cast<int> (m_currentStep)) {
      // Use actual duration for completed steps
      estimatedTotal+= m_stepDurations[i];
    }
    else {
      // Use estimated duration for remaining steps
      estimatedTotal+= estimateStepTime (static_cast<TaskStep> (i));
    }
  }

  emit timeEstimationUpdated (elapsed, estimatedTotal);
}

qint64
BootstrapTaskExecutor::estimateStepTime (TaskStep step) const {
  int index= static_cast<int> (step);
  if (index >= 0 && index < m_stepDurations.size ()) {
    return m_stepDurations[index];
  }
  return 1000; // Default 1 second
}