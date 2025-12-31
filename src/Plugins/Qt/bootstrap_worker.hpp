/******************************************************************************
 * MODULE     : bootstrap_worker.hpp
 * DESCRIPTION: Background initialization worker for Mogan bootstrap
 * COPYRIGHT  : (C) 2025
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef BOOTSTRAP_WORKER_H
#define BOOTSTRAP_WORKER_H

#include <QString>
#include <QThread>

/**
 * @class BootstrapWorker
 * @brief Background thread for performing Mogan initialization tasks
 *
 * This worker executes initialization tasks in the background while
 * the bootstrap window remains responsive. It provides progress updates
 * and handles initialization failures gracefully.
 */
class BootstrapWorker : public QThread {
  Q_OBJECT

public:
  enum InitializationStep {
    Step_NotStarted= 0,
    Step_FileSystemCheck,
    Step_ConfigurationLoad,
    Step_PluginInitialization,
    Step_SchemeEnvironment,
    Step_Complete,
    Step_Count // Total number of steps
  };

  explicit BootstrapWorker (QObject* parent= nullptr);
  ~BootstrapWorker ();

  /**
   * @brief Request the worker to stop gracefully
   *
   * This method sets a flag that will be checked in the run() method.
   * The worker will finish its current task and then exit.
   */
  void requestStop ();

  /**
   * @brief Get the current progress percentage
   * @return Progress percentage from 0 to 100
   */
  int progress () const { return m_progress; }

  /**
   * @brief Check if initialization was successful
   * @return true if initialization completed successfully
   */
  bool isSuccessful () const { return m_success; }

  /**
   * @brief Get the last error message
   * @return Error message if initialization failed, empty string otherwise
   */
  QString errorMessage () const { return m_errorMessage; }

signals:
  /**
   * @brief Signal emitted when progress updates
   * @param step Current initialization step
   * @param message Descriptive message of current task
   * @param percentage Progress percentage (0-100)
   */
  void progressUpdated (int step, const QString& message, int percentage);

  /**
   * @brief Signal emitted when initialization completes
   * @param success true if initialization succeeded, false otherwise
   */
  void initializationComplete (bool success);

  /**
   * @brief Signal emitted when an error occurs
   * @param error Error message describing the problem
   */
  void errorOccurred (const QString& error);

  /**
   * @brief Signal emitted for time estimation updates
   * @param elapsedMs Time elapsed in milliseconds
   * @param estimatedTotalMs Estimated total time in milliseconds
   */
  void timeEstimationUpdated (qint64 elapsedMs, qint64 estimatedTotalMs);

protected:
  void run () override;

private:
  // Main initialization method
  bool performInitialization ();

  // Individual initialization steps
  bool initializeFileSystem ();
  bool initializeConfiguration ();
  bool initializePlugins ();
  bool initializeSchemeEnvironment ();

  // Progress tracking
  void updateProgress (InitializationStep step, const QString& message);
  void calculateDynamicProgress ();

  // Time estimation
  void   updateTimeEstimation ();
  qint64 estimateStepTime (InitializationStep step) const;

  // Thread control
  volatile bool m_stopRequested;
  volatile bool m_running;

  // State tracking
  int                m_progress;
  bool               m_success;
  QString            m_errorMessage;
  InitializationStep m_currentStep;

  // Time tracking for dynamic progress
  qint64          m_startTime;
  qint64          m_stepStartTime;
  QVector<qint64> m_stepDurations;
};

#endif // BOOTSTRAP_WORKER_H