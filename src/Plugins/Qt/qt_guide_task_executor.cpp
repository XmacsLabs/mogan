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
#include <QDateTime>
#include <QDebug>
#include <QThread>

// External declarations from init_texmacs.cpp
extern void init_main_paths ();
extern void init_user_dirs ();
extern void init_texmacs ();
extern bool g_startup_login_executed;

BootstrapTaskExecutor::BootstrapTaskExecutor (QObject* parent)
    : QObject (parent), m_running (false), m_cancelled (false),
      m_completed (false), m_success (false), m_progress (0),
      m_errorMessage (""), m_currentStatus (""),
      m_currentStep (TaskStep::NotStarted), m_startTime (0),
      m_stepStartTime (0), m_executionTimer (nullptr) {

  // 初始化步骤持续时间（毫秒）
  // 这些是默认值，实际执行时会根据实际耗时更新
  m_stepDurations.resize (static_cast<int> (TaskStep::Complete) + 1);
  m_stepDurations[static_cast<int> (TaskStep::FileSystemCheck)]  = 500; // 0.5秒
  m_stepDurations[static_cast<int> (TaskStep::ConfigurationLoad)]= 1000; // 1秒
  m_stepDurations[static_cast<int> (TaskStep::PluginInitialization)]=
      3000;                                                              // 3秒
  m_stepDurations[static_cast<int> (TaskStep::SchemeEnvironment)]= 2000; // 2秒
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
  // 如果已经在运行或已完成，则直接返回
  if (m_running || m_completed) {
    return;
  }

  // 重置所有状态
  m_running  = true;
  m_cancelled= false;
  m_completed= false;
  m_success  = false;
  m_progress = 0;
  m_errorMessage.clear ();
  m_currentStatus.clear ();
  m_currentStep  = TaskStep::FileSystemCheck;
  m_startTime    = QDateTime::currentMSecsSinceEpoch ();
  m_stepStartTime= m_startTime;

  qDebug () << "BootstrapTaskExecutor: 在主线程中启动Mogan初始化...";

  // 创建执行计时器（如果不存在）
  if (!m_executionTimer) {
    m_executionTimer= new QTimer (this);
    m_executionTimer->setSingleShot (true);
    connect (m_executionTimer, &QTimer::timeout, this,
             &BootstrapTaskExecutor::executeNextSegment);
  }

  // 启动执行，使用小延迟以允许UI更新
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
  // 检查是否已取消或未运行
  if (m_cancelled || !m_running) {
    handleExecutionStopped ();
    return;
  }

  try {
    // 执行当前步骤
    bool     stepSuccess= false;
    QString  stepMessage;
    TaskStep nextStep= TaskStep::NotStarted;

    // 根据当前步骤执行相应的任务
    switch (m_currentStep) {
    case TaskStep::FileSystemCheck:
      stepSuccess= performFileSystemCheck ();
      stepMessage= "检查文件系统...";
      nextStep   = TaskStep::ConfigurationLoad;
      break;

    case TaskStep::ConfigurationLoad:
      stepSuccess= performConfigurationLoad ();
      stepMessage= "加载配置...";
      nextStep   = TaskStep::PluginInitialization;
      break;

    case TaskStep::PluginInitialization:
      stepSuccess= performPluginInitialization ();
      stepMessage= "初始化插件...";
      nextStep   = TaskStep::SchemeEnvironment;
      break;

    case TaskStep::SchemeEnvironment:
      stepSuccess= performSchemeEnvironmentSetup ();
      stepMessage= "准备Scheme环境...";
      nextStep   = TaskStep::Complete;
      break;

    case TaskStep::Complete:
      // 所有步骤完成
      completeInitialization ();
      return;

    default:
      // 不应该到达这里
      handleUnexpectedStep ();
      return;
    }

    // 处理步骤执行结果
    if (stepSuccess) {
      // 步骤成功，更新进度并移动到下一步
      updateProgress (m_currentStep, stepMessage);
      m_currentStep= nextStep;

      // 如果还有下一步，调度下一次执行
      if (m_currentStep != TaskStep::Complete) {
        scheduleNextExecution ();
      }
      else {
        // 所有步骤完成
        completeInitialization ();
      }
    }
    else {
      // 步骤失败
      handleStepFailure (stepMessage);
    }

  } catch (const std::exception& e) {
    handleException (e.what ());
  } catch (...) {
    handleUnknownException ();
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
    qWarning () << "BootstrapTaskExecutor: File system check failed:"
                << e.what ();
    return false;
  } catch (...) {
    qWarning ()
        << "BootstrapTaskExecutor: Unknown error during file system check";
    return false;
  }
}

bool
BootstrapTaskExecutor::performConfigurationLoad () {
  // 占位符方法：配置加载
  // TODO: 实现实际的配置加载逻辑
  // 应包括：
  // - 加载用户偏好设置
  // - 设置环境变量
  // - 初始化应用程序设置

  // 短暂延迟以允许UI更新
  QThread::msleep (60);
  return true;
}

bool
BootstrapTaskExecutor::performPluginInitialization () {
  // 占位符方法：插件初始化
  // 注意：核心初始化应在主线程中完成
  // 此方法专注于可以在后台并发运行的任务

  qDebug () << "BootstrapTaskExecutor: Performing plugin initialization...";

  // TODO: 实现实际的插件初始化逻辑
  // 应包括：
  // - 加载和初始化插件
  // - 设置插件依赖关系
  // - 注册插件服务

  qDebug () << "BootstrapTaskExecutor: Plugin initialization complete";
  QThread::msleep (50);
  return true;
}

bool
BootstrapTaskExecutor::performSchemeEnvironmentSetup () {
  // 占位符方法：Scheme环境设置
  // TODO: 实现实际的Scheme环境设置逻辑
  // 应包括：
  // - 设置Scheme加载路径
  // - 初始化Scheme运行时
  // - 加载核心Scheme模块

  // 短暂延迟以允许UI更新
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
  if (m_currentStep > TaskStep::NotStarted &&
      m_currentStep < TaskStep::Complete) {
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
  return 1000; // 默认1秒
}

// ============================================================================
// 辅助方法：错误处理和状态管理
// ============================================================================

void
BootstrapTaskExecutor::handleExecutionStopped () {
  m_running  = false;
  m_completed= true;
  m_success  = false;
  if (m_cancelled) {
    m_errorMessage= "初始化被用户取消";
  }
  emit initializationComplete (false);
}

void
BootstrapTaskExecutor::completeInitialization () {
  m_running  = false;
  m_completed= true;
  m_success  = true;
  updateProgress (TaskStep::Complete, "初始化完成");
  emit initializationComplete (true);
}

void
BootstrapTaskExecutor::handleUnexpectedStep () {
  m_errorMessage=
      QString ("意外的步骤: %1").arg (static_cast<int> (m_currentStep));
  m_running  = false;
  m_completed= true;
  m_success  = false;
  emit errorOccurred (m_errorMessage);
  emit initializationComplete (false);
}

void
BootstrapTaskExecutor::handleStepFailure (const QString& stepMessage) {
  m_running     = false;
  m_completed   = true;
  m_success     = false;
  m_errorMessage= QString ("步骤失败: %1").arg (stepMessage);
  emit errorOccurred (m_errorMessage);
  emit initializationComplete (false);
}

void
BootstrapTaskExecutor::handleException (const QString& error) {
  m_running     = false;
  m_completed   = true;
  m_success     = false;
  m_errorMessage= QString ("异常: %1").arg (error);
  qWarning () << "BootstrapTaskExecutor: 异常:" << m_errorMessage;
  emit errorOccurred (m_errorMessage);
  emit initializationComplete (false);
}

void
BootstrapTaskExecutor::handleUnknownException () {
  m_running     = false;
  m_completed   = true;
  m_success     = false;
  m_errorMessage= "初始化过程中发生未知异常";
  qWarning () << "BootstrapTaskExecutor: 未知异常";
  emit errorOccurred (m_errorMessage);
  emit initializationComplete (false);
}

void
BootstrapTaskExecutor::scheduleNextExecution () {
  // 调度下一次执行，保持UI响应性（段之间10ms延迟）
  if (m_executionTimer) {
    m_executionTimer->start (10);
  }
  else {
    // 备用方案：使用singleShot
    QTimer::singleShot (10, this, &BootstrapTaskExecutor::executeNextSegment);
  }
}