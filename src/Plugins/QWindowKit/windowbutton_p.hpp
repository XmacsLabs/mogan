// Copyright (C) 2023-2024 Stdware Collections (https://www.github.com/stdware)
// Copyright (C) 2021-2023 wangwenx190 (Yuhang Zhao)
// SPDX-License-Identifier: Apache-2.0

#ifndef WINDOWBUTTONPRIVATE_H
#define WINDOWBUTTONPRIVATE_H

#include "windowbutton.hpp"

namespace QWK {

class WindowButtonPrivate {
  Q_DECLARE_PUBLIC (WindowButton)
public:
  WindowButtonPrivate ();
  virtual ~WindowButtonPrivate ();

  void init ();

  WindowButton* q_ptr;

  QIcon iconNormal;
  QIcon iconChecked;
  QIcon iconDisabled;
  QIcon iconHovered;

  void reloadIcon ();
};

} // namespace QWK

#endif // WINDOWBUTTONPRIVATE_H