//
// Created by kumar on 2016/03/29.
//

// via https://github.com/kumar8600/win32_SetProcessDpiAware

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>

/// Sets the current process as (per-monitor) dpi aware.
/// \return true if succeed.
bool win32_SetProcessDpiAware(void);

#ifdef __cplusplus
}
#endif
