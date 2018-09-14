#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <stdlib.h>

#include "octo.h"
#include "errors.h"

const char *log_prefix = "[%s] %04d-%02d-%02d %02d:%02d:%02d : ";
const char *log_postfix = "\n";

void octo_log(enum ERROR_LEVEL level, enum ERROR error, ...) {
  va_list args;
  va_start(args, error);
  const char *type;
  time_t log_time;
  struct tm local_time;

  if(level < config->record_error_level)
    return;

  log_time = time(NULL);
  local_time = *localtime(&log_time);

  switch(level) {
  case TRACE:
    type = "TRACE";
    break;
  case INFO:
    type = "INFO";
    break;
  case DEBUG:
    type = "DEBUG";
    break;
  case WARNING:
    type = "WARNING";
    break;
  case ERROR:
    type = "ERROR";
    break;
  case FATAL:
    type = "FATAL";
    break;
  default:
    type = "UNKNOWN";
    break;
  }
  fprintf(stderr, log_prefix, type,
    local_time.tm_year + 1900,
    local_time.tm_mon + 1,
    local_time.tm_mday,
    local_time.tm_hour,
    local_time.tm_min,
    local_time.tm_sec);
  switch(error) {
  case CUSTOM_ERROR:
    vfprintf(stderr, va_arg(args, const char *), args);
    break;
  default:
    vfprintf(stderr, err_format_str[error], args);
    break;
  }
  va_end(args);
  fprintf(stderr, log_postfix);
  if(level == FATAL) {
    exit(error);
  }
  return;
}
