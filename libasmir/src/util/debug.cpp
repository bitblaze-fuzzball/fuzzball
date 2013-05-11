#include "debug.h"
#include <set>
#include <string>
#include <iostream>
using namespace std;

static set<string> debugKeys;

bool is_debug_on(const char *key)
{
  if (debugKeys.size() == 0)
    // fast-path return that avoids building a string
    return false;
  return (debugKeys.find(string(key)) != debugKeys.end());
}

void debug_on(const char *key)
{
  debugKeys.insert(string(key));
}

void _print_debug(const char *key, const char *fmt, ...)
{
  va_list args;
  char buf[8192];
  if(!is_debug_on(key))
	return;
  
  va_start(args, fmt);
  vsnprintf(buf, sizeof(buf)-1, fmt, args);
  va_end(args);
  buf[8191] = 0;

  cerr << "++ " << string(key) << ": " << string(buf) << endl;
  cerr.flush();
}

void fatal(const char *funcname, const char *fmt, ...)
{
  va_list args;
  char buf[1024];

  va_start(args, fmt);
  vsnprintf(buf, sizeof(buf)-1, fmt, args);
  va_end(args);
  buf[1023] = 0;
  cerr << "fatal err in " << string(funcname) << ": ";
  cerr << string(buf) << endl;
  cerr.flush();
  exit(-1);
}

