#include <iostream>
#include <cstdio>
#include "walltimer.h"
#include "walltime_c.h"

using namespace std;

void wallTimer::start()
  { t_started=wallTime(); running=true; }
void wallTimer::stop()
  { if (running) t_acc+=wallTime()-t_started; running=false; }
double wallTimer::acc() const
  { return running ? t_acc+wallTime()-t_started : t_acc; }

int wallTimerSet::getIndex(const string &name)
  {
  map<std::string,int>::iterator it = lut.find(name);
  if (it!=lut.end())
    return it->second;
  timer.push_back(wallTimer());
  lut[name]=timer.size()-1;
  return timer.size()-1;
  }

void wallTimerSet::start(int index)
  { timer[index].start(); }
void wallTimerSet::stop(int index)
  { timer[index].stop(); }
void wallTimerSet::stopstart(int index1, int index2)
  { double t=wallTime(); timer[index1].stop(t); timer[index2].start(t); }
void wallTimerSet::reset(int index)
  { timer[index].reset(); }
double wallTimerSet::acc(int index)
  { return timer[index].acc(); }

void wallTimerSet::report() const
  {
  cout << "\nWall clock timer report:" << endl;
  for (map<string,int>::const_iterator it=lut.begin(); it!=lut.end(); ++it)
    printf("  %-15s: %10.5fs\n", it->first.c_str(), timer[it->second].acc());
  cout << "End wall clock timer report\n" << endl;
  }

wallTimerSet wallTimers;
