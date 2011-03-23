/*
 *  This file is part of libcxxsupport.
 *
 *  libcxxsupport is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  libcxxsupport is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with libcxxsupport; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

/*
 *  libcxxsupport is being developed at the Max-Planck-Institut fuer Astrophysik
 *  and financially supported by the Deutsches Zentrum fuer Luft- und Raumfahrt
 *  (DLR).
 */

/*
 *  This file contains functionality related to wall-clock timers
 *
 *  Copyright (C) 2010, 2011 Max-Planck-Society
 *  Author: Martin Reinecke
 */

#include <iostream>
#include <utility>
#include <cstdio>
#include <cmath>
#include <algorithm>
#include "walltimer.h"
#include "walltime_c.h"
#include "error_handling.h"

using namespace std;

void wallTimer::start()
  { start(wallTime()); }
void wallTimer::stop()
  { stop(wallTime()); }
double wallTimer::acc() const
  { return acc(wallTime()); }

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
void wallTimerSet::start(const string &name)
  { start(getIndex(name)); }
void wallTimerSet::stop(const string &name)
  { stop(getIndex(name)); }
void wallTimerSet::stopstart(const string &name1, const string &name2)
  { stopstart(getIndex(name1),getIndex(name2)); }
void wallTimerSet::reset(const string &name)
  { reset(getIndex(name)); }
double wallTimerSet::acc(const string &name)
  { return acc(getIndex(name)); }

void wallTimerSet::report() const
  {
  cout << "\nWall clock timer report:" << endl;
  for (map<string,int>::const_iterator it=lut.begin(); it!=lut.end(); ++it)
    printf("  %-15s: %10.5fs\n", it->first.c_str(), timer[it->second].acc());
  cout << "End wall clock timer report\n" << endl;
  }

wallTimerSet wallTimers;

namespace {

struct tstack_entry
  {
  string fullname, name;
  };

vector<tstack_entry> tstack;
wallTimerSet stackTimers;

struct timecomp
  {
  bool operator() (const pair<string,double> &a, const pair<string,double> &b)
    const
    { return a.second>b.second; }
  };

void tstack_report(const string &stem, const string &indent, int twidth, int slen)
  {
  double total=stackTimers.acc(stem);
  unsigned int lstem=stem.size();
  vector<pair<string,double> > tmp;
  const map<string,int> &tbl (stackTimers.table());
  for (map<string,int>::const_iterator it=tbl.begin(); it!=tbl.end(); ++it)
    if (it->first.find(stem)==0) // begins with the stem
      if (it->first.substr(lstem,string::npos).rfind("$")==0)
        tmp.push_back(make_pair(it->first,stackTimers.acc(it->second)));
  if (tmp.size()>0)
    {
    sort(tmp.begin(),tmp.end(),timecomp());
    double tsum=0;
    printf("%s|\n", indent.c_str());
    for (unsigned i=0; i<tmp.size(); ++i)
      {
      printf("%s+- %-*s:%6.2f%% (%*.4fs)\n",indent.c_str(),slen,
        tmp[i].first.substr(lstem+1,string::npos).c_str(),
        100*tmp[i].second/total,twidth,tmp[i].second);
      tstack_report(tmp[i].first,indent+"|  ",twidth,slen);
      tsum+=tmp[i].second;
      }
    printf("%s+- %-*s:%6.2f%% (%*.4fs)\n%s\n",indent.c_str(),slen,
      "<unaccounted>",100*(total-tsum)/total,twidth,total-tsum,indent.c_str());
    }
  }

} // unnamed namespace

void tstack_push(const string &name)
  {
  tstack_entry entry;
  entry.name=name;
  entry.fullname=tstack.empty() ? string("$")+name :
    tstack.back().fullname + "$" + name;
  tstack.push_back(entry);
  stackTimers.start(entry.fullname);
  }
void tstack_pop(const string &name)
  {
  planck_assert((!tstack.empty()) && (tstack.back().name==name),
    "incorrect tstack operation");
  stackTimers.stop(tstack.back().fullname);
  tstack.pop_back();
  }
void tstack_report(const string &stem)
  {
  double total=stackTimers.acc(string("$")+stem);
  printf("\nTotal wall clock time for '%s': %1.4fs\n",stem.c_str(),total);

  int slen=string("<unaccounted>").size();
  const map<string,int> &tbl (stackTimers.table());
  for (map<string,int>::const_iterator it=tbl.begin(); it!=tbl.end(); ++it)
    slen=max(slen,int(it->first.size()-it->first.rfind("$")));
  int logtime=max(1,int(log10(total)+1));
  tstack_report(string("$")+stem,"",logtime+5,slen);
  }
