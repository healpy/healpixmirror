#ifndef PLANCK_WALLTIMER_H
#define PLANCK_WALLTIMER_H

#include <string>
#include <map>
#include <vector>

class wallTimer
  {
  private:
    double t_acc, t_started;
    bool running;

  public:
    wallTimer() : t_acc(0.), t_started(0.), running(false) {}
    void start(double wtime_now)
      { t_started=wtime_now; running=true; }
    void start();
    void stop(double wtime_now)
      { if (running) t_acc+=wtime_now-t_started; running=false; }
    void stop();
    void reset() { t_acc=t_started=0.; running=false;}
    double acc() const;
  };

class wallTimerSet
  {
  private:
    std::map<std::string,int> lut;
    std::vector<wallTimer> timer;

  public:
    int getIndex(const std::string &name);
    void start(int index);
    void stop(int index);
    void stopstart(int index1, int index2);
    void reset(int index);
    double acc(int index);
    void start(const std::string &name)
      { start(getIndex(name)); }
    void stop(const std::string &name)
      { stop(getIndex(name)); }
    void reset(const std::string &name)
      { reset(getIndex(name)); }
    double acc(const std::string &name)
      { return acc(getIndex(name)); }

    void report() const;
  };

extern wallTimerSet wallTimers;

#endif
