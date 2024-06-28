#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <asm/unistd.h>
#include <sys/prctl.h>
#include <sys/ioctl.h>

#include <linux/perf_event.h>
#include <linux/hw_breakpoint.h>

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

static long
perf_event_open(struct perf_event_attr *hw_event, pid_t pid,
		int cpu, int group_fd, unsigned long flags)
{
  int ret;
  ret = syscall(__NR_perf_event_open, hw_event, pid, cpu,
		group_fd, flags);
  return ret;
}

CAMLprim value mperf_events_enable_all (value unit)
{
  CAMLparam1(unit);
  int ret;
  ret = prctl(PR_TASK_PERF_EVENTS_ENABLE, 0, 0, 0, 0);

  if(ret == -1)
    uerror(__func__, Nothing);

  CAMLreturn(Val_unit);
}

CAMLprim value mperf_events_disable_all (value unit)
{
  CAMLparam1(unit);
  int ret;
  ret = prctl(PR_TASK_PERF_EVENTS_DISABLE, 0, 0, 0, 0);

  if(ret == -1)
    uerror(__func__, Nothing);

  CAMLreturn(Val_unit);
}

CAMLprim value mperf_event_ioc_enable (value fd)
{
  CAMLparam1(fd);
  int ret;
  ret = ioctl(Int_val(fd), PERF_EVENT_IOC_ENABLE);

  if(ret == -1)
    uerror(__func__, Nothing);

  CAMLreturn(Val_unit);
}

CAMLprim value mperf_event_ioc_disable (value fd)
{
  CAMLparam1(fd);
  int ret;
  ret = ioctl(Int_val(fd), PERF_EVENT_IOC_DISABLE);

  if(ret == -1)
    uerror(__func__, Nothing);

  CAMLreturn(Val_unit);
}

CAMLprim value mperf_event_ioc_reset (value fd)
{
  CAMLparam1(fd);
  int ret;
  ret = ioctl(Int_val(fd), PERF_EVENT_IOC_RESET);

  if(ret == -1)
    uerror(__func__, Nothing);

  CAMLreturn(Val_unit);
}

CAMLprim value mperf_event_open_native (value kind, value attr_flags,
				     value pid, value cpu, value group_fd, value flags)
{
  CAMLparam5(kind, attr_flags, pid, cpu, group_fd);
  CAMLxparam1(flags);

  int ret;
  int c_flags = 0;

  struct perf_event_attr attr;
  memset(&attr, 0, sizeof(struct perf_event_attr));
  attr.size = sizeof(struct perf_event_attr);
#ifdef PERF_FLAG_FD_CLOEXEC
  if (Int_val(flags) & 1) c_flags += PERF_FLAG_FD_CLOEXEC;
#endif
  if (Int_val(flags) & 2) c_flags += PERF_FLAG_FD_NO_GROUP;
  if (Int_val(flags) & 4) c_flags += PERF_FLAG_FD_OUTPUT;
  if (Int_val(flags) & 8) c_flags += PERF_FLAG_PID_CGROUP;

  if (Int_val(kind) < 10) attr.type = PERF_TYPE_HARDWARE;
  else attr.type = PERF_TYPE_SOFTWARE;

  switch (Int_val(kind))
    {
    case 0: attr.config = PERF_COUNT_HW_CPU_CYCLES; break;
    case 1: attr.config = PERF_COUNT_HW_INSTRUCTIONS; break;
    case 2: attr.config = PERF_COUNT_HW_CACHE_REFERENCES; break;
    case 3: attr.config = PERF_COUNT_HW_CACHE_MISSES; break;
    case 4: attr.config = PERF_COUNT_HW_BRANCH_INSTRUCTIONS; break;
    case 5: attr.config = PERF_COUNT_HW_BRANCH_MISSES; break;
    case 6: attr.config = PERF_COUNT_HW_BUS_CYCLES; break;
    case 7: attr.config = PERF_COUNT_HW_STALLED_CYCLES_FRONTEND; break;
    case 8: attr.config = PERF_COUNT_HW_STALLED_CYCLES_BACKEND; break;
    case 9: attr.config = PERF_COUNT_HW_REF_CPU_CYCLES; break;
    case 10: attr.config = PERF_COUNT_SW_CPU_CLOCK; break;
    case 11: attr.config = PERF_COUNT_SW_TASK_CLOCK; break;
    case 12: attr.config = PERF_COUNT_SW_PAGE_FAULTS; break;
    case 13: attr.config = PERF_COUNT_SW_CONTEXT_SWITCHES; break;
    case 14: attr.config = PERF_COUNT_SW_CPU_MIGRATIONS; break;
    case 15: attr.config = PERF_COUNT_SW_PAGE_FAULTS_MIN; break;
    case 16: attr.config = PERF_COUNT_SW_PAGE_FAULTS_MAJ; break;
    case 17: attr.config = PERF_COUNT_SW_ALIGNMENT_FAULTS; break;
    case 18: attr.config = PERF_COUNT_SW_EMULATION_FAULTS; break;
    case 19: attr.config = PERF_COUNT_SW_DUMMY; break;
    }

  if(Int_val(attr_flags) & 1) attr.disabled = 1;
  if(Int_val(attr_flags) & 2) attr.inherit = 1;
  if(Int_val(attr_flags) & 4) attr.exclude_user = 1;
  if(Int_val(attr_flags) & 8) attr.exclude_kernel = 1;
  if(Int_val(attr_flags) & 16) attr.exclude_hv = 1;
  if(Int_val(attr_flags) & 32) attr.exclude_idle = 1;
  if(Int_val(attr_flags) & 64) attr.enable_on_exec = 1;

  ret = perf_event_open(&attr, Int_val(pid), Int_val(cpu), Int_val(group_fd), c_flags);

  if(ret == -1)
    uerror(__func__, Nothing);

  CAMLreturn(Val_int(ret));
}

CAMLprim value mperf_event_open_byte (value *argv, int argn)
{
  return mperf_event_open_native(argv[0], argv[1], argv[2],
			      argv[3], argv[4], argv[5]);
}

