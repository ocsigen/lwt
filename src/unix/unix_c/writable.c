#include "lwt_config.h"
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/version.h>

#if defined(LWT_ON_WINDOWS)
LWT_NOT_AVAILABLE1(unix_writable)
#else
#include <dirent.h>
#include <poll.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <unistd.h>

CAMLprim value lwt_unix_writable(value fd)
{
    struct pollfd pollfd;
    pollfd.fd = Int_val(fd);
    pollfd.events = POLLOUT;
    pollfd.revents = 0;
    if (poll(&pollfd, 1, 0) < 0) uerror("writable", Nothing);
    return (Val_bool(pollfd.revents & POLLOUT));
}
#endif
