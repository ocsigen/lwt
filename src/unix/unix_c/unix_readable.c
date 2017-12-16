#include "lwt_config.h"

#if !defined(LWT_ON_WINDOWS)
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/version.h>

#include <dirent.h>
#include <poll.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <unistd.h>
#include "lwt_unix.h"

CAMLprim value lwt_unix_readable(value fd)
{
    struct pollfd pollfd;
    pollfd.fd = Int_val(fd);
    pollfd.events = POLLIN;
    pollfd.revents = 0;
    if (poll(&pollfd, 1, 0) < 0) uerror("readable", Nothing);
    return (Val_bool(pollfd.revents & POLLIN));
}
#endif
