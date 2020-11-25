/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#if !defined(LWT_ON_WINDOWS)

#include <caml/mlvalues.h>
#include <sys/uio.h>

#include "unix_recv_send_utils.h"
#include "unix_readv_writev_utils.h"

CAMLprim value lwt_unix_send_msg(value val_fd, value val_n_iovs, value val_iovs,
                                 value val_n_fds, value val_fds, value val_dest)
{
    int n_iovs = Int_val(val_n_iovs);
    struct iovec iovs[n_iovs];
    flatten_io_vectors(iovs, val_iovs, n_iovs, NULL, NULL);
    return wrapper_send_msg(Int_val(val_fd), n_iovs, iovs, val_n_fds, val_fds, val_dest);
}
#endif
