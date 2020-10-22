#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include <uv.h>

CAMLprim value luv_unix_fd_to_os_fd(value unix_fd, value os_fd_storage)
{
    uv_os_fd_t *os_fd = (uv_os_fd_t *)Nativeint_val(os_fd_storage);

#ifndef _WIN32
    *os_fd = Int_val(unix_fd);
#else
    if (Descr_kind_val(unix_fd) == KIND_HANDLE)
        *os_fd = Handle_val(unix_fd);
    else
        *os_fd = -1;
#endif

    return Val_unit;
}
