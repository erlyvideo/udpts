#include <erl_driver.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <errno.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <sys/types.h>



const int MPEGTS_SIZE = 188;
const int BUFFER_SIZE = 12000;
const int LIMIT_SIZE = 8000;

typedef struct {
  ErlDrvPort port;
  int socket;
  char *buf;
  ssize_t size;
  ssize_t len;
} Udpts;

static ErlDrvData udpts_drv_start(ErlDrvPort port, char *buff)
{
    Udpts* d = (Udpts *)driver_alloc(sizeof(Udpts));
    d->port = port;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    d->size = BUFFER_SIZE;
    d->buf = (char *)malloc(d->size);
    d->len = 0;
    return (ErlDrvData)d;
}


static void udpts_drv_stop(ErlDrvData handle)
{
    driver_free((char*)handle);
}


static void udpts_drv_output(ErlDrvData handle, char *buff, int bufflen)
{
    Udpts* d = (Udpts *)handle;
    char fn = buff[0], arg = buff[1], res;
    res = 5;
    driver_output(d->port, &res, 1);
}

static int udpts_drv_command(ErlDrvData handle, unsigned int command, char *buf, 
                   int len, char **rbuf, int rlen) {
  Udpts* d = (Udpts*) handle;
  
  switch(command) {
    case 1: {
      int sock;
      int port;
      struct sockaddr_in si;
      port = atoi(buf);
      // fprintf(stderr, "Connecting to port %d\r\n", port);
      sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
      
      bzero(&si, sizeof(si));
      si.sin_family = AF_INET;
      si.sin_port = htons(port);
      si.sin_addr.s_addr = htonl(INADDR_ANY);
      if(bind(sock, (struct sockaddr *)&si, sizeof(si)) == -1) {
        driver_failure_posix(d->port, errno);
        return 0;
        // memcpy(*rbuf, "error", 5);
        // return 5;
      }
      d->socket = sock;
      driver_select(d->port, (ErlDrvEvent)sock, DO_READ, 1);
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    break;
    default:
    return 0;
  }
  return 0;
}


static void udpts_drv_input(ErlDrvData handle, ErlDrvEvent event)
{
  Udpts* d = (Udpts*) handle;
  struct sockaddr_in peer;
  socklen_t peer_len;
  ssize_t s;
  
  s = recvfrom(d->socket, d->buf + d->len, d->size - d->len, 0, (struct sockaddr *)&peer, &peer_len);
  // fprintf(stderr, "Select: %lu\r\n", s);
  d->len += s;
  if(d->len > LIMIT_SIZE) {
    driver_output(d->port, d->buf, d->len);
    d->len = 0;
  }
}


ErlDrvEntry udpts_driver_entry = {
    NULL,			/* F_PTR init, N/A */
    udpts_drv_start,		/* L_PTR start, called when port is opened */
    udpts_drv_stop,		/* F_PTR stop, called when port is closed */
    udpts_drv_output,		/* F_PTR output, called when erlang has sent */
    udpts_drv_input,			/* F_PTR ready_input, called when input descriptor ready */
    NULL,			/* F_PTR ready_output, called when output descriptor ready */
    "udpts_drv",		/* char *driver_name, the argument to open_port */
    NULL,			/* F_PTR finish, called when unloaded */
    NULL,     /* void *handle */
    udpts_drv_command,			/* F_PTR control, port_command callback */
    NULL,			/* F_PTR timeout, reserved */
    NULL,			/* F_PTR outputv, reserved */
    NULL,                      /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING,     /* ERL_DRV_FLAGs */
    NULL,     /* void *handle2 */
    NULL,     /* process_exit */
    NULL      /* stop_select */
};
DRIVER_INIT(udpts_drv) /* must match name in driver_entry */
{
    return &udpts_driver_entry;
}
