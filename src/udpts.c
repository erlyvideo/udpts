#include <erl_driver.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <errno.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <assert.h>


const int MPEGTS_SIZE = 188;
const int BUFFER_SIZE = 12000;
const int LIMIT_SIZE = 1300;
#define PID_COUNT 8192

typedef struct {
  ErlDrvPort port;
  int socket;
  uint8_t *buf;
  ssize_t size;
  ssize_t len;
  uint8_t counters[PID_COUNT];
  uint32_t error_count;
} Udpts;

static ErlDrvData udpts_drv_start(ErlDrvPort port, char *buff)
{
    Udpts* d = (Udpts *)driver_alloc(sizeof(Udpts));
    bzero(d, sizeof(Udpts));
    memset(d->counters, 0xFF, PID_COUNT);
    d->port = port;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    d->size = BUFFER_SIZE;
    d->buf = (char *)malloc(d->size);
    d->len = 0;
    return (ErlDrvData)d;
}


static void udpts_drv_stop(ErlDrvData handle)
{
  Udpts* d = (Udpts *)handle;
  driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ, 0);
  close(d->socket);
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
      int flags;
      struct sockaddr_in si;
      uint16_t port;
      if(len < 2) return 0;
      memcpy(&port, buf, 2);
      // fprintf(stderr, "Connecting to port %d\r\n", port);
      sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
      
      bzero(&si, sizeof(si));
      si.sin_family = AF_INET;
      si.sin_port = port;
      si.sin_addr.s_addr = htonl(INADDR_ANY);
      if(bind(sock, (struct sockaddr *)&si, sizeof(si)) == -1) {
        driver_failure_posix(d->port, errno);
        return 0;
        // memcpy(*rbuf, "error", 5);
        // return 5;
      }
      
      if(len >= 6) {
        struct ip_mreq mreq;
        memcpy(&mreq.imr_multiaddr.s_addr, buf+2, 4);
        mreq.imr_interface.s_addr = htonl(INADDR_ANY);

        if (setsockopt(sock, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof(mreq)) < 0) {
          perror("multicast join error\n");
          driver_failure_posix(d->port, errno);
          return 0;
        }
      }

      d->socket = sock;
      flags = fcntl(d->socket, F_GETFL);
      assert(flags >= 0);
      assert(!fcntl(d->socket, F_SETFL, flags | O_NONBLOCK));
      driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ, 1);
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    case 2: {
      memcpy(*rbuf, &d->error_count, 4);
      d->error_count = 0;
      return 4;
    }
    break;
    default:
    return 0;
  }
  return 0;
}


static void check_errors(Udpts *d) 
{
  uint8_t *packet;
  assert(d->len % 188 == 0);
  for(packet = d->buf; packet < d->buf + d->len; packet += 188) {
    if(packet[0] == 0x47) {
      uint16_t pid = packet[1] & 0x1F << 8 | packet[2];
      uint8_t counter = packet[3] & 0x0F;
      // fprintf(stderr, "%d,%d,%d,%d:  %5d %2d\r\n", packet[0], packet[1], packet[2], packet[3], pid, counter);
      // fprintf(stderr, "Pid: %5d %2d\r\n", pid, counter);
      if(d->counters[pid] != 0xFF && d->counters[pid] != counter) {
        // fprintf(stderr, "Pid: %5d %2d %2d\r\n", pid, d->counters[pid], counter);
        d->error_count++;
      }
      d->counters[pid] = (counter + 1) % 0x10; 
    }
  }
}

static void udpts_drv_input(ErlDrvData handle, ErlDrvEvent event)
{
  Udpts* d = (Udpts*) handle;
  struct sockaddr_in peer;
  socklen_t peer_len;
  ssize_t s;
  uint8_t *packet;
  
  while((s = recvfrom(d->socket, d->buf + d->len, d->size - d->len, 0, (struct sockaddr *)&peer, &peer_len)) > 0) {
    d->len += s;
    if(d->len > LIMIT_SIZE) {
      check_errors(d);
      driver_output(d->port, d->buf, d->len);
      d->len = 0;
    }
  }
  if(errno != EAGAIN) {
    driver_failure_posix(d->port, errno);    
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
    ERL_DRV_EXTENDED_MINOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING,     /* ERL_DRV_FLAGs */
    NULL,     /* void *handle2 */
    NULL,     /* process_exit */
    NULL      /* stop_select */
};
DRIVER_INIT(udpts_drv) /* must match name in driver_entry */
{
    return &udpts_driver_entry;
}
