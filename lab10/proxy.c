/*
 * proxy.c - ICS Web proxy
 *
 *
 */

#include "csapp.h"
#include <stdarg.h>
#include <sys/select.h>

#define MIN(a,b)((a)<(b)?(a):(b))
typedef struct {
    int id;
    int browser_fd;
    struct sockaddr_in sockaddr;
} do_args;

static sem_t mutex;

/*
 * Function prototypes
 */
int parse_uri(char *uri, char *target_addr, char *path, char *port);
void format_log_entry(char *logstring, struct sockaddr_in *sockaddr, char *uri, size_t size);
void* doit(void* argp);
int do_get(int browser_fd, rio_t rio_s);
int do_post(int browser_fd, int server_fd, rio_t rio_b, rio_t rio_s, int req_size);
ssize_t Rio_readlineb_w(rio_t *rp, void *usrbuf, size_t maxlen);
ssize_t Rio_writen_w(int fd, void *usrbuf, size_t n);
ssize_t Rio_readn_w(int fd, void *ptr, size_t nbytes);
ssize_t Rio_readnb_w(rio_t *rp, void *usrbuf, size_t n);

/*
 * read and write wrapper
 */
ssize_t Rio_readlineb_w(rio_t *rp, void *usrbuf, size_t maxlen) 
{
    ssize_t rc;

    rc = rio_readlineb(rp, usrbuf, maxlen);
    
    if (rc < 0){
        rc = 0;
        //printf("read error\n");
    }
	
    return rc;
} 

ssize_t Rio_readn_w(int fd, void *ptr, size_t nbytes) 
{
    ssize_t n;
  
    if ((n = rio_readn(fd, ptr, nbytes)) < 0){
        //printf("Rio_readn error\n");
        n = 0;
    }
    return n;
}
ssize_t Rio_readnb_w(rio_t *rp, void *usrbuf, size_t n) 
{
    ssize_t rc;

    if ((rc = rio_readnb(rp, usrbuf, n)) < 0){
	    //printf("Rio_readnb error");
        rc = 0;
    }
    return rc;
}
ssize_t Rio_writen_w(int fd, void *usrbuf, size_t n) 
{
    ssize_t wc;
    if ((wc = rio_writen(fd, usrbuf, n)) != n){
        wc = 0;
        //printf("write error\n");
    }
    return wc;
}

/*
 * main - Main routine for the proxy program
 */
int main(int argc, char **argv)
{
    /* Check arguments */
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <port number>\n", argv[0]);    
        exit(0);
    }

    Signal(SIGPIPE, SIG_IGN);

    int listenfd, connfd;    
    struct sockaddr_in browser_addr;
    //char browser_host[MAXLINE],browser_port[MAXLINE];
    socklen_t len;

    listenfd = Open_listenfd(argv[1]);
    len = sizeof(browser_addr);

    do_args* argp;
    pthread_t tid;
    int id = 0;
    Sem_init(&mutex, 0, 1);
    
    while(1){
        connfd = Accept(listenfd,(SA*)&browser_addr,&len);
        //Getnameinfo((SA*)&browser_addr,len,browser_host,MAXLINE,browser_port,MAXLINE,0);
        
        argp = Malloc(sizeof(do_args));
        argp->browser_fd = connfd;
        argp->sockaddr = browser_addr;
        argp->id = id++;

        Pthread_create(&tid, NULL, doit, argp);      
        
    }

    Close(listenfd);

    exit(0);
}

void* doit(void* argp){
    Pthread_detach(pthread_self());

    do_args args = *((do_args*)argp);
    int browser_fd = args.browser_fd;
    struct sockaddr_in browser_addr = args.sockaddr;
    Free(argp);

    char buf_b[MAXLINE], buf_s[MAXLINE];
    char method[MAXLINE], uri[MAXLINE], version[MAXLINE];
    char header[MAXLINE];
    rio_t rio_b, rio_s;
    int server_fd;

    memset(buf_b,0,MAXLINE);
    memset(buf_s,0,MAXLINE);
    memset(header,0,MAXLINE);
    memset(method,0,MAXLINE);
    memset(uri,0,MAXLINE);
    memset(version,0,MAXLINE);

    /* Read and parse request line */
    Rio_readinitb(&rio_b,browser_fd);
    if(Rio_readlineb_w(&rio_b,buf_b,MAXLINE) == 0){
        Close(browser_fd);
        return NULL;
    }    
    //printf("\n1: %s",buf_b);
    sscanf(buf_b,"%s %s %s",method,uri,version);

    /* Parse the name of end server */
    char serv_host[MAXLINE], serv_port[MAXLINE], serv_path[MAXLINE];
    memset(serv_host,0,MAXLINE);
    memset(serv_port,0,MAXLINE);
    memset(serv_path,0,MAXLINE);
    if(parse_uri(uri,serv_host,serv_path,serv_port)<0){
        //printf("Bad URI: %s\n",uri);
        Close(browser_fd);
        return NULL;
    }

    /* Connect to end server */ 
    server_fd = open_clientfd(serv_host,serv_port);
    if(server_fd < 0){
        printf("BAD host [%s :%s]\n",serv_host,serv_port);
        Close(browser_fd);
        return NULL;
    }
    Rio_readinitb(&rio_s,server_fd);

    /* Send request header line */
    sprintf(buf_s,"%s /%s %s\r\n",method,serv_path,version);
    if(Rio_writen_w(server_fd,buf_s,strlen(buf_s))== 0){ 
        Close(browser_fd);
        Close(server_fd);
        return NULL;
    }

    /* Read header*/
    int req_size;    
    if(Rio_readlineb_w(&rio_b,buf_b,MAXLINE) == 0){
        Close(browser_fd);
        Close(server_fd);
        return NULL;
    }
    //printf("2: %s",buf_b);
    while(strcmp(buf_b,"\r\n")){
        sprintf(header,"%s%s",header,buf_b);
        if(strstr(buf_b,"Length")){
            if(sscanf(buf_b,"Content-Length: %d",&req_size) != 1){
                Close(browser_fd);
                Close(server_fd);
                return NULL;            
            }  
            //printf("length: %d\n",req_size);          
        }
        if(Rio_readlineb_w(&rio_b,buf_b,MAXLINE) == 0){
            Close(browser_fd);
            Close(server_fd);
            return NULL;
        }
        //printf("3: %s",buf_b);
    }
    sprintf(header,"%s%s",header,buf_b);
    //printf("4: req over\n");

    /* Constuct and send new request */
    //printf("header:\n%s",header);
    if(Rio_writen_w(server_fd,header,strlen(header))== 0){ //send request to server
        Close(browser_fd);
        Close(server_fd);
        return NULL;
    }

    /* Deal with GET or POST*/
    int cnt;
    if(!strcasecmp(method, "POST")){
        if(req_size <= 0){
            Close(server_fd);
            Close(browser_fd);
            //printf("----------------ERROR0--------------------\n");
            return NULL;
        }
        if(do_post(browser_fd, server_fd, rio_b, rio_s, req_size) < 0){
            Close(server_fd);
            Close(browser_fd);
            //printf("----------------ERROR1--------------------\n");
            return NULL;
        }
        cnt = do_get(browser_fd, rio_s);
    }
    else if(!strcasecmp(method, "GET")){
        cnt = do_get(browser_fd, rio_s);
    }
    else{
        //printf("METHOD error\n");
        return NULL;
    }

    if(cnt < 0){
        Close(server_fd);
        Close(browser_fd);
        //printf("----------------ERROR2--------------------\n");
        return NULL;
    }


    Close(server_fd);
    Close(browser_fd);

    /* Log */
    P(&mutex);

    char logstring[MAXLINE];
    format_log_entry(logstring,&browser_addr,uri,(size_t)cnt);    
    fprintf(stderr,"%s\n",logstring);  
    fflush(stderr);

    V(&mutex);

    return NULL;       
    
}

int do_get(int browser_fd, rio_t rio_s){
    /* Get response */ 
    int resp_size, read_size;
    int n;
    int cnt = 0; 
    char buf_s[MAXLINE];

    memset(buf_s,0,MAXLINE);

    /* Read response header */
    while((n=Rio_readlineb_w(&rio_s,buf_s,MAXLINE)) != 0){
        cnt += n;
        if(Rio_writen_w(browser_fd,buf_s,n) != n){
            return -1;
        };
        if(strstr(buf_s,"Length")){
            char* p = strchr(buf_s,' ');
            p++;
            resp_size = atoi(p);
        }
        if(!strcmp(buf_s,"\r\n")){
            break;
        }
    }
    if(n == 0){
        return -1;
    }

    /* Read reponse body */
    if(resp_size >= 1024*1024){  /* Big file */      
        while(resp_size){
            if((n = Rio_readnb_w(&rio_s,buf_s,1)) == 0){
                return -1;
            }
            cnt += n;  
            if(Rio_writen_w(browser_fd,buf_s,n) != n){
                return -1;
            }; 
            resp_size -= n;
        }
        return cnt;
    }

    read_size = MIN(MAXLINE, resp_size);
    while(read_size){
        if((n = Rio_readnb_w(&rio_s,buf_s,read_size)) == 0){
            return -1;
        }
        cnt += n; 
        if(Rio_writen_w(browser_fd,buf_s,n) != n){
            return -1;
        }; 
        resp_size -= n;
        read_size = MIN(MAXLINE, resp_size);
    }
    return cnt;

}

int do_post(int browser_fd, int server_fd, rio_t rio_b, rio_t rio_s, int req_size)
{
    int n, read_size;
    int cnt = 0;
    char buf_b[MAXLINE];

    memset(buf_b,0,MAXLINE);

    /* Read content */
    read_size = MIN(MAXLINE, req_size);
    while(read_size){        
        if((n = Rio_readnb_w(&rio_b,buf_b,read_size)) == 0){
            return -1;
        }        
        cnt += n;
        
        if(Rio_writen_w(server_fd,buf_b,n) != n){
            return -1;
        };
        req_size -= n;
        read_size = MIN(MAXLINE, req_size);
    }

    return cnt;

}

/*
 * parse_uri - URI parser
 *
 * Given a URI from an HTTP proxy GET request (i.e., a URL), extract
 * the host name, path name, and port.  The memory for hostname and
 * pathname must already be allocated and should be at least MAXLINE
 * bytes. Return -1 if there are any problems.
 */
int parse_uri(char *uri, char *hostname, char *pathname, char *port)
{
    char *hostbegin;
    char *hostend;
    char *pathbegin;
    int len;

    if (strncasecmp(uri, "http://", 7) != 0) {
        hostname[0] = '\0';
        return -1;
    }

    /* Extract the host name */
    hostbegin = uri + 7;
    hostend = strpbrk(hostbegin, " :/\r\n\0");
    if (hostend == NULL)
        return -1;
    len = hostend - hostbegin;
    strncpy(hostname, hostbegin, len);
    hostname[len] = '\0';

    /* Extract the port number */
    if (*hostend == ':') {
        char *p = hostend + 1;
        while (isdigit(*p))
            *port++ = *p++;
        *port = '\0';
    } else {
        strcpy(port, "80");
    }

    /* Extract the path */
    pathbegin = strchr(hostbegin, '/');
    if (pathbegin == NULL) {
        pathname[0] = '\0';
    }
    else {
        pathbegin++;
        strcpy(pathname, pathbegin);
    }

    return 0;
}

/*
 * format_log_entry - Create a formatted log entry in logstring.
 *
 * The inputs are the socket address of the requesting client
 * (sockaddr), the URI from the request (uri), the number of bytes
 * from the server (size).
 */
void format_log_entry(char *logstring, struct sockaddr_in *sockaddr,
                      char *uri, size_t size)
{
    time_t now;
    char time_str[MAXLINE];
    unsigned long host;
    unsigned char a, b, c, d;

    /* Get a formatted time string */
    now = time(NULL);
    strftime(time_str, MAXLINE, "%a %d %b %Y %H:%M:%S %Z", localtime(&now));

    /*
     * Convert the IP address in network byte order to dotted decimal
     * form. Note that we could have used inet_ntoa, but chose not to
     * because inet_ntoa is a Class 3 thread unsafe function that
     * returns a pointer to a static variable (Ch 12, CS:APP).
     */
    host = ntohl(sockaddr->sin_addr.s_addr);
    a = host >> 24;
    b = (host >> 16) & 0xff;
    c = (host >> 8) & 0xff;
    d = host & 0xff;

    /* Return the formatted log entry string */
    sprintf(logstring, "%s: %d.%d.%d.%d %s %zu", time_str, a, b, c, d, uri, size);
}


