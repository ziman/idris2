#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <stdio.h>
#include <string.h>

#include "getline.h"
#include "idris_buffer.h"
#include "idris_directory.h"
#include "idris_file.h"
#include "idris_net.h"
#include "idris_support.h"

#include "sys/stat.h"

CAMLprim value c_hello(value i) {
	CAMLparam0();
	const char * const msg = "hello from C!";
	printf("this is C; we received %d from OCaml\n", Int_val(i));
	CAMLreturn(caml_alloc_initialized_string(strlen(msg), msg));
}

// apparently this could be done using the OCaml Obj module from the stdlib
// but this is already written so let's keep it
void inspect_(int indent, value x) {
	for (int i = 0; i < indent; ++i) printf("  ");
	if (Is_block(x)) {
		switch (Tag_val(x)) {
			case Double_tag:
				printf("double: %f\n", Double_val(x));
				break;

			case String_tag:
				printf("string: %s\n", String_val(x));
				break;

			case Custom_tag:
				printf("custom tag\n");
				break;

			default:
				printf(
					"block(tag = %d, size = %d)\n",
					Tag_val(x),
					Wosize_val(x)
				);

				if (Tag_val(x) < 16) {
					// probably an ADT
					for (int i = 0; i < Wosize_val(x); ++i) {
						inspect_(indent+1, Field(x, i));
					}
				} else {
					for (int i = 0; i < indent+1; ++i) printf("  ");
					printf("(fields omitted because tag too high)\n");
				}
				break;
		}
	} else {
		printf("int %d\n", Int_val(x));
	}
}

CAMLprim value inspect(value ty, value x)
{
	CAMLparam2(ty, x);
	inspect_(0, x);
	CAMLreturn(Val_int(0));  // return unit
}

CAMLprim value idris_nil(value unit)
{
  CAMLparam1(unit);
  CAMLreturn (Val_int(0));
}

CAMLprim value idris_cons(value x, value xs)
{
  CAMLparam2 (x, xs);
  CAMLlocal1 (xxs);
  xxs = caml_alloc (2, 1);
  Store_field (xxs, 0, x);
  Store_field (xxs, 1, xs);
  CAMLreturn (xxs);
}

CAMLprim value ml_idris2_getStr()
{
	CAMLparam0();
	value result = caml_copy_string(idris2_getStr()); 
	CAMLreturn((value) result);
}

CAMLprim value ml_idris2_getString(value sptr)
{
	CAMLparam1(sptr);
	value result = caml_copy_string(idris2_getString((void *)sptr));
	CAMLreturn((value) result);
}

CAMLprim value ml_idris2_getEnvPair(value i)
{
	CAMLparam1(i);
	const char * result = idris2_getEnvPair(Int_val(i));
	CAMLreturn((value) result);
}

CAMLprim value ml_idris2_isNull(value ptr)
{
	CAMLparam1(ptr);
	const int result = idris2_isNull((void *) ptr);
	CAMLreturn(Val_int(result));
}




CAMLprim value ml_idris2_putStr(value s)
{
	CAMLparam1(s);
	idris2_putStr(String_val(s));
	CAMLreturn(Val_int(0));
}

CAMLprim value ml_idris2_openFile(value name, value mode) {
	CAMLparam2(name, mode);
	const FILE* result = idris2_openFile(String_val(name), String_val(mode));
	CAMLreturn((value) result);
}

CAMLprim value ml_idris2_closeFile(value file) {
  CAMLparam1(file);
  idris2_closeFile((FILE *) file);
  CAMLreturn(Val_int(0));
}

CAMLprim value ml_idris2_fileError(value file) {
  CAMLparam1(file);
  const int result = idris2_fileError((FILE *) file);
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_idris2_fileErrno()
{
	CAMLparam0();
	const int result = idris2_fileErrno();
	CAMLreturn(Val_int(result));
}

CAMLprim value ml_idris2_removeFile(value name) {
  CAMLparam1(name);
  const int result = idris2_removeFile((const char*)name);
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_idris2_fileSize(value file) {
  CAMLparam1(file);
  const int result = idris2_fileSize((FILE *) file);
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_idris2_fpoll(value file) {
  CAMLparam1(file);
  const int result = idris2_fpoll((FILE *) file);
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_idris2_readLine(value file) {
  CAMLparam1(file);
  char * result = idris2_readLine((FILE *) file);
  CAMLreturn((value) result);
}

CAMLprim value ml_idris2_readChars(value num, value file) {
  CAMLparam2(num, file);
  char * result = idris2_readChars(Int_val(num), (FILE *) file);
  CAMLreturn((value) result);
}

CAMLprim value ml_idris2_writeLine(value file, value str) {
  CAMLparam2(file, str);
  const int result = idris2_writeLine((FILE *) file, (char *)str);
  CAMLreturn((value) result);
}

CAMLprim value ml_idris2_eof(value file) {
  CAMLparam1(file);
  const int result = idris2_eof((FILE *)file);
  CAMLreturn((value) result);
}

CAMLprim value ml_idris2_fileAccessTime(value file) {
  CAMLparam1(file);
  const int result = idris2_fileAccessTime((FILE *)file);
  CAMLreturn((value) result);
}

CAMLprim value ml_idris2_fileModifiedTime(value file) {
  CAMLparam1(file);
  const int result = idris2_fileModifiedTime((FILE *)file);
  CAMLreturn((value) result);
}

CAMLprim value ml_idris2_fileStatusTime(value file) {
  CAMLparam1(file);
  const int result = idris2_fileStatusTime((FILE *)file);
  CAMLreturn((value) result);
}

CAMLprim value ml_idris2_stdin() {
  CAMLparam0();
  FILE* result = idris2_stdin();
  CAMLreturn((value) result);
}

CAMLprim value ml_idris2_stdout() {
  CAMLparam0();
  FILE* result = idris2_stdout();
  CAMLreturn((value) result);
}

CAMLprim value ml_idris2_stderr() {
  CAMLparam0();
  FILE* result = idris2_stderr();
  CAMLreturn((value) result);
}

CAMLprim value ml_idris2_currentDirectory() {
  CAMLparam0();
  value result = caml_copy_string(idris2_currentDirectory());
  CAMLreturn(result);
}

CAMLprim value ml_idris2_changeDir(value dir) {
  CAMLparam1(dir);
  const int result = idris2_changeDir(String_val(dir));
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_idris2_createDir(value dir) {
  CAMLparam1(dir);
  const int result = idris2_createDir(String_val(dir));
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_idris2_openDir(value dir) {
  CAMLparam1(dir);
  const void *result = idris2_openDir(String_val(dir));
  CAMLreturn((value) result);
}

CAMLprim value ml_idris2_closeDir(value dir) {
  CAMLparam1(dir);
  idris2_closeDir(String_val(dir));
  CAMLreturn(Val_int(0));
}

CAMLprim value ml_idris2_removeDir(value dir) {
  CAMLparam1(dir);
  const int result = idris2_removeDir(String_val(dir));
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_idris2_nextDirEntry(value dir) {
  CAMLparam1(dir);
  const value result = caml_copy_string(idris2_nextDirEntry((void *)dir));
  CAMLreturn(result);
}

/*  libc stuff  */

CAMLprim value ml_getenv(value s)
{
	CAMLparam1(s);
	const char * result = getenv(String_val(s));
	CAMLreturn((value) result);
}

CAMLprim value ml_system(value s)
{
	CAMLparam1(s);
	const int result = system(String_val(s));
	CAMLreturn(Val_int(result));
}

CAMLprim value ml_exit(value s)
{
	CAMLparam1(s);
	exit(Int_val(s));
	CAMLreturn(Val_int(0));
}

CAMLprim value ml_fflush(value file) {
  CAMLparam1(file);
  const int result = fflush((FILE *)file);
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_fdopen(value fd, value mode) {
  CAMLparam2(fd, mode);
  FILE * result = fdopen(Int_val(fd), String_val(mode));
  CAMLreturn((value) result);
}

CAMLprim value ml_chmod(value path, value mode) {
  CAMLparam2(path, mode);
  const int result = chmod((const char *)path, Int_val(mode));
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_putchar(value c) {
  CAMLparam1(c);
  const int result = putchar(Int_val(c));
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_getchar() {
  CAMLparam0();
  const int result = getchar();
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_strlen(value str) {
  CAMLparam1(str);
  size_t len = strlen(String_val(str));
  CAMLreturn(Val_int(len));
}

CAMLprim value ml_fgetc(value fptr) {
  CAMLparam1(fptr);
  CAMLreturn(Val_int(fgetc((FILE *)fptr)));
}

/* buffer stuff */

CAMLprim value ml_idris2_newBuffer(value size) {
  CAMLparam1(size);
  void * result = idris2_newBuffer(Int_val(size));
  CAMLreturn((value) result);
}

CAMLprim value ml_idris2_freeBuffer(value buffer) {
  CAMLparam1(buffer);
  idris2_freeBuffer((void *)buffer);
  CAMLreturn(Val_int(0));
}

CAMLprim value ml_idris2_getBufferSize(value buffer) {
  CAMLparam1(buffer);
  const int result = idris2_getBufferSize((void *)buffer);
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_idris2_setBufferByte(value buffer, value loc, value val) {
  CAMLparam3(buffer, loc, val);
  idris2_setBufferByte((void *)buffer, Int_val(loc), Int_val(val));
  CAMLreturn(Val_int(0));
}

CAMLprim value ml_idris2_setBufferInt(value buffer, value loc, value val) {
  CAMLparam3(buffer, loc, val);
  idris2_setBufferInt((void *)buffer, Int_val(loc), Int_val(val));
  CAMLreturn(Val_int(0));
}

CAMLprim value ml_idris2_setBufferDouble(value buffer, value loc, value val) {
  CAMLparam3(buffer, loc, val);
  idris2_setBufferDouble((void *)buffer, Int_val(loc), Double_val(val));
  CAMLreturn(Val_int(0));
}

CAMLprim value ml_idris2_setBufferString(value buffer, value loc, value val) {
  CAMLparam3(buffer, loc, val);
  idris2_setBufferString((void *) buffer, Int_val(loc), String_val(val));
  CAMLreturn(Val_int(0));
}

CAMLprim value ml_idris2_copyBuffer(value from, value start, value len, value to, value loc) {
  CAMLparam5(from,start,len,to,loc);
  idris2_copyBuffer((void *)from, Int_val(start), Int_val(len), (void *)to, Int_val(loc));
  CAMLreturn(Val_int(0));
}

CAMLprim value ml_idris2_readBufferData(value file, value buffer, value loc, value max) {
  CAMLparam4(file, buffer, loc, max);
  const int result = idris2_readBufferData((FILE *)file, (char *)buffer, Int_val(loc), Int_val(max));
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_idris2_writeBufferData(value file, value buffer, value loc, value len) {
  CAMLparam4(file, buffer, loc, len);
  const int result = idris2_writeBufferData((FILE *)file, (char *)buffer, Int_val(loc), Int_val(len));
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_idris2_getBufferByte(value buffer, value loc) {
  CAMLparam2(buffer, loc);
  const int result = idris2_getBufferByte((void *)buffer, Int_val(loc));
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_idris2_getBufferInt(value buffer, value loc) {
  CAMLparam2(buffer, loc);
  const int result = idris2_getBufferInt((void *)buffer, Int_val(loc));
  CAMLreturn(Val_int(result));
}

CAMLprim value ml_idris2_getBufferDouble(value buffer, value loc) {
  CAMLparam2(buffer, loc);
  const value result = caml_copy_double(idris2_getBufferDouble((void *)buffer, Int_val(loc)));
  CAMLreturn(result);
}

CAMLprim value ml_idris2_getBufferString(value buffer, value loc, value len) {
  CAMLparam3(buffer, loc, len);
  value result = caml_copy_string(idris2_getBufferString((void *)buffer, Int_val(loc), Int_val(len)));
  CAMLreturn(result);
}

/* Idrnet */

CAMLprim value ml_idrnet_malloc(value size) {
  CAMLparam1(size);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_free(value buffer) {
  CAMLparam1(buffer);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_peek(value buffer, value loc) {
  CAMLparam2(buffer, loc);
  value result = Val_int(0);
  CAMLreturn(result);
}
CAMLprim value ml_idrnet_poke(value buffer, value loc, value val) {
  CAMLparam3(buffer, loc, val);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_errno() {
  CAMLparam0();
  value result = Val_int(idrnet_errno());
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_socket(value domain, value type, value protocol) {
  CAMLparam3(domain, type, protocol);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_bind(value sockfd, value family, value socket_type, value host, value port) {
  CAMLparam5(sockfd, family, socket_type, host, port);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_getsockname(value sockfd, value address, value len) {
  CAMLparam3(sockfd, address, len);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_connect(value sockfd, value family, value socket_type, value host, value port) {
  CAMLparam5(sockfd, family, socket_type, host, port);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_sockaddr_family(value sockaddr) {
  CAMLparam1(sockaddr);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_sockaddr_ipv4(value sockaddr) {
  CAMLparam1(sockaddr);
  value result = Val_int(0);
  CAMLreturn(result);
}
CAMLprim value ml_idrnet_sockaddr_ipv4_port(value sockaddr) {
  CAMLparam1(sockaddr);
  value result = Val_int(0);
  CAMLreturn(result);
}
CAMLprim value ml_idrnet_create_sockaddr() {
  CAMLparam0();
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_accept(value sockaddr) {
  CAMLparam1(sockaddr);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_send(value sockfd, value data) {
  CAMLparam2(sockfd, data);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_send_buf(value sockfd, value data, value len) {
  CAMLparam3(sockfd, data, len);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_recv(value sockfd, value len) {
  CAMLparam2(sockfd, len);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_recv_buf(value sockfd, value buf, value len) {
  CAMLparam3(sockfd, buf, len);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_sendto(value sockfd, value data, value host, value port, value family) {
  CAMLparam5(sockfd, data, host, port, family);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_sendto_buf_native(value sockfd, value buf, value len, value host, value port, value family) {
  CAMLparam5(sockfd, buf, len, host, port);
  CAMLxparam1(family);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_sendto_buf_bytecode(value * argv, int argn ) {
  // TODO: Assert argn == 6?
  return ml_idrnet_sendto_buf_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value ml_idrnet_recvfrom(value sockfd, value len) {
  CAMLparam2(sockfd, len);
  value result = Val_int(0);
  CAMLreturn(result);
}
CAMLprim value ml_idrnet_recvfrom_buf(value sockfd, value buf, value len) {
  CAMLparam3(sockfd, buf, len);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_get_recv_res(value res_struct) {
  CAMLparam1(res_struct);
  value result = Val_int(0);
  CAMLreturn(result);
}
CAMLprim value ml_idrnet_get_recv_payload(value res_struct) {
  CAMLparam1(res_struct);
  value result = Val_int(0);
  CAMLreturn(result);
}
CAMLprim value ml_idrnet_free_recv_struct(value res_struct) {
  CAMLparam1(res_struct);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_get_recvfrom_res(value res_struct) {
  CAMLparam1(res_struct);
  value result = Val_int(0);
  CAMLreturn(result);
}
CAMLprim value ml_idrnet_get_recvfrom_payload(value res_struct) {
  CAMLparam1(res_struct);
  value result = Val_int(0);
  CAMLreturn(result);
}
CAMLprim value ml_idrnet_get_recvfrom_sockaddr(value res_struct) {
  CAMLparam1(res_struct);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_free_recvfrom_struct(value res_struct) {
  CAMLparam1(res_struct);
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idrnet_geteagain() {
  CAMLparam0();
  value result = Val_int(0);
  CAMLreturn(result);
}

CAMLprim value ml_idris2_listen(value socket, value backlog) {
  CAMLparam2(socket, backlog);
  const int result = listen(socket, backlog);
  CAMLreturn(Val_int(result));
}

