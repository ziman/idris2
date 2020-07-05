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
  idris2_closeDIr(String_val(dir));
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
