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

CAMLprim value ml_idris2_putStr(value s)
{
	CAMLparam1(s);
	idris2_putStr(String_val(s));
	CAMLreturn(Val_int(0));
}

CAMLprim value ml_idris2_getString(value sptr)
{
	CAMLparam1(sptr);
	const char * result = idris2_getString((void *) sptr);
	CAMLreturn((value) result);
}

CAMLprim value ml_idris2_getEnvPair(value i)
{
	CAMLparam1(i);
	const char * result = idris2_getEnvPair(Int_val(i));
	CAMLreturn((value) result);
}

CAMLprim value ml_getenv(value s)
{
	CAMLparam1(s);
	const char * result = getenv(String_val(s));
	CAMLreturn((value) result);
}

CAMLprim value ml_idris2_isNull(value ptr)
{
	CAMLparam1(ptr);
	const int result = idris2_isNull((void *) ptr);
	CAMLreturn(Val_int(result));
}
