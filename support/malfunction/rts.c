#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <stdio.h>
#include <string.h>

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
