#define CAML_INTERNALS 1
#include <caml/callback.h>
#include <caml/intext.h>
#include <caml/memory.h>
#include <caml/stack.h>

void interpreter_register_module(value m_frametable, value m_gc_roots, value m_data, value m_code) {
  CAMLparam4(m_frametable, m_gc_roots, m_data, m_code);

  // frametable
  void *frametable_sym = (void *)Nativeint_val(m_frametable);
  caml_register_frametable(frametable_sym);

  // gc_roots
  void *gc_roots_sym = (void *)Nativeint_val(m_gc_roots);
  caml_register_dyn_global(gc_roots_sym);

  // data section
  void *data_begin_sym = (void *)Nativeint_val(Field(m_data, 0));
  void *data_end_sym = (void *)Nativeint_val(Field(m_data, 1));
  caml_page_table_add(In_static_data, data_begin_sym, data_end_sym);

  // code section
  struct code_fragment *cf;
  void *code_begin_sym = (void *)Nativeint_val(Field(m_code, 0));
  void *code_end_sym = (void *)Nativeint_val(Field(m_code, 1));
  caml_page_table_add(In_code_area, code_begin_sym, code_end_sym);
  cf = caml_stat_alloc(sizeof(struct code_fragment));
  cf->code_start = (char *)0;
  cf->code_end = (char *)code_end_sym;
  cf->digest_computed = 0;
  caml_ext_table_add(&caml_code_fragments_table, cf);

  // caml_globals_inited++;
  CAMLreturn0;
}
