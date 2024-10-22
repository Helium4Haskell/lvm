/* internal value kinds */
enum value_kind {
  Kind_unknown,
  Kind_int,
  Kind_float,
  Kind_bytes,
  Kind_con,
  Kind_nap,
  Kind_code,
  Kind_custom,
  Kind_ap,
  Kind_caf,
  Kind_raise,
  Kind_inv,
  Kind_ind,
  Kind_suspend
};

    Instr(GETKIND): {
      value v = sp[0];
      Assert(Is_heap_val(v) || Is_atom(v) || Is_long(v) || Tag_val(v) == Code_tag);

      if (Is_long(v)) {
        /* int */
        v = Atom(Kind_int);
      }
      else {
        tag_t tag = Tag_val(v);
        if (tag <= Con_max_tag) {
          v = Atom(Kind_con);
        }
        else {
          switch (tag) {
          case Raise_tag:   v = Atom(Kind_raise); break;
          case Inv_tag:     v = Atom(Kind_inv); break;
          case Caf_tag:     v = Atom(Kind_caf); break;
          case Nap_tag:     v = Atom(Kind_nap); break;
          case Ap_tag:      v = Atom(Kind_ap);  break;
          case Ind_tag:     v = Atom(Kind_ind); break;
          case Code_tag:    v = Atom(Kind_code); break;
          case String_tag:  v = Atom(Kind_bytes); break;
          case Double_tag:  v = Atom(Kind_float); break;
          case Suspend_tag: v = Atom(Kind_suspend); break;
          case Custom_tag:  v = Atom(Kind_custom); break;
          default:          v = Atom(Kind_unknown); break;
          }
        }
      }
      sp[0] = v;
      Next;
    }
