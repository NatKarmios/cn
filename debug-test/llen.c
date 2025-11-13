typedef struct sll {
  int head;
  struct sll* tail;
} sll_t;

/*@
datatype List {
  Nil {},
  Cons {i32 Head, datatype List Tail}
}

predicate [rec] (datatype List) SLL_At(pointer p) {
  if (is_null(p)) {
    return Nil{};
  } else {
    take H = RW<struct sll>(p);
    take T = SLL_At(H.tail);
    return (Cons { Head: H.head, Tail: T });
  }
}

function [rec] (u32) Length(datatype List L) {
  match L {
    Nil {} => {
      0u32
    }
    Cons {Head: H, Tail : T}  => {
      1u32 + Length(T)
    }
  }
}
@*/

unsigned int llen(sll_t* l)
/*@ requires take L = SLL_At(l);
    ensures take L_post = SLL_At(l);
            L == L_post;
            return == Length(L);
@*/
{
    /*@ unfold Length(L); @*/
    if (l == 0) {
        return 0;
    } else {
        return llen(l->tail);
    }
}
