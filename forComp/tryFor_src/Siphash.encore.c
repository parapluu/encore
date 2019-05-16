#include "header.h"


static void* trait_method_selector(int id)
{
  switch (id)
  {
    case _ENC__MSG_Hasher_Hasher_hash:
    {
      return _enc__method_Siphash_Siphash_hash;
      break;
    }
    default:
    {
      printf("error, got invalid id: %d", id);
    }
  };
  return NULL;
}


void _enc__type_init_Siphash_Siphash(_enc__class_Siphash_Siphash_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  va_end(params);
}


void _enc__trace_Siphash_Siphash(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class_Siphash_Siphash_t* _this = p;
}


_enc__class_Siphash_Siphash_t* _enc__constructor_Siphash_Siphash(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class_Siphash_Siphash_t* _this = ((_enc__class_Siphash_Siphash_t*) encore_alloc((*_ctx), sizeof(_enc__class_Siphash_Siphash_t)));
  _this->_enc__self_type = (&(_enc__class_Siphash_Siphash_type));
  return _this;
}


uint64_t _enc__method_Siphash_Siphash_hash(pony_ctx_t** _ctx, _enc__class_Siphash_Siphash_t* _this, pony_type_t** runtimeType, uint64_t _enc__arg_id)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "hash");
  uint64_t _embed_0 = ({// Hashing-key, can be any random bytes
      uint8_t k[16] = {
          0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07, // k0
          0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f  // k1
      };

      // Allocate 8 bytes on the stack for the hashcode
      uint8_t hash[8];

      // Get the argument
      uint64_t id = _enc__arg_id;
      uint8_t id_arr[8];
      U64TO8_LE(id_arr, id);

      // Compute the hash value
      siphash(hash, id_arr, (uint64_t) 8, k);

      // Return the value to the caller
      U8TO64_LE(hash);});
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "hash");
  return ((uint64_t) _embed_0);
}


void* _enc__method_Siphash_Siphash_init(pony_ctx_t** _ctx, _enc__class_Siphash_Siphash_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  /* () */;
  UNIT;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  return UNIT;
}


pony_type_t _enc__class_Siphash_Siphash_type = {.id=_ENC__ID_Siphash_Siphash, .size=sizeof(_enc__class_Siphash_Siphash_t), .trace=_enc__trace_Siphash_Siphash, .vtable=trait_method_selector};
