#include "party.h"
#include <ds/list.h>
#include "structure.h"

// static threshold at which to run things in parallel
#define PAR_REDUCE_SPLIT 400

// static threshold to run things sequentially. partys with less than 200
// known items can be fused and run sequentially with partys of size
// less than PAR_REDUCE_SPLIT
#define PAR_REDUCE_THRESHOLD_MIN 200

// Helper functions
#define reduction_less_min(n) (n < PAR_REDUCE_THRESHOLD_MIN)
#define reduction_split(n) (n > PAR_REDUCE_SPLIT)
#define setup_closure_args(value, init) (value_t[]){(init), (value)};

/** Creates a list of ParT nodes containing values
 *
 * This function returns leaf nodes of the ParT structure.
 *
 * @param p ParT structure
 * @return list with all ParT containing values (no intermediate nodes)
 */
static list_t* party_group_values(par_t * const p){
  list_t *queue = NULL;
  list_t *result = NULL;
  par_t *current = p;

  while(current) {
    if (party_tag(current) == PAR_PAR) {
      par_t* pleft = party_get_parleft(current);
      queue = ponyint_list_push(queue, party_get_parright(current));
      current = pleft;
    } else if (party_tag(current) == EMPTY_PAR) {
      if (queue != NULL) {
        par_t *tmp;
        queue = ponyint_list_pop(queue, (void**)&tmp);
        current = tmp;
      } else {
        break;
      }
    } else {
      par_t *tmp;
      result = ponyint_list_push(result, current);
      if (queue != NULL) {
        queue = ponyint_list_pop(queue, (void**)&tmp);
        current = tmp;
      } else {
        break;
      }
    }
  }
  return result;
}


/**
 * Helper function to reduce a ParT sequentially. This is similar to
 * `party_reduce_sequential` and `party_reduce_assoc` except that it does not
 * return a future, it returns an `encore_arg_t` with the result value of
 * applying the closure function (cumulatively) to the items in the ParT.
 */
static encore_arg_t reduce_sequential(pony_ctx_t **ctx,
                                      par_t * const p,
                                      encore_arg_t init,
                                      closure_t * const closure,
                                      pony_type_t *type){
  list_t *list = party_group_values(p);
  par_t *pval = NULL;
  while(list){
    list = ponyint_list_pop(list, (void**)(&pval));

    switch(party_tag(pval)){
    case EMPTY_PAR: return init;
    case VALUE_PAR: {
      encore_arg_t value = party_get_v(pval);
      value_t *args = setup_closure_args(value, init);
      init = closure_call(ctx, closure, args);
      break;
    }
    case FUTURE_PAR: {
      /** OPTIMISATION:
       * we could add the future to another queue and re-visit if realised later
       */
      future_t *f = party_get_fut(pval);
      future_await(ctx, f);
      encore_arg_t value = future_get_actor(ctx, f);
      value_t *args = setup_closure_args(value, init);
      init = closure_call(ctx, closure, args);
      break;
    }
    case FUTUREPAR_PAR: {
      future_t *fp = party_get_futpar(pval);
      future_await(ctx, fp);
      par_t * const par = (future_get_actor(ctx, fp)).p;
      init = reduce_sequential(ctx, par, init, closure, type);
      break;
    }
    case ARRAY_PAR: {
      array_t* ar = party_extract(ctx, pval, type);
      size_t ar_size = array_size(ar);
      for(size_t i = 0; i < ar_size; ++i){
        encore_arg_t value = array_get(ar, i);
        value_t *args = setup_closure_args(value, init);
        init = closure_call(ctx, closure, args);
      }
      break;
    }
    case PAR_PAR: exit(-1);
    default: exit(-1);
    }
  }
  return init;
}

// OPTIMISATION: Minimize the number of context switches of awaiting futures
// A good strategy could be to await until all futures in the Par t have been
// realised.
future_t* party_reduce_sequential(pony_ctx_t **ctx,
                                  par_t * const p,
                                  encore_arg_t init,
                                  closure_t * const closure,
                                  pony_type_t *type){

  future_t *fut = future_mk(ctx, type);

  switch(party_tag(p)){
  case EMPTY_PAR: {
    future_fulfil(ctx, fut, init);
    return fut;
  }
  case VALUE_PAR: {
    encore_arg_t value = party_get_v(p);
    value_t *args = setup_closure_args(value, init);

    value_t result = closure_call(ctx, closure, args);
    future_fulfil(ctx, fut, result);
    return fut;
  }
  case FUTURE_PAR: {
    future_t *fval = party_get_fut(p);
    if (!future_fulfilled(fval)) {
      future_await(ctx, fval);
    }

    encore_arg_t value = future_get_actor(ctx, fval);
    value_t *args = setup_closure_args(value, init);
    encore_arg_t result = closure_call(ctx, closure, args);
    future_fulfil(ctx, fut, result);
    return fut;
  }
  case PAR_PAR: {
    par_t* pleft = party_get_parleft(p);
    par_t* pright = party_get_parright(p);
    par_t **p_par = (par_t*[]){pleft, pright, NULL};

    encore_arg_t result = init;
    while(*p_par){
      result = reduce_sequential(ctx, *p_par, result, closure, type);
      ++p_par;
    }
    future_fulfil(ctx, fut, result);
    return fut;
  }
  case FUTUREPAR_PAR: {
    future_t *fp = party_get_futpar(p);
    future_await(ctx, fp);
    par_t * const par = (future_get_actor(ctx, fp)).p;
    return party_reduce_sequential(ctx, par, init, closure, type);
  }
  case ARRAY_PAR: {
    array_t* ar = party_extract(ctx, p, type);
    size_t ar_size = array_size(ar);
    encore_arg_t result = init;
    for(size_t i = 0; i < ar_size; ++i){
      encore_arg_t value = array_get(ar, i);
      result = closure_call(ctx, closure, (value_t[]){result, value});
    }
    future_fulfil(ctx, fut, result);
    return fut;
  }
  default: exit(-1);
  }
}


// TODO: this is a mock up and should run the function in parallel
future_t* party_reduce_assoc(pony_ctx_t **ctx,
                             par_t * const p,
                             encore_arg_t init,
                             closure_t * const closure,
                             pony_type_t * type){
  future_t *fut = future_mk(ctx, type);
  encore_arg_t result = reduce_sequential(ctx, p, init, closure, type);
  future_fulfil(ctx, fut, result);
  return fut;
}
