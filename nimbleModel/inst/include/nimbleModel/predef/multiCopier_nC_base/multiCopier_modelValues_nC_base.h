#ifndef __MULTICOPIER_MODELVALUES_NC_BASE
#define __MULTICOPIER_MODELVALUES_NC_BASE

#include <nimbleModel/predef/multiCopier_nC_base/multiCopier_nC_base.h>

// modelValues-backed multiCopier setup: each copier's varName resolves to an
// nList (one element per row) rather than directly to a value, so getting an
// ETaccessorBase for a variable additionally requires picking a row via
// nListBase_nClass::access_at(row). setActiveRow(row) is how that row gets
// (re)selected, and it can be called repeatedly to retarget the same
// flatViewGroup at a different row without re-resolving variable names.
//
// Unlike the model-backed sibling class, the nList a variable currently
// resolves to cannot be cached across calls: modelValues variables are
// exposed to R via genericInterfaceC, and an R-level assignment onto a
// compiled-nList-typed field (accessor_class<P,T2>::set(), P_is_shared_ptr
// branch) reassigns the member outright rather than mutating it in place.
// A cached std::shared_ptr<nListBase_nClass> copy would not see that -- it
// would keep pointing at the old (now orphaned but still valid, since our
// copy owns a reference to it) nList, silently disconnected from whatever
// the field now holds. There is no safe way to hold a live "pointer to the
// member slot" through the type-erased genericInterfaceBaseC interface
// either (that would need a reinterpret_cast between unrelated shared_ptr
// instantiations). So instead, only the name -> accessor_base resolution is
// cached (fieldAccessors, resolved once in init()); the nList itself is
// re-derived from the live modelValues object via accessor_base::
// getInterfacePtr() on every use (see resolveRowSource) -- no map lookup by
// name, but always a fresh read, so a reassignment is never missed.
class multiCopier_modelValues_nC_base : public multiCopier_nC_base {
public:
  // One accessor per copier, parallel to (and in the same order as) the
  // views held in flatViewGroup once built. This is the field's
  // name-resolved accessor_base, NOT the nList it currently resolves to --
  // see the class comment above for why the nList itself must not be cached.
  std::vector<std::shared_ptr<accessor_base>> fieldAccessors;
  // Keeps modelValues itself alive (and is the intBasePtr argument to
  // fieldAccessors[k]->getInterfacePtr()) for as long as fieldAccessors hold
  // resolutions against it -- same reasoning as modelKeepAlive in
  // multiCopier_model_nC_base.
  std::shared_ptr<genericInterfaceBaseC> modelValuesKeepAlive;
  // Saved index-block selections, one per copier, needed to (re)build a view
  // the first time a row is actually available (see setActiveRow).
  std::vector<std::vector<b__>> ssList;
  // Whether flatViewGroup has been fully built (shape + data) yet.
  bool built = false;

  // Resolves each copier's varName to its accessor_base, via modelValues's
  // own (virtual) get_name2access(), so this works through a base
  // modelValuesBase_nClass pointer -- no need for the caller to have the
  // fully-derived generated modelValues type in hand.
  //
  // Deliberately does NOT touch flatViewGroup: at this point modelValues may
  // have declared/initialized each nList but not yet resized it (a row count
  // of 0 is possible), so there may be no row yet to read shape/data from.
  // setActiveRow() must be called at least once, after rows exist, before
  // flatViewGroup is used for anything (getValues/setValues/nimCopy_/etc.).
  template<typename T>
  void init(std::shared_ptr<T> copiers,
            std::shared_ptr<modelValuesBase_nClass> modelValues) {
    flatViewGroup.clear();
    fieldAccessors.clear();
    ssList.clear();
    built = false;
    modelValuesKeepAlive = modelValues;
    const auto& name2access = modelValues->get_name2access();
    for (const auto& copier : copiers->contents()) {
      auto it = name2access.find(copier->varName);
      if (it == name2access.end())
        Rcpp::stop("multiCopier (modelValues): variable not found: " + copier->varName);
      fieldAccessors.push_back(it->second);
      ssList.push_back(vec_2_vecB__(copier->indsList->contents()));
    }
  }

  // Re-derives fieldAccessors[k]'s current nList from the live modelValues
  // object -- see the class comment for why this must not be cached.
  std::shared_ptr<nListBase_nClass> resolveRowSource(size_t k) {
    std::shared_ptr<genericInterfaceBaseC> ip =
      fieldAccessors[k]->getInterfacePtr(modelValuesKeepAlive.get());
    std::shared_ptr<nListBase_nClass> nlPtr =
      std::dynamic_pointer_cast<nListBase_nClass>(ip);
    if (!nlPtr)
      Rcpp::stop("multiCopier (modelValues): variable is not an nList.");
    return nlPtr;
  }

  // Rebinds every view to row `newRow`. The first call also builds each
  // view's shape (from that row's accessor); every later call assumes all
  // rows of a variable share that shape (an invariant of
  // modelValuesClass_::resize_one, which always resizes every row to the
  // same dims) and only swaps the data pointer.
  void setActiveRow(int newRow) {
    if (!built) {
      flatViewGroup.clear();
      for (size_t k = 0; k < fieldAccessors.size(); ++k) {
        auto acc = resolveRowSource(k)->access_at(newRow);
        flatViewGroup.add(makeRuntimeFlatView<double>(acc, ssList[k]));
      }
      built = true;
    } else {
      // TODO(perf): this is a hot path (called on every sampler iteration in
      // MCMC). resolveRowSource()+access_at() each do a dynamic_cast plus
      // access_at() builds a whole ETaccessorBase -- 2 heap allocations (the
      // accessor object + its intDims_ vector, the latter never even read
      // here) -- just to reach .data(). Once the shape is known (built ==
      // true), all we actually need is the raw pointer, re-derived fresh
      // (both the current nList and the current row's data are still
      // re-read every call, since either can change underneath us -- see the
      // class comment). A lightweight nListBase_nClass::data_ptr_at(int i)
      // (contents_.at(i).data(), no ETaccessorBase involved) would let this
      // branch skip the ETaccessorBase allocation. Deferred until this shows
      // up as a real bottleneck; see conversation for the sizing.
      flatViewGroup.prepare_for_setData();
      for (size_t k = 0; k < fieldAccessors.size(); ++k) {
        auto acc = resolveRowSource(k)->access_at(newRow);
        flatViewGroup.setNextData(acc->template S<double>().data());
      }
    }
  }
};
#endif
