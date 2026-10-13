#ifndef __MULTICOPIER_MODELVALUES_NC_BASE
#define __MULTICOPIER_MODELVALUES_NC_BASE

#include <nimbleModel/predef/mCopy_nC_base/mCopy_nC_base.h>

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
  // The accessor_base here is the base class for the generic interface scheme
  // for name-based lookup. (This is not related to ETaccessors).
  // E.g. for a variable "mu", we can cache the accessor for "mu" without
  // repeating the name-based lookup, but that's as close as we can get.
  // For safety we need to re-obtain the nList for "mu" on each copy usage.
  std::vector<std::shared_ptr<accessor_base>> fieldAccessors;
  // Ensure the modelValues stays alive. This shouldn't be needed in
  // nimble2's design but is here for safety and is cheap.
  std::shared_ptr<genericInterfaceBaseC> modelValuesKeepAlive;
  // Saved index-block information for deferring (lazy) setup of the RuntimeFlatViews.
  std::vector<std::vector<b__>> ssList;
  bool built = false;

  // When init is called, there is no guarantee that the variables will be
  // initialized so we can't count on reading their sizes.
  // Hence we defer that step until the first actual copy (via setActiveRow).
  template<typename T>
  void init(std::shared_ptr<T> copiers,
            std::shared_ptr<MVBase_nC> modelValues) {
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

  // Get an nList base class pointer from the accessor_base for the k-th variable.
  std::shared_ptr<nListBase_nClass> resolveRowSource(size_t k) {
    std::shared_ptr<genericInterfaceBaseC> ip =
      fieldAccessors[k]->getInterfacePtr(modelValuesKeepAlive.get());
    // The modelValues object may have been created by a different DLL (see nc_shared_ptr_cast in nCompiler).
    std::shared_ptr<nListBase_nClass> nlPtr =
      nc_shared_ptr_cast<nListBase_nClass>(ip);
    if (!nlPtr)
      Rcpp::stop("multiCopier (modelValues): variable is not an nList.");
    return nlPtr;
  }

  // On the first call, we assume we can obtain size information
  // that will be constant thereafter (based on whichever row was chosen first!)
  void setActiveRow(int newRow) {
    if (!built) {
      flatViewGroup.clear();
      for (size_t k = 0; k < fieldAccessors.size(); ++k) {
        auto acc = resolveRowSource(k)->access_at(newRow);
        // makeRuntimeFlatView does initialize the data pointer, so the
        // prepare_for_setData and setNextData steps are not necessary.
        flatViewGroup.add(makeRuntimeFlatView<double>(acc, ssList[k]));
      }
      built = true;
    } else {
      // See comment in multiCopier_model_nC_base.
      // Performance note for the modelValues case:
      // the nListBase_nClass::access_at(row) allocates a new ETaccessor
      // every time. Future optimization could re-use a single ETaccessor,
      // but that would take a small extension of ETaccessor.
      flatViewGroup.prepare_for_setData(); // reset the counter.
      for (size_t k = 0; k < fieldAccessors.size(); ++k) {
        auto acc = resolveRowSource(k)->access_at(newRow); // Get an ETaccessor.
        flatViewGroup.setNextData(acc->template S<double>().data()); // reset the data pointer.
      }
    }
  }
};
#endif
