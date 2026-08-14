#ifndef __MULTICOPIER_MODEL_NC_BASE
#define __MULTICOPIER_MODEL_NC_BASE

#include <nimbleModel/predef/multiCopier_nC_base/multiCopier_nC_base.h>

// Model-backed multiCopier setup: each copier's varName resolves directly
// (via the model's own access()) to an ETaccessorBase for a live field, no
// intermediate row concept.
//
// A field's own storage (e.g. an Eigen::Tensor member) can be reallocated
// in place -- resize_from_list() in modelClass_.h calls
// ETA->template ref<n>().resize(...), which reallocates that Tensor's
// internal buffer -- between the time this multiCopier is built and the
// time its flatViewGroup is next used. ETaccessorBase::data() re-derefs the
// live field on every call rather than caching a pointer at construction
// time, so holding onto the accessors (moved, not just read from once) and
// asking them again via setActiveRow is enough to track that: no need to
// go back through the (slow) name-based access() scheme.
class multiCopier_model_nC_base : public multiCopier_nC_base {
public:
  // One accessor per copier, parallel to (and in the same order as) the
  // views held in flatViewGroup. Moved (not copied) from model->access() so
  // that S<double>().data() can be re-read later to pick up any
  // reallocation of the field's own storage.
  std::vector<std::unique_ptr<ETaccessorBase>> accessors;
  // Keeps the model itself alive for as long as accessors hold references
  // into its fields (ETaccessorBase binds a bare reference to the field,
  // not an owning pointer to the model).
  std::shared_ptr<genericInterfaceBaseC> modelKeepAlive;

  template<typename T, typename M>
  void init(std::shared_ptr<T> copiers, std::shared_ptr<M> model) {
    flatViewGroup.clear();
    accessors.clear();
    modelKeepAlive = model;
    for (const auto& copier : copiers->contents()) {
      auto acc = model->access(copier->varName); // acc will be std::unique_ptr<ETaccessorBase>
      flatViewGroup.add(makeRuntimeFlatView<double>(acc,
                                                copier->indsList->contents()));
      accessors.push_back(std::move(acc));
    }
  }

  // Shadows multiCopier_nC_base's no-op. There is no "row" concept here --
  // `row` is ignored -- but nimCopy_ calls setActiveRow(row) unconditionally
  // on both its src and dst, so this is the natural place to re-sync
  // flatViewGroup's data pointers with any reallocation that may have
  // happened to the model's fields since the last use.
  void setActiveRow(int /*row*/) {
    flatViewGroup.prepare_for_setData();
    for (auto& acc : accessors)
      flatViewGroup.setNextData(acc->template S<double>().data());
  }
};
#endif
