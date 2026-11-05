#ifndef __MULTICOPIER_MODEL_NC_BASE
#define __MULTICOPIER_MODEL_NC_BASE

#include <nimbleModel/predef/mCopy_nC_base/mCopy_nC_base.h>

// model version of a multiCopier.
//
// When an Eigen::Tensor is reallocated (e.g. resized), its allocated
// memory (its data pointer) changes, but the Eigen::Tensor object
// itself does not. So we hold a reference to the Eigen::Tensor
// (via an ETaccessorBase that is an ETaccessor for that Tensor).
class multiCopier_model_nC_base : public multiCopier_nC_base {
public:
  // ETaccessors (for a non-scalar) hold a reference to an Eigen::Tensor.
  // The allows us to check the data pointer on each use, in case the
  // Eigen::Tensor has been reallocated since the last call.
  std::vector<std::unique_ptr<ETaccessorBase>> accessors;
  // Hold a shared_ptr to the model to keep it alive.
  // In normal nimble usage, this should not be necessary, but it is safer and cheap.
  std::shared_ptr<genericInterfaceBaseC> modelKeepAlive;

  // init sets the vectors of ETaccessors and RuntimeFlatView.
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

  // RuntimeFlatViewGroup and RuntimeFlatView have a system of updating
  // all the data pointers to be used (with the precomputed and cached
  // indexing and striding, which [along with the sizes of the variables themselves]
  // are assumed to be constant after initialization).
  // The system assumes the data pointers will always be provided in the same order,
  // i.e. the order of the ETaccessors.
  // The argument is ignored (it is used for rows of the modelValues case).
  void setActiveRow(int /*row*/) {
    flatViewGroup.prepare_for_setData(); // reset counter to 0
    for (auto& acc : accessors)
      flatViewGroup.setNextData(acc->template S<double>().data()); // update each one's data pointer.
  }

  // Get and set all values, refreshing the data pointers first in case a
  // model variable has been reallocated since init(). setValues_() returns
  // a proxy so that `setValues_() = v;` does the copy.
  Eigen::Tensor<double, 1> getValues_() {
    setActiveRow(0);
    return flatViewGroup.copyIntoVector();
  }
  RuntimeFlatViewGroup<double>::SetValuesProxy setValues_() {
    setActiveRow(0);
    return flatViewGroup.setValues_();
  }

  // Get and set one entry, for values(model, nodes[i], mode = ...). i is 1-based.
  // "raw" mode: entry i of nodes (one view each).
  // "scalar" mode: element i of the concatenated values, returned as a length-1 vector.
  // The setters return proxies, as setValues_() does.
  Eigen::Tensor<double, 1> getValues_raw_(int i) {
    setActiveRow(0);
    return flatViewGroup.copyIntoVector_one(i - 1);
  }
  RuntimeFlatViewGroup<double>::SetValuesOneProxy setValues_raw_(int i) {
    setActiveRow(0);
    return flatViewGroup.setValues_one_(i - 1);
  }
  Eigen::Tensor<double, 1> getValues_scalar_(int i) {
    setActiveRow(0);
    Eigen::Tensor<double, 1> ans(1);
    ans(0) = flatViewGroup.at(i - 1);
    return ans;
  }
  RuntimeFlatViewGroup<double>::SetValuesAtProxy setValues_scalar_(int i) {
    setActiveRow(0);
    return flatViewGroup.setValues_at_(i - 1);
  }

  // For use from R, via
  // multiCopier_nClass$getValues_indexed and $setValues_indexed,
  // allowing run-time choice of mode.
  Eigen::Tensor<double, 1> getValues_indexed_(int i, const std::string &mode) {
    if (mode == "raw") return getValues_raw_(i);
    if (mode == "scalar") return getValues_scalar_(i);
    Rcpp::stop("getValues_indexed: mode must be \"raw\" or \"scalar\".");
  }
  void setValues_indexed_(int i, const Eigen::Tensor<double, 1> &v, const std::string &mode) {
    if (mode == "raw") {setValues_raw_(i) = v; return;}
    if (mode == "scalar") {setValues_scalar_(i) = v; return;}
    Rcpp::stop("setValues_indexed: mode must be \"raw\" or \"scalar\".");
  }
};
#endif
