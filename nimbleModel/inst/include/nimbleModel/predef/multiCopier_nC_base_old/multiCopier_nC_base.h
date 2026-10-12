#ifndef __MULTICOPIER_NC_BASE
#define __MULTICOPIER_NC_BASE

// multiCopier is used for nimble2's implementations of values() and nimCopy().
// It represents access to a set of variables and index ranges for each variable.
// It use's nCompiler's RuntimeFlatViewGroup for the actual copying (including indexing and striding).
//
// Here is the base class for model and modelValues versions of multiCopies
// and the universal nimCopy_ implementation between any two multiCopiers.
//
// The trick is that we want to look up named variables only once, but hold 
// access to each member variable (e.g. an Eigen::Tensor for a model or an 
// nList<Eigen::Tensor<nDim, ScalarType > > for a modelValues), not the data
// it points to. That way if the member variable is reset, we don't point to
// stale data. For a model, we hold a reference to the Eigen::Tensor. For
// a modelValues, it is harder: we hold the generic accessor object (accessor
// of the generic interface scheme, not to be confused with ETaccessor for
// type-flexible access to Eigen::Tensors).
// A second trick is that we want to precompute and cache indexing and striding
// info for each copiers, but we need to update the data pointer every time
// in case it has changed. setActiveRow() does that, with the row concept
// relevant only for modelValue.
//
// All copying machinery assumes that the sizes of variables, and the 
// selection of indexed blocks of variables, will not change after initialization.
class multiCopier_nC_base {
public:
  RuntimeFlatViewGroup<double> flatViewGroup;
  void setActiveRow(int row) {}
};

template<typename SrcT, typename DstT>
void nimCopy_(const std::shared_ptr<SrcT> &src, int srcRow,
              const std::shared_ptr<DstT> &dst, int dstRow) {
  src->setActiveRow(srcRow);
  dst->setActiveRow(dstRow);
  src->flatViewGroup.copyTo(dst->flatViewGroup);
}

#endif
