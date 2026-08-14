#ifndef __MULTICOPIER_NC_BASE
#define __MULTICOPIER_NC_BASE

// Common base for every multiCopier flavor (model-backed, modelValues-backed,
// ...). Holds the one piece of state every flavor needs -- a flattened view
// over the selected variables/index-ranges -- plus a `setActiveRow` hook so
// generic code (nimCopy_ below) can address a "current row" uniformly, even
// for backends, like a model, that have no notion of rows at all.
//
// Flavor-specific setup (`init`) lives in each flavor's own base class
// (e.g. multiCopier_model_nC_base, multiCopier_modelValues_nC_base) since the
// source object each flavor reads from differs in kind, not just in type.
class multiCopier_nC_base {
public:
  RuntimeFlatViewGroup<double> flatViewGroup;

  // No-op default: most backends (e.g. a model) have nothing to key by row.
  // multiCopier_modelValues_nC_base shadows this with a real implementation.
  // Not virtual: which flavor is in play is always known statically at the
  // call site (see nimCopy_), so ordinary name lookup/shadowing is enough.
  void setActiveRow(int row) {}
};

// Copies flatViewGroup-to-flatViewGroup from src (at row srcRow) to dst (at
// row dstRow), after pointing each one at its requested row first. SrcT/DstT
// are resolved at compile time, so this one function works uninformed of
// which concrete multiCopier flavor either side is -- setActiveRow is a
// no-op for a flavor (e.g. model-backed) that doesn't have rows, and does
// the real rebind for one (e.g. modelValues-backed) that does.
template<typename SrcT, typename DstT>
void nimCopy_(const std::shared_ptr<SrcT> &src, int srcRow,
              const std::shared_ptr<DstT> &dst, int dstRow) {
  src->setActiveRow(srcRow);
  dst->setActiveRow(dstRow);
  src->flatViewGroup.copyTo(dst->flatViewGroup);
}

#endif
