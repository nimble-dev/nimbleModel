#ifndef __MULTICOPIER_NC_BASE
#define __MULTICOPIER_NC_BASE

class multiCopier_nC_base {
public:
  int stuff;
  RuntimeFlatViewGroup<double> flatViewGroup;

  template<typename T, typename M>
    void init(std::shared_ptr<T> multiCopier, std::shared_ptr<M> model) {
      flatViewGroup.clear();
      const auto& copiers = multiCopier->contents();
      for(const auto& copier : copiers) {
        auto acc = model->access(copier->varName); // acc will be std::unique_ptr<ETaccessorBase>
        flatViewGroup.add(makeRuntimeFlatView<double>(acc, 
                                                  copier->indsList->contents()));
      }
      std::cout<<"hw from init"<<std::endl;
    }
};
#endif
