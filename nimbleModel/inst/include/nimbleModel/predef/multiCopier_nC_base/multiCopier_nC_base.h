#ifndef __MULTICOPIER_NC_BASE
#define __MULTICOPIER_NC_BASE

class multiCopier_nC_base {
public:
  int stuff;
  RuntimeFlatViewGroup<double> flatViewGroup;
    template<typename T, typename M>
    void init(std::shared_ptr<T> multiCopier, std::shared_ptr<M> model) {
      const auto& copiers = multiCopier->contents();
      for(const auto& copier : copiers) {
        auto acc = model->access(copier->varName); // acc will be std::unique_ptr<ETaccessorBase>
        std::vector<int> sizes = acc->intDims();
        std::cout<<"sizes from init: ";
        for(auto s : sizes) std::cout<<s<<" ";
        std::cout<<std::endl;
        std::vector<b__> blocks;
        const auto& indsList = copier->indsList->contents();
        for(const auto& inds : indsList) {
          blocks.push_back(b__(inds[0]-1, inds[1]-1));
          std::cout<<"pushing back "<<inds[0]<<" "<<inds[1]<<std::endl;
        }
        flatViewGroup.add(RuntimeFlatView<double>(acc->template S<double>().data(), RuntimeSubviewInfo(sizes, blocks)));
      }
        std::cout<<"hw from init"<<std::endl;
    };
};

#endif
