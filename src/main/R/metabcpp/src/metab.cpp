#include <Rcpp.h>
using namespace Rcpp;

//' @importFrom Rcpp evalCpp
//' @export MetabOneStationDO
class MetabOneStationDO {
public:
   MetabOneStationDO(double x_) : x(x_) {}
   
   double add(double y) {
      return x + y;
   }
   
   double x;
};

RCPP_MODULE(mod_metab) {
   class_<MetabOneStationDO>("MetabOneStationDO")
   
   .constructor<double>()
   
   .field("x", &MetabOneStationDO::x)
   
   .method("add", &MetabOneStationDO::add);
}
