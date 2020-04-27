#include <Rcpp.h>
using namespace Rcpp;

//' @importFrom Rcpp evalCpp
//' @export MetabOneStationDO
class MetabOneStationDO {
public:
   MetabOneStationDO(double x_) : x(x_) {}
   
   DataFrame add(double y) {
      NumericVector blah = NumericVector::create(x + y);
      DataFrame answer = DataFrame::create(Named("a") = blah);
      return answer;
   }
   
   double x;
};

RCPP_MODULE(mod_metab) {
   class_<MetabOneStationDO>("MetabOneStationDO")
   
   .constructor<double>()
   
   .field("x", &MetabOneStationDO::x)
   
   .method("add", &MetabOneStationDO::add);
}
