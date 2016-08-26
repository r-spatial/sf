// mostly attributed to cmhh, see https://github.com/ianmcook/wkb/issues/10
// @cmhh: if you make yourself known, I can add you to the contributors
#include <fstream>
#include <iostream>

#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
RawVector HexToRaw(CharacterVector cx) {

   Rcpp::CharacterVector invec(cx);
   Rcpp::RawVector raw(invec[0].size() / 2);
   std::string s = Rcpp::as<std::string>(invec[0]);
   int x;
   for (int i=0; i<raw.size(); i++){
      std::istringstream iss(s.substr(i*2, 2));
      iss >> std::hex >> x;
      raw[i] = x;
   }
   return raw;
}
