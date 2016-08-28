// modified from cmhh, see https://github.com/ianmcook/wkb/issues/10
// @cmhh: if you make yourself known, I can add you to the contributors
#include <iostream>

#include <Rcpp.h>

// using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List HexToRaw(Rcpp::CharacterVector cx) {

	Rcpp::List output(cx.size());
	Rcpp::CharacterVector invec(cx);
	for (int j=0; j<cx.size(); j++) {
		Rcpp::RawVector raw(invec[j].size() / 2);
		std::string s = Rcpp::as<std::string>(invec[j]);
		int x;
		for (int i=0; i<raw.size(); i++){
			std::istringstream iss(s.substr(i*2, 2));
			iss >> std::hex >> x;
			raw[i] = x;
			if (i % 100000 == 0)
				Rcpp::checkUserInterrupt();
		}
		output[j] = raw;
		if (j % 1000 == 0)
			Rcpp::checkUserInterrupt();
	}
	return output;
}
