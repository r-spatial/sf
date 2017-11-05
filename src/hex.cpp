#include <iostream>
#include <iomanip>

#include <Rcpp.h>

inline unsigned char char2int(char c) {
	if (c >= '0' && c <= '9')
		return c - '0';
	if (c >= 'a' && c <= 'f')
		return c - 'a' + 10;
	if (c >= 'A' && c <= 'F')
		return c - 'A' + 10;
	Rcpp::stop("char2int: false character in hex string");
}

// [[Rcpp::export]]
Rcpp::List CPL_hex_to_raw(Rcpp::CharacterVector cx) {
// HexToRaw modified from cmhh, see https://github.com/ianmcook/wkb/issues/10
// @cmhh: if you make yourself known, I can add you to the contributors

// convert a hexadecimal string into a raw vector
// this version, dropping istringstream and std::hex, is 12 time faster than
// the one in the wkb github issue. C rules.

	Rcpp::List output(cx.size());
	for (int j = 0; j < cx.size(); j++) {
		Rcpp::RawVector raw(cx[j].size() / 2);
		const char *cp = cx[j];
		for (int i = 0; i < raw.size(); i++) {
			raw[i] = (char2int(cp[0]) << 4) + char2int(cp[1]);
			cp += 2;
			if (i % 100000 == 0)
				Rcpp::checkUserInterrupt();
		}
		output[j] = raw;
		if (j % 1000 == 0)
			Rcpp::checkUserInterrupt();
	}
	return output;
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_raw_to_hex(Rcpp::RawVector raw) {
	std::ostringstream os;
	char hex[16] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		'a', 'b', 'c', 'd', 'e', 'f' };
	unsigned char *cp = &(raw[0]);
	for (int i = 0; i < raw.size(); i++) {
		int high = ((int) cp[i]) / 16;
		int low =  ((int) cp[i]) % 16;
  		os.write(&hex[high], sizeof(char));
  		os.write(&hex[low], sizeof(char));
	}
	return Rcpp::CharacterVector::create(os.str());
}
