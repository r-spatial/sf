#include <Rcpp.h>

inline unsigned char char2int(char c) {
	if (c >= '0' && c <= '9')
		return c - '0';
	if (c >= 'a' && c <= 'f')
		return c - 'a' + 10;
	if (c >= 'A' && c <= 'F')
		return c - 'A' + 10;
	Rcpp::stop("char2int: unrecognized character in hex string");
	return '0'; // never reached, satisfy -Wreturn-type
}

// [[Rcpp::export]]
Rcpp::List CPL_hex_to_raw(Rcpp::CharacterVector cx) {
// convert hexadecimal string into a raw vector:
	Rcpp::List output(cx.size());
	for (int j = 0; j < cx.size(); j++) {
		Rcpp::RawVector raw(cx[j].size() / 2);
		const char *cp = cx[j];
		for (int i = 0; i < raw.size(); i++) {
			raw[i] = (char2int(cp[0]) << 4) + char2int(cp[1]);
			cp += 2;
			if (i % 131072 == 0) // 2^17, see https://www.jottr.org/2015/06/05/checkuserinterrupt/
				Rcpp::checkUserInterrupt();
		}
		output[j] = raw;
		if (j % 1024 == 0)
			Rcpp::checkUserInterrupt();
	}
	return output;
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_raw_to_hex(Rcpp::RawVector raw) {
// convert a raw vector into hexadecimal string:
	std::vector<char> str(raw.size() * 2 + 1);
	char hex[16] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		'a', 'b', 'c', 'd', 'e', 'f' };
	unsigned char *cp = &(raw[0]);
	int j = 0;
	for (int i = 0; i < raw.size(); i++) {
		str[j] = hex[(((int) cp[i]) / 16)];
		j++;
		str[j] = hex[(((int) cp[i]) % 16)];
		j++;
	}
	str[j] = '\0';
	return Rcpp::CharacterVector::create(str.data());
}
