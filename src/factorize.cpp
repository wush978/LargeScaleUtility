#include <Rcpp.h>

using namespace Rcpp;

template <int RTYPE, class T>
SEXP factorize(SEXP Rsrc, SEXP Rindex_src) {
  std::map<T, int> index;
  Vector<RTYPE> src(Rsrc);
  IntegerVector retval(src.size());
  if (Rindex_src == R_NilValue) {
    std::vector<T> levels;
    int nlevel = 1;
    for(int i = 0;i < src.size();i++) {
      T glue = as<T>(wrap(src[i]));
      int value = index[glue];
      if (value == 0) {
        index[glue] = nlevel;
        value = nlevel++;
        levels.push_back(glue);
      }
      retval[i] = value;
    }
    retval.attr("levels") = wrap(levels);
  }
  else {
    Vector<RTYPE> index_src(Rindex_src);
    for(int i = 0;i < index_src.size();i++) {
      T glue = as<T>(wrap(index_src[i]));
      int value = index[glue];
      if (value != 0) {
        throw std::invalid_argument("Duplicated element in Rindex_src");
      }
      index[glue] = i + 1;
    }
    for(int i = 0;i < src.size();i++) {
      T glue = as<T>(wrap(src[i]));
      retval[i] = index[glue];
      if (retval[i] == 0) {
        retval[i] = NA_INTEGER;
      }
    }
  }
  return retval;
}

RcppExport SEXP factorize_integer(SEXP Rsrc, SEXP Rindex_src) {
	BEGIN_RCPP
	return factorize<INTSXP, int>(Rsrc, Rindex_src);
	END_RCPP
}

RcppExport SEXP factorize_character(SEXP Rsrc, SEXP Rindex_src) {
  BEGIN_RCPP
  return factorize<STRSXP, std::string>(Rsrc, Rindex_src);
  END_RCPP
} 

RcppExport SEXP interact(SEXP Rfactor1, SEXP Rfactor2, SEXP Rlevels) {
  BEGIN_RCPP
  IntegerVector factor1(Rfactor1), factor2(Rfactor2);
  if (factor1.size() != factor2.size()) 
    throw std::invalid_argument("Length of factor1 and factor2 are inconsistent");
  IntegerVector retval(factor1.size());
  std::map< std::pair<int, int> , int > index;
  std::pair<int, int> glue;
  int value;
  if (Rlevels != R_NilValue) {
    List levels(Rlevels);
    std::vector<int> level1(as< std::vector<int> >(wrap(levels[0])));
    std::vector<int> level2(as< std::vector<int> >(wrap(levels[1])));
    if (level1.size() != level2.size()) 
      throw std::invalid_argument("Length of level1 and level2 are inconsistent");
    for(int i = 0;i < level1.size();i++) {
      glue.first = level1[i];
      glue.second = level2[i];
      value = index[glue];
      if (value != 0) {
        throw std::invalid_argument("Duplicated level pair");
      }
      index[glue] = i + 1;
    }
    for(int i = 0;i < factor1.size();i++) {
      glue.first = factor1[i];
      glue.second = factor2[i];
      value = index[glue];
      if (value == 0) {
        value = NA_INTEGER;
      }
      retval[i] = value;
    }
    return retval;
  }
  else {
    std::vector<int> level1, level2;
    for(int i = 0;i < factor1.size();i++) {
      glue.first = factor1[i];
      glue.second = factor2[i];
      value = index[glue];
      if (value == 0) {
        index[glue] = level1.size() + 1;
        level1.push_back(glue.first);
        level2.push_back(glue.second);
        value = level1.size();
      }
      retval[i] = value;
    }
    retval.attr("level1") = wrap(level1);
    retval.attr("level2") = wrap(level2);
    return retval;
  }
  END_RCPP
}