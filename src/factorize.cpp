#include <boost/thread.hpp>
#include <boost/bind.hpp>
#include <Rcpp.h>

using namespace Rcpp;

template <int RTYPE, class T>
void fill_retval(Vector<RTYPE>& src, std::map<T, int>& index, IntegerVector& retval, int thread_number, int thread_no) {
	T glue;
	for(int i = thread_no;i < src.size();i += thread_number) {
		glue = as<T>(wrap(src[i]));
		retval[i] = index[glue];
		if (retval[i] == 0) {
			retval[i] = NA_INTEGER;
		}
	}
}

boost::shared_mutex _access;

template <int RTYPE, class T>
void fill_retval(Vector<RTYPE>& src, std::map<T, int>& index, IntegerVector& retval, int thread_number, int thread_no, std::vector<T>& levels) {
	T glue;
	int value;
	for(int i = thread_no;i < src.size();i += thread_number) {
		glue = as<T>(wrap(src[i]));
		{
			boost::shared_lock<boost::shared_mutex> lock(_access);
			value = index[glue];
		}
		if (value == 0) {
			boost::upgrade_lock<boost::shared_mutex> lock(_access);
			boost::upgrade_to_unique_lock<boost::shared_mutex> unique_lock(lock);
			levels.push_back(glue);
			index[glue] = levels.size();
			value = levels.size();
//			Rcout << value << std::endl;
		}
		retval[i] = value;
	}
}


template <int RTYPE, class T>
SEXP factorize(SEXP Rsrc, SEXP Rindex_src, int thread_number = 0) {
  std::map<T, int> index;
  Vector<RTYPE> src(Rsrc);
  IntegerVector retval(src.size());
  if (Rindex_src == R_NilValue) {
		std::vector<T> levels;
//  	if (thread_number > 0) {
//  		boost::thread_group tgroup;
//  		void (&fun) (Vector<RTYPE>&, std::map<T, int>&, IntegerVector&, int, int, std::vector<T>&) = fill_retval<RTYPE, T>;
//  		for(int i = 0;i < thread_number;i++) {
//		    tgroup.create_thread(boost::bind( fun, boost::ref(src), boost::ref(index), boost::ref(retval), thread_number, i, boost::ref(levels)));
//	    }
//			tgroup.join_all();
//  	}
//  	else {
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
//  	}
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
    if (thread_number > 0) {
	    boost::thread_group tgroup;
	    void (&fun) (Vector<RTYPE>&, std::map<T, int>&, IntegerVector&, int, int) = fill_retval<RTYPE, T>;
	    for(int i = 0;i < thread_number;i++) {
		    tgroup.create_thread(boost::bind( fun, boost::ref(src), boost::ref(index), boost::ref(retval), thread_number, i ));
	    }
			tgroup.join_all();
    }
    else {
	    for(int i = 0;i < src.size();i++) {
	      T glue = as<T>(wrap(src[i]));
	      retval[i] = index[glue];
	      if (retval[i] == 0) {
	        retval[i] = NA_INTEGER;
	      }
	    }
    }
  }
  return retval;
}

RcppExport SEXP factorize_integer(SEXP Rsrc, SEXP Rindex_src, SEXP Rthread_number) {
	BEGIN_RCPP
	return factorize<INTSXP, int>(Rsrc, Rindex_src, as<int>(Rthread_number));
	END_RCPP
}

RcppExport SEXP factorize_character(SEXP Rsrc, SEXP Rindex_src, SEXP Rthread_number) {
  BEGIN_RCPP
  return factorize<STRSXP, std::string>(Rsrc, Rindex_src, as<int>(Rthread_number));
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