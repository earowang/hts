#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/IterativeLinearSolvers>
//#include <Eigen/SparseCore>
//#include <Eigen/Sparse>
using namespace Eigen;
using namespace Rcpp;

using Eigen::SparseMatrix;
using Eigen::MappedSparseMatrix;
using Eigen::Map;
using Eigen::MatrixXd;
using Eigen::VectorXd;
using Rcpp::as;
using Eigen::ConjugateGradient;
typedef Eigen::MappedSparseMatrix<double> MSpMat;

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
MatrixXd cgm_c(SEXP As, SEXP bs) {
  const MSpMat A = as<MSpMat>(As);
  //const Map<MatrixXd> A(as<Map<MatrixXd> > (As));
  const Map<MatrixXd> b(as<Map<MatrixXd> > (bs));
  ConjugateGradient<SparseMatrix<double> > cg;
  cg.setTolerance(1e-06);
  //ConjugateGradient<MatrixXd> cg;
  //cg.compute(A);
  MatrixXd x=cg.compute(A).solve(b);
  return x;
}
