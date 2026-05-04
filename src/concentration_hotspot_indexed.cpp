#include <Rcpp.h>
#include <cmath>
#include <cstdint>
#include <limits>
#include <unordered_map>
#include <vector>

namespace {

typedef std::unordered_map<long long, std::vector<int> > GridIndex;

long long cell_key(const long long gx, const long long gy) {
  const std::uint64_t ux = static_cast<std::uint32_t>(gx);
  const std::uint64_t uy = static_cast<std::uint32_t>(gy);
  return static_cast<long long>((ux << 32) | uy);
}

long long cell_id(const double value, const double min_value,
                  const double cell_width) {
  return static_cast<long long>(std::floor((value - min_value) / cell_width));
}

void check_numeric_inputs(const Rcpp::NumericVector& x,
                          const Rcpp::NumericVector& y,
                          const char* x_name,
                          const char* y_name) {
  if (x.size() != y.size()) {
    Rcpp::stop("%s and %s must have the same length.", x_name, y_name);
  }
}

void check_reference_inputs(const Rcpp::NumericVector& x_ref,
                            const Rcpp::NumericVector& y_ref,
                            const Rcpp::NumericVector& value_ref,
                            const Rcpp::IntegerVector& ix_ref) {
  const int n = x_ref.size();
  if (y_ref.size() != n || value_ref.size() != n || ix_ref.size() != n) {
    Rcpp::stop("Reference vectors must have the same length.");
  }
}

double vector_min(const Rcpp::NumericVector& x) {
  if (x.size() == 0) {
    Rcpp::stop("Reference vectors must not be empty.");
  }

  double out = x[0];
  for (int i = 1; i < x.size(); ++i) {
    if (x[i] < out) {
      out = x[i];
    }
  }
  return out;
}

GridIndex build_grid_index(const Rcpp::NumericVector& x_ref,
                           const Rcpp::NumericVector& y_ref,
                           const double min_x,
                           const double min_y,
                           const double cell_width) {
  GridIndex index;
  index.reserve(static_cast<std::size_t>(x_ref.size() * 1.3));

  for (int i = 0; i < x_ref.size(); ++i) {
    const long long gx = cell_id(x_ref[i], min_x, cell_width);
    const long long gy = cell_id(y_ref[i], min_y, cell_width);
    index[cell_key(gx, gy)].push_back(i);
  }

  return index;
}

double indexed_sum_at_center(const double x_center,
                             const double y_center,
                             const Rcpp::NumericVector& x_ref,
                             const Rcpp::NumericVector& y_ref,
                             const Rcpp::NumericVector& value_ref,
                             const GridIndex& index,
                             const double min_x,
                             const double min_y,
                             const double radius,
                             const double radius2,
                             const double cell_width,
                             const int neighbor_range) {
  const long long gx_center = cell_id(x_center, min_x, cell_width);
  const long long gy_center = cell_id(y_center, min_y, cell_width);

  double total = 0.0;

  for (long long gx = gx_center - neighbor_range;
       gx <= gx_center + neighbor_range; ++gx) {
    for (long long gy = gy_center - neighbor_range;
         gy <= gy_center + neighbor_range; ++gy) {

      GridIndex::const_iterator it = index.find(cell_key(gx, gy));
      if (it == index.end()) {
        continue;
      }

      const std::vector<int>& ids = it->second;
      for (std::size_t k = 0; k < ids.size(); ++k) {
        const int i = ids[k];
        const double dx = x_ref[i] - x_center;
        if (std::fabs(dx) > radius) {
          continue;
        }

        const double dy = y_ref[i] - y_center;
        if (std::fabs(dy) > radius) {
          continue;
        }

        const double abs_dx = std::fabs(dx);
        const double abs_dy = std::fabs(dy);
        if (abs_dx + abs_dy <= radius ||
            dx * dx + dy * dy <= radius2) {
          total += value_ref[i];
        }
      }
    }
  }

  return total;
}

} // namespace

// [[Rcpp::export]]
Rcpp::DataFrame indexed_concentration_best_cpp(
    Rcpp::NumericVector x_candidates,
    Rcpp::NumericVector y_candidates,
    Rcpp::NumericVector x_ref,
    Rcpp::NumericVector y_ref,
    Rcpp::NumericVector value_ref,
    Rcpp::IntegerVector ix_ref,
    double radius,
    double cell_width) {

  check_numeric_inputs(x_candidates, y_candidates,
                       "x_candidates", "y_candidates");
  check_reference_inputs(x_ref, y_ref, value_ref, ix_ref);

  if (x_candidates.size() == 0) {
    Rcpp::stop("Candidate vectors must not be empty.");
  }
  if (!std::isfinite(radius) || radius <= 0.0) {
    Rcpp::stop("`radius` must be a single finite positive number.");
  }
  if (!std::isfinite(cell_width) || cell_width <= 0.0) {
    Rcpp::stop("`cell_width` must be a single finite positive number.");
  }

  const double min_x = vector_min(x_ref);
  const double min_y = vector_min(y_ref);
  const double radius2 = radius * radius;
  const int neighbor_range =
    static_cast<int>(std::ceil(radius / cell_width));

  GridIndex index = build_grid_index(x_ref, y_ref, min_x, min_y, cell_width);

  double best_concentration = -std::numeric_limits<double>::infinity();
  int best_candidate = 0;

  for (int j = 0; j < x_candidates.size(); ++j) {
    const double total = indexed_sum_at_center(
      x_candidates[j],
                  y_candidates[j],
                              x_ref,
                                    y_ref,
                                           value_ref,
                                                     index,
                                                           min_x,
                                                                 min_y,
                                                                       radius,
                                                                              radius2,
                                                                                      cell_width,
                                                                                                 neighbor_range
    );

    if (total > best_concentration) {
      best_concentration = total;
      best_candidate = j;
    }
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("candidate_index") = best_candidate + 1,
    Rcpp::Named("x") = x_candidates[best_candidate],
    Rcpp::Named("y") = y_candidates[best_candidate],
    Rcpp::Named("concentration") = best_concentration
  );
}

// [[Rcpp::export]]
Rcpp::DataFrame indexed_points_in_radius_cpp(
    double x_center,
    double y_center,
    Rcpp::NumericVector x_ref,
    Rcpp::NumericVector y_ref,
    Rcpp::NumericVector value_ref,
    Rcpp::IntegerVector ix_ref,
    double radius,
    double cell_width) {

  check_reference_inputs(x_ref, y_ref, value_ref, ix_ref);

  if (!std::isfinite(radius) || radius <= 0.0) {
    Rcpp::stop("`radius` must be a single finite positive number.");
  }
  if (!std::isfinite(cell_width) || cell_width <= 0.0) {
    Rcpp::stop("`cell_width` must be a single finite positive number.");
  }

  const double min_x = vector_min(x_ref);
  const double min_y = vector_min(y_ref);
  const double radius2 = radius * radius;
  const int neighbor_range =
    static_cast<int>(std::ceil(radius / cell_width));

  GridIndex index = build_grid_index(x_ref, y_ref, min_x, min_y, cell_width);

  const long long gx_center = cell_id(x_center, min_x, cell_width);
  const long long gy_center = cell_id(y_center, min_y, cell_width);

  std::vector<int> ix_out;
  std::vector<double> distance_out;
  std::vector<double> value_out;

  for (long long gx = gx_center - neighbor_range;
       gx <= gx_center + neighbor_range; ++gx) {
    for (long long gy = gy_center - neighbor_range;
         gy <= gy_center + neighbor_range; ++gy) {

      GridIndex::const_iterator it = index.find(cell_key(gx, gy));
      if (it == index.end()) {
        continue;
      }

      const std::vector<int>& ids = it->second;
      for (std::size_t k = 0; k < ids.size(); ++k) {
        const int i = ids[k];
        const double dx = x_ref[i] - x_center;
        if (std::fabs(dx) > radius) {
          continue;
        }

        const double dy = y_ref[i] - y_center;
        if (std::fabs(dy) > radius) {
          continue;
        }

        const double distance2 = dx * dx + dy * dy;
        if (distance2 <= radius2) {
          ix_out.push_back(ix_ref[i]);
          distance_out.push_back(std::sqrt(distance2));
          value_out.push_back(value_ref[i]);
        }
      }
    }
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("ix") = ix_out,
    Rcpp::Named("distance_m") = distance_out,
    Rcpp::Named("value") = value_out
  );
}

// [[Rcpp::export]]
Rcpp::DataFrame pair_intersection_best_cpp(
    Rcpp::NumericVector x_ref,
    Rcpp::NumericVector y_ref,
    Rcpp::NumericVector value_ref,
    Rcpp::IntegerVector ix_ref,
    double radius,
    double cell_width) {

  check_reference_inputs(x_ref, y_ref, value_ref, ix_ref);

  if (!std::isfinite(radius) || radius <= 0.0) {
    Rcpp::stop("`radius` must be a single finite positive number.");
  }
  if (!std::isfinite(cell_width) || cell_width <= 0.0) {
    Rcpp::stop("`cell_width` must be a single finite positive number.");
  }
  if (x_ref.size() == 0) {
    Rcpp::stop("Reference vectors must not be empty.");
  }

  const double min_x = vector_min(x_ref);
  const double min_y = vector_min(y_ref);
  const double radius2 = radius * radius;
  const double max_pair_distance2 = 4.0 * radius2;
  const int neighbor_range =
    static_cast<int>(std::ceil((2.0 * radius) / cell_width));

  GridIndex index = build_grid_index(x_ref, y_ref, min_x, min_y, cell_width);

  double best_concentration = -std::numeric_limits<double>::infinity();
  double best_x = x_ref[0];
  double best_y = y_ref[0];

  for (int i = 0; i < x_ref.size(); ++i) {
    const double total = indexed_sum_at_center(
      x_ref[i], y_ref[i], x_ref, y_ref, value_ref, index, min_x, min_y,
      radius, radius2, cell_width,
      static_cast<int>(std::ceil(radius / cell_width))
    );

    if (total > best_concentration) {
      best_concentration = total;
      best_x = x_ref[i];
      best_y = y_ref[i];
    }
  }

  for (int i = 0; i < x_ref.size(); ++i) {
    const long long gx_center = cell_id(x_ref[i], min_x, cell_width);
    const long long gy_center = cell_id(y_ref[i], min_y, cell_width);

    for (long long gx = gx_center - neighbor_range;
         gx <= gx_center + neighbor_range; ++gx) {
      for (long long gy = gy_center - neighbor_range;
           gy <= gy_center + neighbor_range; ++gy) {

        GridIndex::const_iterator it = index.find(cell_key(gx, gy));
        if (it == index.end()) {
          continue;
        }

        const std::vector<int>& ids = it->second;
        for (std::size_t k = 0; k < ids.size(); ++k) {
          const int j = ids[k];
          if (j <= i) {
            continue;
          }

          const double dx = x_ref[j] - x_ref[i];
          const double dy = y_ref[j] - y_ref[i];
          const double d2 = dx * dx + dy * dy;

          if (d2 <= 0.0 || d2 > max_pair_distance2) {
            continue;
          }

          const double d = std::sqrt(d2);
          const double mx = (x_ref[i] + x_ref[j]) / 2.0;
          const double my = (y_ref[i] + y_ref[j]) / 2.0;
          const double h2 = radius2 - d2 / 4.0;
          if (h2 < 0.0) {
            continue;
          }

          const double h = std::sqrt(h2);
          const double ux = -dy / d;
          const double uy = dx / d;
          const double candidate_x[2] = {mx + h * ux, mx - h * ux};
          const double candidate_y[2] = {my + h * uy, my - h * uy};

          for (int c = 0; c < 2; ++c) {
            const double total = indexed_sum_at_center(
              candidate_x[c], candidate_y[c],
              x_ref, y_ref, value_ref, index, min_x, min_y, radius, radius2,
              cell_width, static_cast<int>(std::ceil(radius / cell_width))
            );

            if (total > best_concentration) {
              best_concentration = total;
              best_x = candidate_x[c];
              best_y = candidate_y[c];
            }
          }
        }
      }
    }
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = best_x,
    Rcpp::Named("y") = best_y,
    Rcpp::Named("concentration") = best_concentration
  );
}
