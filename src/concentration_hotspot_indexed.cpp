#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <unordered_map>
#include <unordered_set>
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
  const double distance_tolerance =
    1e-9 * std::max(1.0, radius);
  const double radius2_tolerance =
    1e-9 * std::max(1.0, radius2);

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
        if (std::fabs(dx) > radius + distance_tolerance) {
          continue;
        }

        const double dy = y_ref[i] - y_center;
        if (std::fabs(dy) > radius + distance_tolerance) {
          continue;
        }

        const double abs_dx = std::fabs(dx);
        const double abs_dy = std::fabs(dy);
        if (abs_dx + abs_dy <= radius ||
            dx * dx + dy * dy <= radius2 + radius2_tolerance) {
          total += value_ref[i];
        }
      }
    }
  }

  return total;
}

struct PairBestResult {
  double x;
  double y;
  double concentration;
};

struct PairCandidateEvaluation {
  double x[2];
  double y[2];
  double concentration[2];
  bool evaluated[2];
};

struct RasterGeometry {
  double xmin;
  double xmax;
  double ymin;
  double ymax;
  double xres;
  double yres;
  int nrow;
  int ncol;
};

typedef std::unordered_map<int, double> ObservedEvaluationCache;
typedef std::unordered_map<long long, PairCandidateEvaluation>
  PairEvaluationCache;

long long point_pair_key(const int first, const int second) {
  const int lower = std::min(first, second);
  const int upper = std::max(first, second);
  return cell_key(lower, upper);
}

long long raster_cell_at_center(const double x,
                                const double y,
                                const RasterGeometry& raster) {
  if (x < raster.xmin || x > raster.xmax ||
      y < raster.ymin || y > raster.ymax) {
    return -1;
  }
  int col = static_cast<int>(std::floor((x - raster.xmin) / raster.xres));
  int row = static_cast<int>(std::floor((raster.ymax - y) / raster.yres));
  if (col == raster.ncol) {
    col = raster.ncol - 1;
  }
  if (row == raster.nrow) {
    row = raster.nrow - 1;
  }
  if (row < 0 || row >= raster.nrow || col < 0 || col >= raster.ncol) {
    return -1;
  }
  return static_cast<long long>(row) * raster.ncol + col + 1;
}

bool candidate_center_is_selected(
    const double x,
    const double y,
    const bool filter_centres,
    const std::unordered_set<long long>& selected_cells,
    const RasterGeometry& raster) {
  if (!filter_centres) {
    return true;
  }
  const long long cell = raster_cell_at_center(x, y, raster);
  return selected_cells.find(cell) != selected_cells.end();
}

PairBestResult pair_intersection_best_with_index(
    const Rcpp::NumericVector& x_candidates,
    const Rcpp::NumericVector& y_candidates,
    const Rcpp::NumericVector& x_ref,
    const Rcpp::NumericVector& y_ref,
    const Rcpp::NumericVector& value_ref,
    const GridIndex& evaluation_index,
    const double min_x,
    const double min_y,
    const double radius,
    const double cell_width) {

  const double candidate_min_x = vector_min(x_candidates);
  const double candidate_min_y = vector_min(y_candidates);
  const double radius2 = radius * radius;
  const double max_pair_distance2 = 4.0 * radius2;
  const int pair_neighbor_range =
    static_cast<int>(std::ceil((2.0 * radius) / cell_width));
  const int evaluation_neighbor_range =
    static_cast<int>(std::ceil(radius / cell_width));
  GridIndex candidate_index = build_grid_index(
    x_candidates, y_candidates, candidate_min_x, candidate_min_y, cell_width
  );

  PairBestResult best;
  best.x = x_candidates[0];
  best.y = y_candidates[0];
  best.concentration = -std::numeric_limits<double>::infinity();

  for (int i = 0; i < x_candidates.size(); ++i) {
    const double total = indexed_sum_at_center(
      x_candidates[i], y_candidates[i], x_ref, y_ref, value_ref,
      evaluation_index, min_x, min_y, radius, radius2, cell_width,
      evaluation_neighbor_range
    );
    if (total > best.concentration) {
      best.concentration = total;
      best.x = x_candidates[i];
      best.y = y_candidates[i];
    }
  }

  for (int i = 0; i < x_candidates.size(); ++i) {
    const long long gx_center = cell_id(
      x_candidates[i], candidate_min_x, cell_width
    );
    const long long gy_center = cell_id(
      y_candidates[i], candidate_min_y, cell_width
    );

    for (long long gx = gx_center - pair_neighbor_range;
         gx <= gx_center + pair_neighbor_range; ++gx) {
      for (long long gy = gy_center - pair_neighbor_range;
           gy <= gy_center + pair_neighbor_range; ++gy) {
        GridIndex::const_iterator it = candidate_index.find(cell_key(gx, gy));
        if (it == candidate_index.end()) {
          continue;
        }

        const std::vector<int>& ids = it->second;
        for (std::size_t k = 0; k < ids.size(); ++k) {
          const int j = ids[k];
          if (j <= i) {
            continue;
          }

          const double dx = x_candidates[j] - x_candidates[i];
          const double dy = y_candidates[j] - y_candidates[i];
          const double d2 = dx * dx + dy * dy;
          if (d2 <= 0.0 || d2 > max_pair_distance2) {
            continue;
          }

          const double d = std::sqrt(d2);
          const double mx = (x_candidates[i] + x_candidates[j]) / 2.0;
          const double my = (y_candidates[i] + y_candidates[j]) / 2.0;
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
              candidate_x[c], candidate_y[c], x_ref, y_ref, value_ref,
              evaluation_index, min_x, min_y, radius, radius2, cell_width,
              evaluation_neighbor_range
            );
            if (total > best.concentration) {
              best.concentration = total;
              best.x = candidate_x[c];
              best.y = candidate_y[c];
            }
          }
        }
      }
    }
  }

  return best;
}

PairBestResult pair_intersection_best_rows_with_cache(
    const Rcpp::IntegerVector& candidate_rows,
    const Rcpp::NumericVector& x_ref,
    const Rcpp::NumericVector& y_ref,
    const Rcpp::NumericVector& value_ref,
    const GridIndex& evaluation_index,
    const double min_x,
    const double min_y,
    const double radius,
    const double cell_width,
    ObservedEvaluationCache& observed_cache,
    PairEvaluationCache& pair_cache,
    std::size_t& raw_pair_count,
    std::size_t& evaluated_intersection_count,
    const bool filter_centres,
    const std::unordered_set<long long>& selected_cells,
    const RasterGeometry& raster) {

  const int n_candidates = candidate_rows.size();
  Rcpp::NumericVector x_candidates(n_candidates);
  Rcpp::NumericVector y_candidates(n_candidates);
  std::vector<int> reference_rows(n_candidates);
  for (int i = 0; i < n_candidates; ++i) {
    const int row = candidate_rows[i] - 1;
    if (row < 0 || row >= x_ref.size()) {
      Rcpp::stop("Candidate row indices are outside the reference data.");
    }
    reference_rows[i] = row;
    x_candidates[i] = x_ref[row];
    y_candidates[i] = y_ref[row];
  }

  const double candidate_min_x = vector_min(x_candidates);
  const double candidate_min_y = vector_min(y_candidates);
  const double radius2 = radius * radius;
  const double max_pair_distance2 = 4.0 * radius2;
  const int pair_neighbor_range =
    static_cast<int>(std::ceil((2.0 * radius) / cell_width));
  const int evaluation_neighbor_range =
    static_cast<int>(std::ceil(radius / cell_width));
  GridIndex candidate_index = build_grid_index(
    x_candidates, y_candidates, candidate_min_x, candidate_min_y, cell_width
  );

  PairBestResult best;
  best.x = NA_REAL;
  best.y = NA_REAL;
  best.concentration = -std::numeric_limits<double>::infinity();

  for (int i = 0; i < n_candidates; ++i) {
    if (!candidate_center_is_selected(
          x_candidates[i], y_candidates[i], filter_centres,
          selected_cells, raster)) {
      continue;
    }
    const int row = reference_rows[i];
    ObservedEvaluationCache::const_iterator cached = observed_cache.find(row);
    double total;
    if (cached == observed_cache.end()) {
      total = indexed_sum_at_center(
        x_candidates[i], y_candidates[i], x_ref, y_ref, value_ref,
        evaluation_index, min_x, min_y, radius, radius2, cell_width,
        evaluation_neighbor_range
      );
      observed_cache[row] = total;
    } else {
      total = cached->second;
    }
    if (total > best.concentration) {
      best.concentration = total;
      best.x = x_candidates[i];
      best.y = y_candidates[i];
    }
  }

  for (int i = 0; i < n_candidates; ++i) {
    const long long gx_center = cell_id(
      x_candidates[i], candidate_min_x, cell_width
    );
    const long long gy_center = cell_id(
      y_candidates[i], candidate_min_y, cell_width
    );

    for (long long gx = gx_center - pair_neighbor_range;
         gx <= gx_center + pair_neighbor_range; ++gx) {
      for (long long gy = gy_center - pair_neighbor_range;
           gy <= gy_center + pair_neighbor_range; ++gy) {
        GridIndex::const_iterator it = candidate_index.find(cell_key(gx, gy));
        if (it == candidate_index.end()) {
          continue;
        }

        const std::vector<int>& ids = it->second;
        for (std::size_t k = 0; k < ids.size(); ++k) {
          const int j = ids[k];
          if (j <= i) {
            continue;
          }

          const double dx = x_candidates[j] - x_candidates[i];
          const double dy = y_candidates[j] - y_candidates[i];
          const double d2 = dx * dx + dy * dy;
          if (d2 <= 0.0 || d2 > max_pair_distance2) {
            continue;
          }
          ++raw_pair_count;

          const int first_row = reference_rows[i];
          const int second_row = reference_rows[j];
          const int lower_row = std::min(first_row, second_row);
          const int upper_row = std::max(first_row, second_row);
          const long long key = point_pair_key(lower_row, upper_row);
          PairEvaluationCache::iterator cached = pair_cache.find(key);
          if (cached == pair_cache.end()) {
            const double pair_dx = x_ref[upper_row] - x_ref[lower_row];
            const double pair_dy = y_ref[upper_row] - y_ref[lower_row];
            const double pair_d2 = pair_dx * pair_dx + pair_dy * pair_dy;
            const double d = std::sqrt(pair_d2);
            const double mx = (x_ref[lower_row] + x_ref[upper_row]) / 2.0;
            const double my = (y_ref[lower_row] + y_ref[upper_row]) / 2.0;
            const double h = std::sqrt(std::max(0.0,
                                                radius2 - pair_d2 / 4.0));
            const double ux = -pair_dy / d;
            const double uy = pair_dx / d;
            PairCandidateEvaluation evaluation;
            evaluation.x[0] = mx + h * ux;
            evaluation.x[1] = mx - h * ux;
            evaluation.y[0] = my + h * uy;
            evaluation.y[1] = my - h * uy;
            for (int c = 0; c < 2; ++c) {
              // Pair endpoints generate two possible centres. Screen each
              // centre by its own terra cell before the more expensive exact
              // radius sum; the two centres need not share a raster cell.
              evaluation.evaluated[c] = candidate_center_is_selected(
                evaluation.x[c], evaluation.y[c], filter_centres,
                selected_cells, raster
              );
              if (evaluation.evaluated[c]) {
                evaluation.concentration[c] = indexed_sum_at_center(
                  evaluation.x[c], evaluation.y[c], x_ref, y_ref, value_ref,
                  evaluation_index, min_x, min_y, radius, radius2, cell_width,
                  evaluation_neighbor_range
                );
                ++evaluated_intersection_count;
              } else {
                evaluation.concentration[c] =
                  -std::numeric_limits<double>::infinity();
              }
            }
            cached = pair_cache.emplace(key, evaluation).first;
          }

          const bool canonical_order = first_row == lower_row;
          for (int c = 0; c < 2; ++c) {
            const int cached_center = canonical_order ? c : 1 - c;
            const PairCandidateEvaluation& evaluation = cached->second;
            if (!evaluation.evaluated[cached_center]) {
              continue;
            }
            if (evaluation.concentration[cached_center] > best.concentration) {
              best.concentration = evaluation.concentration[cached_center];
              best.x = evaluation.x[cached_center];
              best.y = evaluation.y[cached_center];
            }
          }
        }
      }
    }
  }

  return best;
}

Rcpp::DataFrame indexed_points_at_center(
    const double x_center,
    const double y_center,
    const Rcpp::NumericVector& x_ref,
    const Rcpp::NumericVector& y_ref,
    const Rcpp::NumericVector& value_ref,
    const Rcpp::IntegerVector& ix_ref,
    const GridIndex& index,
    const double min_x,
    const double min_y,
    const double radius,
    const double cell_width) {

  const double radius2 = radius * radius;
  const double distance_tolerance = 1e-9 * std::max(1.0, radius);
  const double radius2_tolerance = 1e-9 * std::max(1.0, radius2);
  const int neighbor_range =
    static_cast<int>(std::ceil(radius / cell_width));
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
        const double dy = y_ref[i] - y_center;
        if (std::fabs(dx) > radius + distance_tolerance ||
            std::fabs(dy) > radius + distance_tolerance) {
          continue;
        }
        const double distance2 = dx * dx + dy * dy;
        if (distance2 <= radius2 + radius2_tolerance) {
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
  const double distance_tolerance =
    1e-9 * std::max(1.0, radius);
  const double radius2_tolerance =
    1e-9 * std::max(1.0, radius2);
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
        if (std::fabs(dx) > radius + distance_tolerance) {
          continue;
        }

        const double dy = y_ref[i] - y_center;
        if (std::fabs(dy) > radius + distance_tolerance) {
          continue;
        }

        const double distance2 = dx * dx + dy * dy;
        if (distance2 <= radius2 + radius2_tolerance) {
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
    Rcpp::NumericVector x_candidates,
    Rcpp::NumericVector y_candidates,
    Rcpp::NumericVector x_ref,
    Rcpp::NumericVector y_ref,
    Rcpp::NumericVector value_ref,
    double radius,
    double cell_width) {

  check_numeric_inputs(x_candidates, y_candidates,
                       "x_candidates", "y_candidates");
  if (x_ref.size() != y_ref.size() || x_ref.size() != value_ref.size()) {
    Rcpp::stop("Reference vectors must have the same length.");
  }

  if (!std::isfinite(radius) || radius <= 0.0) {
    Rcpp::stop("`radius` must be a single finite positive number.");
  }
  if (!std::isfinite(cell_width) || cell_width <= 0.0) {
    Rcpp::stop("`cell_width` must be a single finite positive number.");
  }
  if (x_candidates.size() == 0) {
    Rcpp::stop("Candidate vectors must not be empty.");
  }
  if (x_ref.size() == 0) {
    Rcpp::stop("Reference vectors must not be empty.");
  }
  const double min_x = vector_min(x_ref);
  const double min_y = vector_min(y_ref);
  GridIndex index = build_grid_index(x_ref, y_ref, min_x, min_y, cell_width);
  PairBestResult best = pair_intersection_best_with_index(
    x_candidates, y_candidates, x_ref, y_ref, value_ref, index,
    min_x, min_y, radius, cell_width
  );

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = best.x,
    Rcpp::Named("y") = best.y,
    Rcpp::Named("concentration") = best.concentration
  );
}

// [[Rcpp::export]]
Rcpp::List pair_intersection_best_groups_cpp(
    Rcpp::List candidate_rows,
    Rcpp::NumericVector x_ref,
    Rcpp::NumericVector y_ref,
    Rcpp::NumericVector value_ref,
    Rcpp::IntegerVector ix_ref,
    double radius,
    double cell_width,
    Rcpp::IntegerVector selected_cell_ids,
    Rcpp::NumericVector raster_geometry,
    bool filter_centres) {

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
  if (raster_geometry.size() != 8) {
    Rcpp::stop("`raster_geometry` must contain eight values.");
  }

  RasterGeometry raster = {
    raster_geometry[0], raster_geometry[1], raster_geometry[2],
    raster_geometry[3], raster_geometry[4], raster_geometry[5],
    static_cast<int>(raster_geometry[6]),
    static_cast<int>(raster_geometry[7])
  };
  std::unordered_set<long long> selected_cells;
  selected_cells.reserve(static_cast<std::size_t>(selected_cell_ids.size()));
  for (int i = 0; i < selected_cell_ids.size(); ++i) {
    selected_cells.insert(selected_cell_ids[i]);
  }
  if (filter_centres && selected_cells.empty()) {
    Rcpp::stop("Centre filtering requires selected raster cells.");
  }

  const double min_x = vector_min(x_ref);
  const double min_y = vector_min(y_ref);
  GridIndex evaluation_index = build_grid_index(
    x_ref, y_ref, min_x, min_y, cell_width
  );
  const int n_groups = candidate_rows.size();
  Rcpp::NumericVector best_x(n_groups);
  Rcpp::NumericVector best_y(n_groups);
  Rcpp::NumericVector concentration(n_groups);
  Rcpp::List selected(n_groups);
  ObservedEvaluationCache observed_cache;
  PairEvaluationCache pair_cache;
  std::size_t raw_observed_count = 0;
  std::size_t raw_pair_count = 0;
  std::size_t evaluated_intersection_count = 0;

  for (int g = 0; g < n_groups; ++g) {
    Rcpp::IntegerVector rows = candidate_rows[g];
    if (rows.size() == 0) {
      Rcpp::stop("Candidate groups must not be empty.");
    }
    raw_observed_count += static_cast<std::size_t>(rows.size());

    PairBestResult best = pair_intersection_best_rows_with_cache(
      rows, x_ref, y_ref, value_ref, evaluation_index, min_x, min_y,
      radius, cell_width, observed_cache, pair_cache, raw_pair_count,
      evaluated_intersection_count, filter_centres, selected_cells, raster
    );
    best_x[g] = best.x;
    best_y[g] = best.y;
    concentration[g] = best.concentration;
    if (std::isfinite(best.x) && std::isfinite(best.y)) {
      selected[g] = indexed_points_at_center(
        best.x, best.y, x_ref, y_ref, value_ref, ix_ref, evaluation_index,
        min_x, min_y, radius, cell_width
      );
    } else {
      selected[g] = Rcpp::DataFrame::create(
        Rcpp::Named("ix") = Rcpp::IntegerVector(),
        Rcpp::Named("distance_m") = Rcpp::NumericVector(),
        Rcpp::Named("value") = Rcpp::NumericVector()
      );
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("x") = best_x,
    Rcpp::Named("y") = best_y,
    Rcpp::Named("concentration") = concentration,
    Rcpp::Named("selected") = selected,
    Rcpp::Named("diagnostics") = Rcpp::List::create(
      Rcpp::Named("raw_observed_centres") =
        static_cast<double>(raw_observed_count),
      Rcpp::Named("unique_observed_centres") =
        static_cast<double>(observed_cache.size()),
      Rcpp::Named("raw_point_pairs") = static_cast<double>(raw_pair_count),
      Rcpp::Named("unique_point_pairs") =
        static_cast<double>(pair_cache.size()),
      Rcpp::Named("evaluated_centres") =
        static_cast<double>(observed_cache.size() +
                            evaluated_intersection_count),
      Rcpp::Named("evaluated_observed_centres") =
        static_cast<double>(observed_cache.size()),
      Rcpp::Named("evaluated_intersection_centres") =
        static_cast<double>(evaluated_intersection_count),
      Rcpp::Named("centre_filter_applied") = filter_centres
    )
  );
}
