#include <Rcpp.h>
#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <limits>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace {

typedef std::unordered_map<long long, std::vector<int> > GridIndex;

const double kDistanceRelativeTolerance = 1e-9;
const double kAngularEventTolerance = 1e-12;
const double kObjectiveComparisonTolerance = 1e-10;

double distance_tolerance(const double radius) {
  return kDistanceRelativeTolerance * std::max(1.0, radius);
}

double squared_distance_tolerance(const double radius2) {
  return kDistanceRelativeTolerance * std::max(1.0, radius2);
}

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
  const double coordinate_tolerance = distance_tolerance(radius);
  const double radius2_tolerance = squared_distance_tolerance(radius2);

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
        if (std::fabs(dx) > radius + coordinate_tolerance) {
          continue;
        }

        const double dy = y_ref[i] - y_center;
        if (std::fabs(dy) > radius + coordinate_tolerance) {
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

struct PairProfile {
  bool enabled;
  double index_seconds;
  double local_setup_seconds;
  double observed_scoring_seconds;
  double pair_loop_seconds;
  double exact_scoring_seconds;
  double result_extraction_seconds;
  std::size_t pairs_considered;
  std::size_t pairs_within_2r;
  std::size_t intersections_generated;
  std::size_t intersections_rejected;
  std::size_t max_angular_events;

  explicit PairProfile(const bool profile) :
    enabled(profile), index_seconds(0.0), local_setup_seconds(0.0),
    observed_scoring_seconds(0.0), pair_loop_seconds(0.0),
    exact_scoring_seconds(0.0), result_extraction_seconds(0.0),
    pairs_considered(0), pairs_within_2r(0),
    intersections_generated(0), intersections_rejected(0),
    max_angular_events(0) {}
};

struct AngularEvent {
  double angle;
  double weight;
  bool starts;
  bool candidate_endpoint;
};

bool angular_event_less(const AngularEvent& lhs, const AngularEvent& rhs) {
  return lhs.angle < rhs.angle;
}

typedef std::chrono::steady_clock ProfileClock;

double elapsed_seconds(const ProfileClock::time_point& start) {
  return std::chrono::duration<double>(ProfileClock::now() - start).count();
}

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
          const double h = std::sqrt(std::max(0.0, radius2 - d2 / 4.0));
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
    PairProfile& profile,
    const bool filter_centres,
    const std::unordered_set<long long>& selected_cells,
    const RasterGeometry& raster) {

  const ProfileClock::time_point setup_start = ProfileClock::now();
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
  if (profile.enabled) {
    profile.local_setup_seconds += elapsed_seconds(setup_start);
  }

  PairBestResult best;
  best.x = NA_REAL;
  best.y = NA_REAL;
  best.concentration = -std::numeric_limits<double>::infinity();

  const ProfileClock::time_point observed_start = ProfileClock::now();
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
      const ProfileClock::time_point score_start = ProfileClock::now();
      total = indexed_sum_at_center(
        x_candidates[i], y_candidates[i], x_ref, y_ref, value_ref,
        evaluation_index, min_x, min_y, radius, radius2, cell_width,
        evaluation_neighbor_range
      );
      if (profile.enabled) {
        profile.exact_scoring_seconds += elapsed_seconds(score_start);
      }
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
  if (profile.enabled) {
    profile.observed_scoring_seconds += elapsed_seconds(observed_start);
  }

  const ProfileClock::time_point pair_start = ProfileClock::now();
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
          if (profile.enabled) {
            ++profile.pairs_considered;
          }

          const double dx = x_candidates[j] - x_candidates[i];
          const double dy = y_candidates[j] - y_candidates[i];
          const double d2 = dx * dx + dy * dy;
          if (d2 <= 0.0 || d2 > max_pair_distance2) {
            continue;
          }
          ++raw_pair_count;
          if (profile.enabled) {
            ++profile.pairs_within_2r;
          }

          const int first_row = reference_rows[i];
          const int second_row = reference_rows[j];
          const int lower_row = std::min(first_row, second_row);
          const int upper_row = std::max(first_row, second_row);
          const long long key = point_pair_key(lower_row, upper_row);
          PairEvaluationCache::iterator cached = pair_cache.find(key);
          if (cached == pair_cache.end()) {
            if (profile.enabled) {
              profile.intersections_generated += 2;
            }
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
                const ProfileClock::time_point score_start =
                  ProfileClock::now();
                evaluation.concentration[c] = indexed_sum_at_center(
                  evaluation.x[c], evaluation.y[c], x_ref, y_ref, value_ref,
                  evaluation_index, min_x, min_y, radius, radius2, cell_width,
                  evaluation_neighbor_range
                );
                if (profile.enabled) {
                  profile.exact_scoring_seconds += elapsed_seconds(score_start);
                }
                ++evaluated_intersection_count;
              } else {
                if (profile.enabled) {
                  ++profile.intersections_rejected;
                }
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
  if (profile.enabled) {
    profile.pair_loop_seconds += elapsed_seconds(pair_start);
  }

  return best;
}

PairBestResult pair_intersection_best_union_sweep(
    const Rcpp::List& candidate_rows,
    const Rcpp::NumericVector& x_ref,
    const Rcpp::NumericVector& y_ref,
    const Rcpp::NumericVector& value_ref,
    const GridIndex& evaluation_index,
    const double min_x,
    const double min_y,
    const double radius,
    const double cell_width,
    PairProfile& profile,
    const bool filter_centres,
    const std::unordered_set<long long>& selected_cells,
    const RasterGeometry& raster,
    std::size_t& unique_observed_count,
    std::size_t& evaluated_observed_count,
    std::size_t& unique_pair_count,
    std::size_t& evaluated_intersection_count) {

  const int n_ref = x_ref.size();
  std::vector<unsigned char> is_candidate(n_ref, 0);
  std::vector<int> candidates;
  for (int g = 0; g < candidate_rows.size(); ++g) {
    Rcpp::IntegerVector rows = candidate_rows[g];
    for (int k = 0; k < rows.size(); ++k) {
      const int row = rows[k] - 1;
      if (row < 0 || row >= n_ref) {
        Rcpp::stop("Candidate row indices are outside the reference data.");
      }
      if (!is_candidate[row]) {
        is_candidate[row] = 1;
        candidates.push_back(row);
      }
    }
  }
  std::sort(candidates.begin(), candidates.end());
  unique_observed_count = candidates.size();

  const double radius2 = radius * radius;
  const double max_pair_distance2 = 4.0 * radius2;
  const int pair_neighbor_range =
    static_cast<int>(std::ceil((2.0 * radius) / cell_width));
  const int evaluation_neighbor_range =
    static_cast<int>(std::ceil(radius / cell_width));
  const double two_pi = 2.0 * std::acos(-1.0);

  PairBestResult best;
  best.x = NA_REAL;
  best.y = NA_REAL;
  best.concentration = -std::numeric_limits<double>::infinity();

  const ProfileClock::time_point observed_start = ProfileClock::now();
  for (std::size_t pos = 0; pos < candidates.size(); ++pos) {
    const int row = candidates[pos];
    if (!candidate_center_is_selected(
          x_ref[row], y_ref[row], filter_centres, selected_cells, raster)) {
      continue;
    }
    const ProfileClock::time_point score_start = ProfileClock::now();
    const double total = indexed_sum_at_center(
      x_ref[row], y_ref[row], x_ref, y_ref, value_ref, evaluation_index,
      min_x, min_y, radius, radius2, cell_width, evaluation_neighbor_range
    );
    if (profile.enabled) {
      profile.exact_scoring_seconds += elapsed_seconds(score_start);
    }
    ++evaluated_observed_count;
    if (total > best.concentration) {
      best.concentration = total;
      best.x = x_ref[row];
      best.y = y_ref[row];
    }
  }
  if (profile.enabled) {
    profile.observed_scoring_seconds += elapsed_seconds(observed_start);
  }

  const ProfileClock::time_point pair_start = ProfileClock::now();
  std::vector<AngularEvent> events;
  // For centres at distance r from an anchor, every nearby active point
  // defines the closed angular interval over which it is covered. Sweeping
  // the interval endpoints visits the same pair-boundary events as explicit
  // radius-r circle intersections, while exact scoring still uses all active
  // points rather than only the candidate-generation subset.
  for (std::size_t pos = 0; pos < candidates.size(); ++pos) {
    const int anchor = candidates[pos];
    const long long gx_anchor = cell_id(x_ref[anchor], min_x, cell_width);
    const long long gy_anchor = cell_id(y_ref[anchor], min_y, cell_width);
    double current = value_ref[anchor];
    events.clear();

    for (long long gx = gx_anchor - pair_neighbor_range;
         gx <= gx_anchor + pair_neighbor_range; ++gx) {
      for (long long gy = gy_anchor - pair_neighbor_range;
           gy <= gy_anchor + pair_neighbor_range; ++gy) {
        GridIndex::const_iterator cell =
          evaluation_index.find(cell_key(gx, gy));
        if (cell == evaluation_index.end()) {
          continue;
        }
        const std::vector<int>& ids = cell->second;
        for (std::size_t k = 0; k < ids.size(); ++k) {
          const int other = ids[k];
          if (other == anchor) {
            continue;
          }
          const double dx = x_ref[other] - x_ref[anchor];
          const double dy = y_ref[other] - y_ref[anchor];
          const double d2 = dx * dx + dy * dy;
          if (d2 <= 0.0) {
            current += value_ref[other];
            continue;
          }
          const bool generates_candidate =
            is_candidate[other] && other > anchor;
          if (generates_candidate) {
            ++profile.pairs_considered;
          }
          if (d2 > max_pair_distance2) {
            continue;
          }
          if (generates_candidate) {
            ++profile.pairs_within_2r;
            ++unique_pair_count;
            profile.intersections_generated += 2;
          }

          const double distance = std::sqrt(d2);
          const double theta = std::atan2(dy, dx);
          const double alpha = std::acos(std::min(1.0,
                                                  distance / (2.0 * radius)));
          double start = theta - alpha;
          double end = theta + alpha;
          while (start < 0.0) start += two_pi;
          while (start >= two_pi) start -= two_pi;
          while (end < 0.0) end += two_pi;
          while (end >= two_pi) end -= two_pi;
          if (start > end) {
            current += value_ref[other];
          }
          events.push_back(AngularEvent{
            start, value_ref[other], true, generates_candidate
          });
          events.push_back(AngularEvent{
            end, value_ref[other], false, generates_candidate
          });
        }
      }
    }

    profile.max_angular_events = std::max(
      profile.max_angular_events, events.size()
    );

    std::sort(events.begin(), events.end(), angular_event_less);
    std::size_t event = 0;
    while (event < events.size()) {
      const double angle = events[event].angle;
      std::size_t group_end = event + 1;
      while (group_end < events.size() &&
             std::fabs(events[group_end].angle - angle) <=
               kAngularEventTolerance) {
        ++group_end;
      }
      bool candidate_endpoint = false;
      for (std::size_t k = event; k < group_end; ++k) {
        if (events[k].starts) {
          current += events[k].weight;
        }
        candidate_endpoint = candidate_endpoint ||
          events[k].candidate_endpoint;
      }

      if (candidate_endpoint) {
        const double candidate_x = x_ref[anchor] + radius * std::cos(angle);
        const double candidate_y = y_ref[anchor] + radius * std::sin(angle);
        if (candidate_center_is_selected(
              candidate_x, candidate_y, filter_centres,
              selected_cells, raster)) {
          // The sweep total determines whether an exact radius query can still
          // match the incumbent. This guard prevents round-off in interval
          // updates from suppressing a potentially improving confirmation.
          const double comparison_tolerance = kObjectiveComparisonTolerance *
            std::max(1.0, std::fabs(best.concentration));
          if (current + comparison_tolerance >= best.concentration) {
            const ProfileClock::time_point score_start = ProfileClock::now();
            const double exact_total = indexed_sum_at_center(
              candidate_x, candidate_y, x_ref, y_ref, value_ref,
              evaluation_index, min_x, min_y, radius, radius2, cell_width,
              evaluation_neighbor_range
            );
            if (profile.enabled) {
              profile.exact_scoring_seconds += elapsed_seconds(score_start);
            }
            ++evaluated_intersection_count;
            if (exact_total > best.concentration) {
              best.concentration = exact_total;
              best.x = candidate_x;
              best.y = candidate_y;
            }
          }
        } else if (profile.enabled) {
          ++profile.intersections_rejected;
        }
      }

      for (std::size_t k = event; k < group_end; ++k) {
        if (!events[k].starts) {
          current -= events[k].weight;
        }
      }
      event = group_end;
    }
  }
  if (profile.enabled) {
    profile.pair_loop_seconds += elapsed_seconds(pair_start);
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
  const double coordinate_tolerance = distance_tolerance(radius);
  const double radius2_tolerance = squared_distance_tolerance(radius2);
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
        if (std::fabs(dx) > radius + coordinate_tolerance ||
            std::fabs(dy) > radius + coordinate_tolerance) {
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
  const double coordinate_tolerance = distance_tolerance(radius);
  const double radius2_tolerance = squared_distance_tolerance(radius2);
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
        if (std::fabs(dx) > radius + coordinate_tolerance) {
          continue;
        }

        const double dy = y_ref[i] - y_center;
        if (std::fabs(dy) > radius + coordinate_tolerance) {
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
Rcpp::NumericVector cell_point_upper_bounds_cpp(
    Rcpp::IntegerVector candidate_cell_ids,
    Rcpp::NumericVector x_candidates,
    Rcpp::NumericVector y_candidates,
    Rcpp::IntegerVector point_cell_ids,
    Rcpp::NumericVector x_ref,
    Rcpp::NumericVector y_ref,
    Rcpp::NumericVector value_ref,
    double radius, double xres, double yres,
    int raster_nrow, int raster_ncol) {
  check_numeric_inputs(x_candidates, y_candidates, "x_candidates", "y_candidates");
  if (candidate_cell_ids.size() != x_candidates.size() ||
      point_cell_ids.size() != x_ref.size() || y_ref.size() != x_ref.size() ||
      value_ref.size() != x_ref.size()) {
    Rcpp::stop("Cell ids, coordinates and values must have matching lengths.");
  }
  if (!std::isfinite(radius) || radius <= 0 ||
      !std::isfinite(xres) || xres <= 0 || !std::isfinite(yres) || yres <= 0 ||
      raster_nrow <= 0 || raster_ncol <= 0) {
    Rcpp::stop("Raster geometry and radius must be positive.");
  }
  // Reuse terra's point-to-cell assignment, not a new coordinate partition.
  std::unordered_map<int, std::vector<int> > rows_by_cell;
  for (int i = 0; i < x_ref.size(); ++i) {
    if (!std::isfinite(value_ref[i]) || value_ref[i] < 0 ||
        !std::isfinite(x_ref[i]) || !std::isfinite(y_ref[i]) ||
        point_cell_ids[i] < 1 ||
        static_cast<double>(point_cell_ids[i]) >
          static_cast<double>(raster_nrow) * raster_ncol) {
      Rcpp::stop("Upper bounds require finite non-negative values and valid point cells.");
    }
    rows_by_cell[point_cell_ids[i]].push_back(i);
  }
  const double radius2 = radius * radius;
  const double limit2 = radius2 + squared_distance_tolerance(radius2);
  // Include the adjacent boundary cells even when radius / resolution is integer.
  const int col_range = static_cast<int>(std::ceil(radius / xres)) + 2;
  const int row_range = static_cast<int>(std::ceil(radius / yres)) + 2;
  Rcpp::NumericVector bounds(candidate_cell_ids.size());
  for (int k = 0; k < candidate_cell_ids.size(); ++k) {
    if (k % 256 == 0) Rcpp::checkUserInterrupt();
    if (candidate_cell_ids[k] < 1 || static_cast<double>(candidate_cell_ids[k]) >
        static_cast<double>(raster_nrow) * raster_ncol ||
        !std::isfinite(x_candidates[k]) || !std::isfinite(y_candidates[k])) {
      Rcpp::stop("Candidate cell is outside the raster or has non-finite coordinates.");
    }
    const int cell0 = candidate_cell_ids[k] - 1;
    const int cr = cell0 / raster_ncol, cc = cell0 % raster_ncol;
    long double total = 0;
    for (int row = std::max(0, cr - row_range);
         row <= std::min(raster_nrow - 1, cr + row_range); ++row) {
      for (int col = std::max(0, cc - col_range);
           col <= std::min(raster_ncol - 1, cc + col_range); ++col) {
        auto found = rows_by_cell.find(row * raster_ncol + col + 1);
        if (found == rows_by_cell.end()) continue;
        for (int i : found->second) {
          const double scale = std::max(1.0, std::max(
            std::max(std::fabs(x_ref[i]), std::fabs(y_ref[i])),
            std::max(std::fabs(x_candidates[k]), std::fabs(y_candidates[k]))));
          const double slack = distance_tolerance(radius) +
            8 * std::numeric_limits<double>::epsilon() * scale;
          // Minimum distance to the closed centre-cell rectangle. A point may
          // contribute to the bound even if no single disk covers all such points.
          const double dx = std::max(0.0,
            std::fabs(x_ref[i] - x_candidates[k]) - xres / 2 - slack);
          const double dy = std::max(0.0,
            std::fabs(y_ref[i] - y_candidates[k]) - yres / 2 - slack);
          if (dx * dx + dy * dy <= limit2) total += value_ref[i];
        }
      }
    }
    // Keep ties despite differences in floating-point summation order.
    bounds[k] = static_cast<double>(total) +
      kObjectiveComparisonTolerance * std::max(1.0, static_cast<double>(total));
  }
  return bounds;
}

// [[Rcpp::export]]
Rcpp::List candidate_union_rows_cpp(
    Rcpp::IntegerVector candidate_cell_ids,
    Rcpp::NumericVector x_candidates,
    Rcpp::NumericVector y_candidates,
    Rcpp::IntegerVector point_cell_ids,
    Rcpp::NumericVector x_ref,
    Rcpp::NumericVector y_ref,
    double search_radius,
    double xres,
    double yres,
    int raster_nrow,
    int raster_ncol) {

  check_numeric_inputs(x_candidates, y_candidates,
                       "x_candidates", "y_candidates");
  if (candidate_cell_ids.size() != x_candidates.size()) {
    Rcpp::stop("Candidate cell ids and coordinates must have equal lengths.");
  }
  if (point_cell_ids.size() != x_ref.size() || x_ref.size() != y_ref.size()) {
    Rcpp::stop("Point cell ids and coordinates must have equal lengths.");
  }
  if (!std::isfinite(search_radius) || search_radius <= 0.0 ||
      !std::isfinite(xres) || xres <= 0.0 ||
      !std::isfinite(yres) || yres <= 0.0 ||
      raster_nrow <= 0 || raster_ncol <= 0) {
    Rcpp::stop("Raster geometry and search radius must be positive.");
  }

  std::unordered_map<int, std::vector<int> > rows_by_cell;
  rows_by_cell.reserve(static_cast<std::size_t>(x_ref.size() * 1.3));
  for (int row = 0; row < point_cell_ids.size(); ++row) {
    rows_by_cell[point_cell_ids[row]].push_back(row);
  }

  const double radius2 = search_radius * search_radius;
  const double margin = std::max(xres, yres);
  const int col_range =
    static_cast<int>(std::ceil((search_radius + margin) / xres));
  const int row_range =
    static_cast<int>(std::ceil((search_radius + margin) / yres));
  std::vector<unsigned char> in_union(x_ref.size(), 0);
  int max_local_points = 0;

  for (int candidate = 0; candidate < candidate_cell_ids.size(); ++candidate) {
    const int cell0 = candidate_cell_ids[candidate] - 1;
    if (cell0 < 0 || cell0 >= raster_nrow * raster_ncol) {
      Rcpp::stop("Candidate cell id is outside the raster.");
    }
    const int centre_row = cell0 / raster_ncol;
    const int centre_col = cell0 % raster_ncol;
    const int row_min = std::max(0, centre_row - row_range);
    const int row_max = std::min(raster_nrow - 1, centre_row + row_range);
    const int col_min = std::max(0, centre_col - col_range);
    const int col_max = std::min(raster_ncol - 1, centre_col + col_range);
    int local_points = 0;

    for (int row = row_min; row <= row_max; ++row) {
      for (int col = col_min; col <= col_max; ++col) {
        const int cell = row * raster_ncol + col + 1;
        std::unordered_map<int, std::vector<int> >::const_iterator found =
          rows_by_cell.find(cell);
        if (found == rows_by_cell.end()) {
          continue;
        }
        const std::vector<int>& rows = found->second;
        for (std::size_t k = 0; k < rows.size(); ++k) {
          const int point = rows[k];
          const double dx = x_ref[point] - x_candidates[candidate];
          const double dy = y_ref[point] - y_candidates[candidate];
          if (dx * dx + dy * dy <= radius2) {
            ++local_points;
            in_union[point] = 1;
          }
        }
      }
    }
    max_local_points = std::max(max_local_points, local_points);
  }

  std::vector<int> union_rows;
  union_rows.reserve(x_ref.size());
  for (int row = 0; row < x_ref.size(); ++row) {
    if (in_union[row]) {
      union_rows.push_back(row + 1);
    }
  }
  return Rcpp::List::create(
    Rcpp::Named("rows") = union_rows,
    Rcpp::Named("max_local_points") = max_local_points
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
    bool filter_centres,
    bool profile = false,
    bool global_only = false) {

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
  PairProfile profile_data(profile);
  const ProfileClock::time_point index_start = ProfileClock::now();
  GridIndex evaluation_index = build_grid_index(
    x_ref, y_ref, min_x, min_y, cell_width
  );
  if (profile_data.enabled) {
    profile_data.index_seconds = elapsed_seconds(index_start);
  }
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

  if (global_only) {
    std::size_t unique_observed_count = 0;
    std::size_t evaluated_observed_count = 0;
    std::size_t unique_pair_count = 0;
    PairBestResult best = pair_intersection_best_union_sweep(
      candidate_rows, x_ref, y_ref, value_ref, evaluation_index,
      min_x, min_y, radius, cell_width, profile_data, filter_centres,
      selected_cells, raster, unique_observed_count,
      evaluated_observed_count, unique_pair_count,
      evaluated_intersection_count
    );
    Rcpp::DataFrame selected = indexed_points_at_center(
      best.x, best.y, x_ref, y_ref, value_ref, ix_ref, evaluation_index,
      min_x, min_y, radius, cell_width
    );
    return Rcpp::List::create(
      Rcpp::Named("x") = Rcpp::NumericVector::create(best.x),
      Rcpp::Named("y") = Rcpp::NumericVector::create(best.y),
      Rcpp::Named("concentration") =
        Rcpp::NumericVector::create(best.concentration),
      Rcpp::Named("selected") = Rcpp::List::create(selected),
      Rcpp::Named("diagnostics") = Rcpp::List::create(
        Rcpp::Named("raw_observed_centres") =
          static_cast<double>(unique_observed_count),
        Rcpp::Named("unique_observed_centres") =
          static_cast<double>(unique_observed_count),
        Rcpp::Named("raw_point_pairs") =
          static_cast<double>(unique_pair_count),
        Rcpp::Named("unique_point_pairs") =
          static_cast<double>(unique_pair_count),
        Rcpp::Named("evaluated_centres") =
          static_cast<double>(evaluated_observed_count +
                              evaluated_intersection_count),
        Rcpp::Named("evaluated_observed_centres") =
          static_cast<double>(evaluated_observed_count),
        Rcpp::Named("evaluated_intersection_centres") =
          static_cast<double>(evaluated_intersection_count),
        Rcpp::Named("centre_filter_applied") = filter_centres,
        Rcpp::Named("profile_enabled") = profile_data.enabled,
        Rcpp::Named("pairs_considered") =
          static_cast<double>(profile_data.pairs_considered),
        Rcpp::Named("pairs_within_2r") =
          static_cast<double>(profile_data.pairs_within_2r),
        Rcpp::Named("intersections_generated") =
          static_cast<double>(profile_data.intersections_generated),
        Rcpp::Named("intersections_rejected_by_screening") =
          static_cast<double>(profile_data.intersections_rejected),
        Rcpp::Named("time_index_seconds") = profile_data.index_seconds,
        Rcpp::Named("time_observed_scoring_seconds") =
          profile_data.observed_scoring_seconds,
        Rcpp::Named("time_pair_loop_seconds") =
          profile_data.pair_loop_seconds,
        Rcpp::Named("time_exact_scoring_seconds") =
          profile_data.exact_scoring_seconds,
        Rcpp::Named("approx_pair_cache_payload_bytes") = 0.0,
        Rcpp::Named("max_angular_events") =
          static_cast<double>(profile_data.max_angular_events),
        Rcpp::Named("approx_max_event_payload_bytes") =
          static_cast<double>(profile_data.max_angular_events *
                              sizeof(AngularEvent)),
        Rcpp::Named("streaming_angular_sweep") = true
      )
    );
  }

  for (int g = 0; g < n_groups; ++g) {
    Rcpp::IntegerVector rows = candidate_rows[g];
    if (rows.size() == 0) {
      Rcpp::stop("Candidate groups must not be empty.");
    }
    raw_observed_count += static_cast<std::size_t>(rows.size());

    PairBestResult best = pair_intersection_best_rows_with_cache(
      rows, x_ref, y_ref, value_ref, evaluation_index, min_x, min_y,
      radius, cell_width, observed_cache, pair_cache, raw_pair_count,
      evaluated_intersection_count, profile_data, filter_centres,
      selected_cells, raster
    );
    best_x[g] = best.x;
    best_y[g] = best.y;
    concentration[g] = best.concentration;
    const ProfileClock::time_point extraction_start = ProfileClock::now();
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
    if (profile_data.enabled) {
      profile_data.result_extraction_seconds +=
        elapsed_seconds(extraction_start);
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
      , Rcpp::Named("profile_enabled") = profile_data.enabled
      , Rcpp::Named("pairs_considered") =
        static_cast<double>(profile_data.pairs_considered)
      , Rcpp::Named("pairs_within_2r") =
        static_cast<double>(profile_data.pairs_within_2r)
      , Rcpp::Named("intersections_generated") =
        static_cast<double>(profile_data.intersections_generated)
      , Rcpp::Named("intersections_rejected_by_screening") =
        static_cast<double>(profile_data.intersections_rejected)
      , Rcpp::Named("time_index_seconds") = profile_data.index_seconds
      , Rcpp::Named("time_local_setup_seconds") =
        profile_data.local_setup_seconds
      , Rcpp::Named("time_observed_scoring_seconds") =
        profile_data.observed_scoring_seconds
      , Rcpp::Named("time_pair_loop_seconds") =
        profile_data.pair_loop_seconds
      , Rcpp::Named("time_exact_scoring_seconds") =
        profile_data.exact_scoring_seconds
      , Rcpp::Named("time_result_extraction_seconds") =
        profile_data.result_extraction_seconds
      , Rcpp::Named("approx_pair_cache_payload_bytes") =
        static_cast<double>(pair_cache.size() *
          (sizeof(long long) + sizeof(PairCandidateEvaluation)))
    )
  );
}
