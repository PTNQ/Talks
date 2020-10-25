#include "vec3.hpp"

#include <benchmark/benchmark.h>

#include <vector>

static constexpr std::int64_t iteration_count{1000000};

struct vertical_particle
{
   vec3 position{0.0, 0.0, 0.0};
   vec3 velocity{0.0, 0.0, 0.0};
};

struct horizontal_particle
{
   std::vector<vec3> positions{};
   std::vector<vec3> velocities{};
};

void vertical_position_iteration(benchmark::State& state)
{
   std::vector<vertical_particle> data{static_cast<std::size_t>(state.range(0))};

   for (auto _ : state)
   {
      for (auto& particle : data)
      {
         particle.position = vec3{1.0, 1.0, 1.0};
      }

      benchmark::DoNotOptimize(data);
   }
}

void horizontal_position_iteration(benchmark::State& state)
{
   horizontal_particle data{};
   data.positions = std::vector<vec3>{static_cast<std::size_t>(state.range(0))};
   data.velocities = std::vector<vec3>{static_cast<std::size_t>(state.range(0))};

   for (auto _ : state)
   {
      for (auto& position : data.positions)
      {
         position = vec3{1.0, 1.0, 1.0};
      }

      benchmark::DoNotOptimize(data);
   }
}

BENCHMARK(vertical_position_iteration)->Arg(iteration_count);
BENCHMARK(horizontal_position_iteration)->Arg(iteration_count);

void vertical_unified_iteration(benchmark::State& state)
{
   std::vector<vertical_particle> data{static_cast<std::size_t>(state.range(0))};

   for (auto _ : state)
   {
      for (auto& particle : data)
      {
         particle.position = vec3{1.0, 1.0, 1.0};
         particle.velocity = vec3{1.0, 1.0, 1.0};
      }

      benchmark::DoNotOptimize(data);
   }
}

void horizontal_unified_iteration(benchmark::State& state)
{
   horizontal_particle data{};
   data.positions = std::vector<vec3>{static_cast<std::size_t>(state.range(0))};
   data.velocities = std::vector<vec3>{static_cast<std::size_t>(state.range(0))};

   for (auto _ : state)
   {
      for (std::int64_t i = 0u; i < state.range(0); ++i)
      {
         data.positions[i] = vec3{1.0, 1.0, 1.0};
         data.velocities[i] = vec3{1.0, 1.0, 1.0};
      }

      benchmark::DoNotOptimize(data);
   }
}

void horizontal_unified_double_iteration(benchmark::State& state)
{
   horizontal_particle data{};
   data.positions = std::vector<vec3>{static_cast<std::size_t>(state.range(0))};
   data.velocities = std::vector<vec3>{static_cast<std::size_t>(state.range(0))};

   for (auto _ : state)
   {
      for (auto& position : data.positions)
      {
         position = vec3{1.0, 1.0, 1.0};
      }
      for (auto& velocity : data.velocities)
      {
         velocity = vec3{1.0, 1.0, 1.0};
      }

      benchmark::DoNotOptimize(data);
   }
}

BENCHMARK(vertical_unified_iteration)->Arg(iteration_count);
BENCHMARK(horizontal_unified_iteration)->Arg(iteration_count);
BENCHMARK(horizontal_unified_double_iteration)->Arg(iteration_count);

BENCHMARK_MAIN();
