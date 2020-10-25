
#include <benchmark/benchmark.h>

#include <vector>

struct wrapper
{
   std::uint32_t integer;
};

void normal_loop(benchmark::State& state)
{
   std::vector<wrapper> data{static_cast<std::size_t>(state.range(0))};

   for (auto _ : state)
   {
      for (std::size_t i = 0; i < data.size(); ++i) // NOLINT
      {
         data[i] = {.integer = 10}; // NOLINT
      }

      benchmark::DoNotOptimize(data);
   }
}

void expanded_loop(benchmark::State& state)
{
   std::vector<wrapper> data{static_cast<std::size_t>(state.range(0)) * 16}; // NOLINT

   for (auto _ : state)
   {
      for (std::size_t i = 0; i < data.size(); i += 16) // NOLINT
      {
         data[i] = {.integer = 10}; // NOLINT
      };

      benchmark::DoNotOptimize(data);
   }
}

BENCHMARK(normal_loop)->Arg(10000);
BENCHMARK(expanded_loop)->Arg(10000);
BENCHMARK_MAIN();
