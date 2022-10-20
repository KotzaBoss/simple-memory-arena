#include <benchmark/benchmark.h>

#include "arena.hpp"

struct Object
{
	int id = 0;

	Object()
		: id{666}
	{}

	~Object() {
		id = -1;
	}
};

#ifndef MAX_OBJS_PREPROC
#define MAX_OBJS_PREPROC (10'000)
#endif
constexpr auto MAX_OBJS = MAX_OBJS_PREPROC;

using Arena_t = Arena<Object, MAX_OBJS>;
using Objs = std::array<Arena_t::Iterator, MAX_OBJS>;

auto allocate_all(Arena_t& arena, Objs& objs) -> void {
	for (auto& obj : objs)
		obj = arena.allocate().value_or(arena.null());
}

#define ASSERT_ALLOCATE_ALL_OK(arena, objs) \
		assert(rs::none_of(objs, [](const auto& o) { return o == nullptr; }));	\
		assert(arena.full());

static void BM_Allocation(benchmark::State& state) {
	auto arena = Arena_t{};
	auto objs = Objs{};

	for (auto _ : state) {
		allocate_all(arena, objs);
		ASSERT_ALLOCATE_ALL_OK(arena, objs);
	}
}

static void BM_Deallocation(benchmark::State& state) {
	auto arena = Arena_t{};
	auto objs = Objs{};

	for (auto _ : state) {
		state.PauseTiming();
		allocate_all(arena, objs);
		ASSERT_ALLOCATE_ALL_OK(arena, objs);
		state.ResumeTiming();

		for (const auto& obj : objs)
			arena.deallocate(obj);
	}

	assert(arena.empty());
}

BENCHMARK(BM_Allocation);
BENCHMARK(BM_Deallocation);

BENCHMARK_MAIN();

