#include <algorithm>
#include <ranges>
namespace rs = std::ranges;

#include "arena.hpp"

struct Object
{
	int id = 0;

	Object()
		: id{666}
	{}

	Object(int _id)
		: id{_id}
	{}

	static auto null_object() -> Object& {
		static auto null = Object(-1);
		return null;
	}

	~Object() {
		id = -1;
	}

	friend
	auto operator<< (std::ostream& o, const Object& obj) -> std::ostream& {
		return o << "Object{id=" << obj.id << '}';
	}
};

#ifndef MAX_OBJS_PREPROC
#define MAX_OBJS_PREPROC (10'000)
#endif
constexpr auto MAX_OBJS = MAX_OBJS_PREPROC;

using Arena_t = Arena<Object, MAX_OBJS>;
using Objs = std::array<Arena_t::Iterator, MAX_OBJS>;

auto main() -> int {
	auto arena = Arena_t{};
	auto objs = Objs{};

	std::cerr << "Running for " << MAX_OBJS << '\n';

	for (auto& obj : objs)
		obj = arena.allocate().value_or(&Object::null_object());

	assert(rs::none_of(objs, [null = &Object::null_object()](const auto& o) { return o == null; }));	\
	assert(arena.full());

	std::cerr << *objs.front() << '\n';
	objs.front()->id = 123;
	std::cerr << *objs.front() << "\n\n";

	for (const auto& obj : objs)
		arena.deallocate(obj);
}
