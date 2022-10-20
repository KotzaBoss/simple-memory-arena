#pragma once

#include <array>
#include <iostream>
#include <cassert>
#include <bitset>
#include <optional>
#include <algorithm>
#include <ranges>
namespace rs = std::ranges;

//
// std::bitset Utils
//

template<size_t N>
auto end(const std::bitset<N>&) -> ssize_t {
	return -1l;
}

template<size_t N>
auto find(const std::bitset<N>& bs, const bool x) -> ssize_t {
	for (auto i = 0ul; i < bs.size(); ++i)
		if (bs[i] == x)
			return i;
	return end(bs);
}

template<size_t N>
auto operator<< (std::ostream& o, const std::bitset<N>& bs) -> std::ostream& {
	return o << '|' << bs.to_string() << '|';
}

//
// Arena
//

template<typename T, size_t ContainerSize>
struct Arena {
	static_assert(ContainerSize > 0);

	using ObjArray = std::array<T, ContainerSize>;
	using Bitset = std::bitset<ContainerSize>;

	using Object = ObjArray::value_type;
	using Iterator = ObjArray::iterator;
	using ConstIterator = ObjArray::const_iterator;

private:
	ObjArray objs;
	Bitset allocation_flags;

public:
	[[nodiscard]]
	auto allocate(auto&&... args) -> std::optional<Iterator> {

		if (const auto pos = find(allocation_flags, false);
			pos != end(allocation_flags))
		{
			assert(0 <= pos and static_cast<size_t>(pos) < allocation_flags.size());

			const auto i = objs.begin() + pos;
			new (&(*i)) Object(args...);	// TODO: Error handling
			allocation_flags[pos].flip();
			return i;
		}
		else
			return std::nullopt;
	}

	auto deallocate(Iterator i) -> void {
		assert(objs.cbegin() <= i and i < objs.cend());

		const auto pos = std::distance(objs.begin(), i);
		assert(allocation_flags[pos]);

		allocation_flags[pos].flip();
		i->~Object();
	}

	auto allocated() const -> size_t {
		return allocation_flags.count();
	}

	auto size() const -> size_t {
		return ContainerSize;
	}

	auto full() const -> bool {
		return allocated() == size();
	}

	auto empty() const -> bool {
		return not allocated();
	}

	auto null() -> Iterator {
		return objs.end();
	}

	template<typename _T, size_t _N>
	friend auto operator<< (std::ostream&, const Arena<_T, _N>&) -> std::ostream&;
};

template<typename T, size_t N>
auto operator<< (std::ostream& o, const Arena<T, N>& a) -> std::ostream& {
	return o << "Arena[" << a.allocation_flags << ']';
}

