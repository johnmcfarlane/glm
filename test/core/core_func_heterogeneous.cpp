#include <glm/gtc/vec1.hpp>

#if GLM_HAS_DECLTYPE && __cplusplus>=201103L

namespace {
	////////////////////////////////////////////////////////////////////////////////
	// tvec_t - generic alias to any vector type

	template<typename T, int N, glm::precision P>
	struct tvec;

	template<typename T, glm::precision P>
	struct tvec<T, 1, P> {
		using type = ::glm::tvec1<T, P>;
	};

	template<typename T, glm::precision P>
	struct tvec<T, 2, P> {
		using type = ::glm::tvec2<T, P>;
	};

	template<typename T, glm::precision P>
	struct tvec<T, 3, P> {
		using type = ::glm::tvec3<T, P>;
	};

	template<typename T, glm::precision P>
	struct tvec<T, 4, P> {
		using type = ::glm::tvec4<T, P>;
	};

	template<typename T, int N, glm::precision P>
	using tvec_t = typename tvec<T, N, P>::type;

	////////////////////////////////////////////////////////////////////////////////
	// make_random - bitwise random object

	using byte_type = std::uint8_t;

	std::uint8_t random_byte()
	{
		constexpr auto max_byte = std::numeric_limits<byte_type>::max();
		constexpr auto upper_limit = static_cast<std::uintmax_t>(max_byte)+1;
		auto r = static_cast<std::uintmax_t>(rand());
		auto random_byte = r*upper_limit/RAND_MAX;
		return random_byte;
	}

	template<typename T, class Enable = void>
	struct make_random_s;

	template<typename T>
	struct make_random_s<T, typename std::enable_if<std::is_pod<T>::value>::type> {
		T operator()()
		{
			union {
				T value;
				byte_type buffer[sizeof(value)];
			} u;

			for (auto& byte : u.buffer) {
				byte = random_byte();
			}

			return u.value;
		}
	};

	template<typename T>
	struct make_random_s<T, typename std::enable_if<!std::is_pod<T>::value>::type> {
		T operator()()
		{
			auto v = T{glm::uninitialize};

			for (auto i = 0; i != v.length(); ++ i) {
				auto& c = v[i];
				v[i] = make_random_s<typename std::decay<decltype(c)>::type>()();
			}

			return v;
		}
	};

	template<typename T>
	T make_random() {
		return make_random_s<T>()();
	}

	////////////////////////////////////////////////////////////////////////////////
	// op - enumeration of binary operations

	enum class op {
		add,
		subtract,
		multiply,
		divide,
		modulo,
		bitwise_and,
		bitwise_or,
		bitwise_xor,
		shift_left,
		shift_right
	};

	////////////////////////////////////////////////////////////////////////////////
	// operation<> - performs the operation specified by OP

	template<op OP, typename LHS, typename RHS>
	struct operation;

	template<typename LHS, typename RHS>
	struct operation<op::add, LHS, RHS> {
		constexpr auto operator()(LHS const& lhs, RHS const& rhs) const
		-> decltype(lhs+rhs)
		{
			return lhs+rhs;
		}
	};

	template<typename LHS, typename RHS>
	struct operation<op::subtract, LHS, RHS> {
		constexpr auto operator()(LHS const& lhs, RHS const& rhs) const
		-> decltype(lhs-rhs)
		{
			return lhs-rhs;
		}
	};

	template<typename LHS, typename RHS>
	struct operation<op::multiply, LHS, RHS> {
		constexpr auto operator()(LHS const& lhs, RHS const& rhs) const
		-> decltype(lhs*rhs)
		{
			return lhs*rhs;
		}
	};

	template<typename LHS, typename RHS>
	struct operation<op::divide, LHS, RHS> {
		constexpr auto operator()(LHS const& lhs, RHS const& rhs) const
		-> decltype(lhs/rhs)
		{
			return lhs/rhs;
		}
	};

	template<typename LHS, typename RHS>
	struct operation<op::modulo, LHS, RHS> {
		constexpr auto operator()(LHS const& lhs, RHS const& rhs) const
		-> decltype(lhs%rhs)
		{
			return lhs%rhs;
		}
	};

	template<typename LHS, typename RHS>
	struct operation<op::bitwise_and, LHS, RHS> {
		constexpr auto operator()(LHS const& lhs, RHS const& rhs) const
		-> decltype(lhs&rhs)
		{
			return lhs&rhs;
		}
	};

	template<typename LHS, typename RHS>
	struct operation<op::bitwise_or, LHS, RHS> {
		constexpr auto operator()(LHS const& lhs, RHS const& rhs) const
		-> decltype(lhs|rhs)
		{
			return lhs|rhs;
		}
	};

	template<typename LHS, typename RHS>
	struct operation<op::bitwise_xor, LHS, RHS> {
		constexpr auto operator()(LHS const& lhs, RHS const& rhs) const
		-> decltype(lhs^rhs)
		{
			return lhs^rhs;
		}
	};

	template<typename LHS, typename RHS>
	struct operation<op::shift_left, LHS, RHS> {
		constexpr auto operator()(LHS const& lhs, RHS const& rhs) const
		-> decltype(lhs<<rhs)
		{
			return lhs<<rhs;
		}
	};

	template<typename LHS, typename RHS>
	struct operation<op::shift_right, LHS, RHS> {
		constexpr auto operator()(LHS const& lhs, RHS const& rhs) const
		-> decltype(lhs>>rhs)
		{
			return lhs>>rhs;
		}
	};

	////////////////////////////////////////////////////////////////////////////////
	// get_component<> - returns component of a vector or the value of a scalar

	template<typename T, bool IsScalar>
	struct get_component_s;

	template<typename T, glm::precision P>
	struct get_component_s<glm::tvec1<T, P>, false> {
		constexpr auto operator()(glm::tvec1<T, P> const& object, int) const
		-> decltype(object.x)
		{
			return object.x;
		}
	};

	template<typename T>
	struct get_component_s<T, false> {
		constexpr auto operator()(T const& object, int index) const
		-> decltype(object[index])
		{
			return object[index];
		}
	};

	template<typename T>
	struct get_component_s<T, true> {
		constexpr T const& operator()(T const& object, int /*index*/) const
		{
			return object;
		}
	};

	template<typename O>
	constexpr auto get_component(O const& object, int index)
	-> decltype(get_component_s<O, std::numeric_limits<O>::is_specialized>()(object, index))
	{
		return get_component_s<O, std::numeric_limits<O>::is_specialized>()(object, index);
	}

	////////////////////////////////////////////////////////////////////////////////
	// test_ functions

	template<int N, op OP, typename LHS, typename RHS, glm::precision P>
	int test_operands()
	{
		int Error(0);

		auto lhs = make_random<LHS>();
		auto rhs = make_random<RHS>();
		auto result = operation<OP, LHS, RHS>()(lhs, rhs);

		for (auto index = 0; index!=N; ++index) {
			auto lhs_component = get_component(lhs, index);
			auto rhs_component = get_component(rhs, index);
			auto expected_component = operation<OP, decltype(lhs_component), decltype(rhs_component)>()(
					lhs_component,
					rhs_component);
			auto actual_component = get_component(result, index);

			static_assert(std::is_same<decltype(expected_component), decltype(actual_component)>::value,
					"the types of the components of the result of a vector operation "
							"are not the same as "
							"the types of the results of the same operation on the components");

			if (expected_component==actual_component) {
				// OK if result is as expected
				continue;
			}

			if ((expected_component!=expected_component) && (actual_component!=actual_component)) {
				// OK if expected and actual are both invalid
				continue;
			}

			++Error;
		}

		return Error;
	}

	template<int N, op OP, typename LHS_S, typename RHS_S, glm::precision P>
	int test_precision()
	{
		int Error(0);

		using tvecN_lhs = tvec_t<LHS_S, N, P>;
		using tvec1_lhs = glm::tvec1<LHS_S, P>;
		using scalar_lhs = LHS_S;

		using tvecN_rhs = tvec_t<RHS_S, N, P>;
		using tvec1_rhs = glm::tvec1<RHS_S, P>;
		using scalar_rhs = RHS_S;

		Error += test_operands<N, OP, scalar_lhs, scalar_rhs, P>(); // this one should definitely not go wrong
		Error += test_operands<N, OP, scalar_lhs, tvecN_rhs, P>();
		Error += test_operands<N, OP, tvec1_lhs, tvecN_rhs, P>();
		Error += test_operands<N, OP, tvecN_lhs, scalar_rhs, P>();
		Error += test_operands<N, OP, tvecN_lhs, tvec1_rhs, P>();
		Error += test_operands<N, OP, tvecN_lhs, tvecN_rhs, P>();

		return Error;
	}

	template<int N, op OP, typename LHS_S, typename RHS_S>
	int test_scalar_types()
	{
		int Error(0);

		Error += test_precision<N, OP, LHS_S, RHS_S, glm::highp>();
		Error += test_precision<N, OP, LHS_S, RHS_S, glm::mediump>();
		Error += test_precision<N, OP, LHS_S, RHS_S, glm::lowp>();

		return Error;
	}

	template<int N, op OP>
	int test_operation_integer()
	{
		int Error(0);

		Error += test_scalar_types<N, OP, uint16_t, int64_t>();
		Error += test_scalar_types<N, OP, int8_t, int>();
		Error += test_scalar_types<N, OP, uint8_t, int>();
		Error += test_scalar_types<N, OP, int16_t, long>();
		Error += test_scalar_types<N, OP, uint32_t, int64_t>();
		Error += test_scalar_types<N, OP, short, int64_t>();

		return Error;
	}

	template<int N, op OP>
	int test_operation()
	{
		int Error(0);

		// integer can do anything floating-point can do
		Error += test_operation_integer<N, OP>();

		// plus bit-wise operations
		Error += test_scalar_types<N, OP, float, float>();
		Error += test_scalar_types<N, OP, double, float>();
		Error += test_scalar_types<N, OP, double, int>();
		Error += test_scalar_types<N, OP, uint8_t, int>();
		Error += test_scalar_types<N, OP, int16_t, long double>();
		Error += test_scalar_types<N, OP, uint32_t, int64_t>();
		Error += test_scalar_types<N, OP, float, int64_t>();

		return Error;
	}

	template<int N>
	int test_dimension()
	{
		int Error(0);

		Error += test_operation<N, op::add>();
		Error += test_operation<N, op::subtract>();
		Error += test_operation<N, op::multiply>();
		Error += test_operation<N, op::divide>();
		Error += test_operation_integer<N, op::modulo>();
		Error += test_operation_integer<N, op::bitwise_and>();
		Error += test_operation_integer<N, op::bitwise_or>();
		Error += test_operation_integer<N, op::bitwise_xor>();
		Error += test_operation_integer<N, op::shift_left>();
		Error += test_operation_integer<N, op::shift_right>();

		return Error;
	}
}

int main()
{
	int Error(0);

//    Error += test_dimension<1>();
//    Error += test_dimension<2>();
//    Error += test_dimension<3>();
	Error += test_dimension<4>();

	return Error;
}

#else	// GLM_HAS_DECLTYPE && __cplusplus>=201103L

int main()
{
	return 0;
}

#endif	// GLM_HAS_DECLTYPE && __cplusplus>=201103L
