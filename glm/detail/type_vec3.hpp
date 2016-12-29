/// @ref core
/// @file glm/detail/type_vec3.hpp

#pragma once

#include "type_vec.hpp"
#if GLM_SWIZZLE == GLM_SWIZZLE_ENABLED
#	if GLM_HAS_UNRESTRICTED_UNIONS
#		include "_swizzle.hpp"
#	else
#		include "_swizzle_func.hpp"
#	endif
#endif //GLM_SWIZZLE == GLM_SWIZZLE_ENABLED
#include <cstddef>

namespace glm
{
	template <typename T, precision P>
	struct tvec<3, T, P>
	{
		// -- Implementation detail --

		typedef T value_type;
		typedef tvec type;
		typedef tvec<3, bool, P> bool_type;

		// -- Data --

#		if GLM_HAS_ALIGNED_TYPE
#			if GLM_COMPILER & GLM_COMPILER_GCC
#				pragma GCC diagnostic push
#				pragma GCC diagnostic ignored "-Wpedantic"
#			endif
#			if GLM_COMPILER & GLM_COMPILER_CLANG
#				pragma clang diagnostic push
#				pragma clang diagnostic ignored "-Wgnu-anonymous-struct"
#				pragma clang diagnostic ignored "-Wnested-anon-types"
#			endif

			union
			{
				struct{ T x, y, z; };
				struct{ T r, g, b; };
				struct{ T s, t, p; };

#				if GLM_SWIZZLE == GLM_SWIZZLE_ENABLED
					_GLM_SWIZZLE3_2_MEMBERS(T, P, x, y, z)
					_GLM_SWIZZLE3_2_MEMBERS(T, P, r, g, b)
					_GLM_SWIZZLE3_2_MEMBERS(T, P, s, t, p)
					_GLM_SWIZZLE3_3_MEMBERS(T, P, x, y, z)
					_GLM_SWIZZLE3_3_MEMBERS(T, P, r, g, b)
					_GLM_SWIZZLE3_3_MEMBERS(T, P, s, t, p)
					_GLM_SWIZZLE3_4_MEMBERS(T, P, x, y, z)
					_GLM_SWIZZLE3_4_MEMBERS(T, P, r, g, b)
					_GLM_SWIZZLE3_4_MEMBERS(T, P, s, t, p)
#				endif//GLM_SWIZZLE
			};
		
#			if GLM_COMPILER & GLM_COMPILER_CLANG
#				pragma clang diagnostic pop
#			endif
#			if GLM_COMPILER & GLM_COMPILER_GCC
#				pragma GCC diagnostic pop
#			endif
#		else
			union { T x, r, s; };
			union { T y, g, t; };
			union { T z, b, p; };

#			if GLM_SWIZZLE == GLM_SWIZZLE_ENABLED
				GLM_SWIZZLE_GEN_VEC_FROM_VEC3(T, P, tvec3, tvec2, tvec3, tvec4)
#			endif//GLM_SWIZZLE
#		endif//GLM_LANG

		// -- Component accesses --

		/// Return the count of components of the vector
		typedef length_t length_type;
		GLM_FUNC_DECL static length_type length(){return 3;}

		GLM_FUNC_DECL T & operator[](length_type i);
		GLM_FUNC_DECL T const & operator[](length_type i) const;

		// -- Implicit basic constructors --

		GLM_FUNC_DECL GLM_CONSTEXPR tvec() GLM_DEFAULT_CTOR;
		GLM_FUNC_DECL GLM_CONSTEXPR tvec(tvec const & v) GLM_DEFAULT;
		template <precision Q>
		GLM_FUNC_DECL GLM_CONSTEXPR tvec(tvec<3, T, Q> const & v);

		// -- Explicit basic constructors --

		GLM_FUNC_DECL GLM_CONSTEXPR_CTOR explicit tvec(ctor);
		GLM_FUNC_DECL GLM_CONSTEXPR explicit tvec(T scalar);
		GLM_FUNC_DECL GLM_CONSTEXPR tvec(T a, T b, T c);

		// -- Conversion scalar constructors --

		/// Explicit converions (From section 5.4.1 Conversion and scalar constructors of GLSL 1.30.08 specification)
		template <typename A, typename B, typename C>
		GLM_FUNC_DECL GLM_CONSTEXPR tvec(A a, B b, C c);
		template <typename A, typename B, typename C>
		GLM_FUNC_DECL GLM_CONSTEXPR tvec(tvec<1, A, P> const & a, tvec<1, B, P> const & b, tvec<1, C, P> const & c);

		// -- Conversion vector constructors --

		/// Explicit conversions (From section 5.4.1 Conversion and scalar constructors of GLSL 1.30.08 specification)
		template <typename A, typename B, precision Q>
		GLM_FUNC_DECL GLM_CONSTEXPR tvec(tvec<2, A, Q> const & a, B b);
		/// Explicit conversions (From section 5.4.1 Conversion and scalar constructors of GLSL 1.30.08 specification)
		template <typename A, typename B, precision Q>
		GLM_FUNC_DECL GLM_CONSTEXPR tvec(tvec<2, A, Q> const & a, tvec<1, B, Q> const & b);
		/// Explicit conversions (From section 5.4.1 Conversion and scalar constructors of GLSL 1.30.08 specification)
		template <typename A, typename B, precision Q>
		GLM_FUNC_DECL GLM_CONSTEXPR tvec(A a, tvec<2, B, Q> const & b);
		/// Explicit conversions (From section 5.4.1 Conversion and scalar constructors of GLSL 1.30.08 specification)
		template <typename A, typename B, precision Q>
		GLM_FUNC_DECL GLM_CONSTEXPR tvec(tvec<1, A, Q> const & a, tvec<2, B, Q> const & b);
		/// Explicit conversions (From section 5.4.1 Conversion and scalar constructors of GLSL 1.30.08 specification)
		template <typename U, precision Q>
		GLM_FUNC_DECL GLM_CONSTEXPR GLM_EXPLICIT tvec(tvec<4, U, Q> const & v);

		/// Explicit conversions (From section 5.4.1 Conversion and scalar constructors of GLSL 1.30.08 specification)
		template <typename U, precision Q>
		GLM_FUNC_DECL GLM_CONSTEXPR GLM_EXPLICIT tvec(tvec<3, U, Q> const & v);

		// -- Swizzle constructors --
#		if GLM_HAS_UNRESTRICTED_UNIONS && (GLM_SWIZZLE == GLM_SWIZZLE_ENABLED)
			template <int E0, int E1, int E2>
			GLM_FUNC_DECL tvec(detail::_swizzle<3, T, P, E0, E1, E2, -1> const & that)
			{
				*this = that();
			}

			template <int E0, int E1>
			GLM_FUNC_DECL tvec(detail::_swizzle<2, T, P, E0, E1, -1, -2> const & v, T const & scalar)
			{
				*this = tvec(v(), scalar);
			}

			template <int E0, int E1>
			GLM_FUNC_DECL tvec(T const & scalar, detail::_swizzle<2, T, P, E0, E1, -1, -2> const & v)
			{
				*this = tvec(scalar, v());
			}
#		endif// GLM_HAS_UNRESTRICTED_UNIONS && (GLM_SWIZZLE == GLM_SWIZZLE_ENABLED)

		// -- Unary arithmetic operators --

		GLM_FUNC_DECL tvec & operator=(tvec const & v) GLM_DEFAULT;

		template <typename U>
		GLM_FUNC_DECL tvec & operator=(tvec<3, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator+=(U scalar);
		template <typename U>
		GLM_FUNC_DECL tvec & operator+=(tvec<1, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator+=(tvec<3, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator-=(U scalar);
		template <typename U>
		GLM_FUNC_DECL tvec & operator-=(tvec<1, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator-=(tvec<3, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator*=(U scalar);
		template <typename U>
		GLM_FUNC_DECL tvec & operator*=(tvec<1, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator*=(tvec<3, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator/=(U scalar);
		template <typename U>
		GLM_FUNC_DECL tvec & operator/=(tvec<1, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator/=(tvec<3, U, P> const & v);

		// -- Increment and decrement operators --

		GLM_FUNC_DECL tvec & operator++();
		GLM_FUNC_DECL tvec & operator--();
		GLM_FUNC_DECL tvec operator++(int);
		GLM_FUNC_DECL tvec operator--(int);

		// -- Unary bit operators --

		template <typename U>
		GLM_FUNC_DECL tvec & operator%=(U scalar);
		template <typename U>
		GLM_FUNC_DECL tvec & operator%=(tvec<1, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator%=(tvec<3, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator&=(U scalar);
		template <typename U>
		GLM_FUNC_DECL tvec & operator&=(tvec<1, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator&=(tvec<3, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator|=(U scalar);
		template <typename U>
		GLM_FUNC_DECL tvec & operator|=(tvec<1, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator|=(tvec<3, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator^=(U scalar);
		template <typename U>
		GLM_FUNC_DECL tvec & operator^=(tvec<1, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator^=(tvec<3, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator<<=(U scalar);
		template <typename U>
		GLM_FUNC_DECL tvec & operator<<=(tvec<1, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator<<=(tvec<3, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator>>=(U scalar);
		template <typename U>
		GLM_FUNC_DECL tvec & operator>>=(tvec<1, U, P> const & v);
		template <typename U>
		GLM_FUNC_DECL tvec & operator>>=(tvec<3, U, P> const & v);
	};

	// -- Unary operators --

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator+(tvec<3, T, P> const & v);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator-(tvec<3, T, P> const & v);

	// -- Binary operators --

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator+(tvec<3, T, P> const & v, T scalar);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator+(tvec<3, T, P> const & v, tvec<1, T, P> const & scalar);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator+(T scalar, tvec<3, T, P> const & v);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator+(tvec<1, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator+(tvec<3, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator-(tvec<3, T, P> const & v, T scalar);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator-(tvec<3, T, P> const & v1, tvec<1, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator-(T scalar, tvec<3, T, P> const & v);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator-(tvec<1, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator-(tvec<3, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator*(tvec<3, T, P> const & v, T scalar);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator*(tvec<3, T, P> const & v1, tvec<1, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator*(T scalar, tvec<3, T, P> const & v);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator*(tvec<1, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator*(tvec<3, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator/(tvec<3, T, P> const & v, T scalar);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator/(tvec<3, T, P> const & v1, tvec<1, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator/(T scalar, tvec<3, T, P> const & v);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator/(tvec<1, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator/(tvec<3, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator%(tvec<3, T, P> const & v, T scalar);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator%(tvec<3, T, P> const & v1, tvec<1, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator%(T const & scalar, tvec<3, T, P> const & v);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator%(tvec<1, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator%(tvec<3, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator&(tvec<3, T, P> const & v1, T scalar);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator&(tvec<3, T, P> const & v1, tvec<1, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator&(T scalar, tvec<3, T, P> const & v);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator&(tvec<1, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator&(tvec<3, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator|(tvec<3, T, P> const & v, T scalar);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator|(tvec<3, T, P> const & v1, tvec<1, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator|(T scalar, tvec<3, T, P> const & v);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator|(tvec<1, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator|(tvec<3, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator^(tvec<3, T, P> const & v, T scalar);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator^(tvec<3, T, P> const & v1, tvec<1, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator^(T scalar, tvec<3, T, P> const & v);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator^(tvec<1, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator^(tvec<3, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator<<(tvec<3, T, P> const & v, T scalar);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator<<(tvec<3, T, P> const & v1, tvec<1, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator<<(T scalar, tvec<3, T, P> const & v);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator<<(tvec<1, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator<<(tvec<3, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator>>(tvec<3, T, P> const & v, T scalar);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator>>(tvec<3, T, P> const & v1, tvec<1, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator>>(T scalar, tvec<3, T, P> const & v);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator>>(tvec<1, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL tvec<3, T, P> operator>>(tvec<3, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P> 
	GLM_FUNC_DECL tvec<3, T, P> operator~(tvec<3, T, P> const & v);

	// -- Boolean operators --

	template <typename T, precision P>
	GLM_FUNC_DECL bool operator==(tvec<3, T, P> const & v1, tvec<3, T, P> const & v2);

	template <typename T, precision P>
	GLM_FUNC_DECL bool operator!=(tvec<3, T, P> const & v1, tvec<3, T, P> const & v2);

	template <precision P>
	GLM_FUNC_DECL tvec<3, bool, P> operator&&(tvec<3, bool, P> const & v1, tvec<3, bool, P> const & v2);

	template <precision P>
	GLM_FUNC_DECL tvec<3, bool, P> operator||(tvec<3, bool, P> const & v1, tvec<3, bool, P> const & v2);
}//namespace glm

#ifndef GLM_EXTERNAL_TEMPLATE
#include "type_vec3.inl"
#endif//GLM_EXTERNAL_TEMPLATE
