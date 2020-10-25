#pragma once

#include <array>

class vec3 final
{
public:
   constexpr vec3() = default;
   constexpr vec3(double x, double y, double z) noexcept
   {
      data[0] = x;
      data[1] = y;
      data[2] = z;
   }

   constexpr auto operator+(vec3 const& rhs) const noexcept -> vec3
   {
      return {x() + rhs.x(), y() + rhs.y(), z() + rhs.z()};
   }
   constexpr auto operator-(vec3 const& rhs) const noexcept -> vec3
   {
      return {x() - rhs.x(), y() - rhs.y(), z() - rhs.z()};
   }
   constexpr auto operator*(vec3 const& rhs) const noexcept -> vec3
   {
      return {x() * rhs.x(), y() * rhs.y(), z() * rhs.z()};
   }
   constexpr auto operator*(double scalar) const noexcept -> vec3
   {
      return {x() * scalar, y() * scalar, z() * scalar};
   }
   constexpr auto operator/(double scalar) const noexcept -> vec3
   {
      const auto reciprocal = 1 / scalar;
      return {x() * reciprocal, y() * reciprocal, z() * reciprocal};
   }
   constexpr auto operator-() const noexcept -> vec3 { return {-x(), -y(), -z()}; }

   constexpr auto operator+=(vec3 const& rhs) noexcept -> vec3&
   {
      data[0] += rhs.x();
      data[1] += rhs.y();
      data[2] += rhs.z();

      return *this;
   }
   constexpr auto operator-=(vec3 const& rhs) noexcept -> vec3&
   {
      data[0] -= rhs.x();
      data[1] -= rhs.y();
      data[2] -= rhs.z();

      return *this;
   }
   constexpr auto operator*=(vec3 const& rhs) noexcept -> vec3&
   {
      data[0] *= rhs.x();
      data[1] *= rhs.y();
      data[2] *= rhs.z();

      return *this;
   }
   constexpr auto operator*=(double scalar) noexcept -> vec3&
   {
      data[0] *= scalar;
      data[1] *= scalar;
      data[2] *= scalar;

      return *this;
   }
   constexpr auto operator/=(double scalar) noexcept -> vec3&
   {
      const auto reciprocal = 1 / scalar;
      data[0] *= reciprocal;
      data[1] *= reciprocal;
      data[2] *= reciprocal;

      return *this;
   }

   constexpr auto operator==(vec3 const& rhs) const noexcept -> bool = default;

   constexpr friend auto operator*(double lhs, vec3 const& rhs) -> vec3
   {
      return vec3(lhs * rhs.x(), lhs * rhs.y(), lhs * rhs.z());
   }
   constexpr friend auto operator/(double lhs, vec3 const& rhs) -> vec3
   {
      return vec3(lhs / rhs.x(), lhs / rhs.y(), lhs / rhs.z());
   }

   [[nodiscard]] constexpr auto x() const -> double { return data[0]; }
   [[nodiscard]] constexpr auto y() const -> double { return data[1]; }
   [[nodiscard]] constexpr auto z() const -> double { return data[2]; }

private:
   std::array<double, 3> data = {0.0, 0.0, 0.0};
};
