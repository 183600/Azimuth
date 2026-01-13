// 新的 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_commutativity" {
  assert_eq(azimuth::add(5, 3), azimuth::add(3, 5))
}

test "add_associativity" {
  let a = 5
  let b = 3
  let c = 2
  let result1 = azimuth::add(azimuth::add(a, b), c)
  let result2 = azimuth::add(a, azimuth::add(b, c))
  assert_eq(result1, result2)
}

test "multiply_commutativity" {
  assert_eq(azimuth::multiply(4, 6), azimuth::multiply(6, 4))
}

test "multiply_distributivity" {
  let a = 3
  let b = 4
  let c = 5
  let left = azimuth::multiply(a, azimuth::add(b, c))
  let right = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  assert_eq(left, right)
}

test "greet_unicode" {
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
}

test "greet_emoji" {
  assert_eq_string("Hello, 🌍!", azimuth::greet("🌍"))
}

test "business_calculation" {
  let price = 100
  let quantity = 3
  let tax = 10
  let subtotal = azimuth::multiply(price, quantity)
  let total = azimuth::add(subtotal, azimuth::multiply(subtotal, tax) / 100)
  assert_eq(330, total)
}

test "temperature_conversion" {
  let celsius = 20
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(70, fahrenheit)
}

test "geometric_area" {
  let length = 10
  let width = 5
  let area = azimuth::multiply(length, width)
  assert_eq(50, area)
}

test "statistical_average" {
  let sum = azimuth::add(azimuth::add(azimuth::add(10, 20), 30), 40)
  let average = sum / 4
  assert_eq(25, average)
}