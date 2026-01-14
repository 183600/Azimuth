// 新的 MoonBit 测试用例
// 测试 azimuth 包的核心功能
test "add_commutativity" {
  @azimuth.assert_eq(azimuth::@azimuth.add(5, 3), azimuth::@azimuth.add(3, 5))
}
test "add_associativity" {
  let a = 5
  let b = 3
  let c = 2
  let result1 = azimuth::@azimuth.add(azimuth::@azimuth.add(a, b), c)
  let result2 = azimuth::@azimuth.add(a, azimuth::@azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
}
test "multiply_commutativity" {
  @azimuth.assert_eq(azimuth::@azimuth.multiply(4, 6), azimuth::@azimuth.multiply(6, 4))
}
test "multiply_distributivity" {
  let a = 3
  let b = 4
  let c = 5
  let left = azimuth::@azimuth.multiply(a, azimuth::@azimuth.add(b, c))
  let right = azimuth::@azimuth.add(azimuth::@azimuth.multiply(a, b), azimuth::@azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}
test "greet_unicode" {
  @azimuth.assert_eq_string("Hello, 世界!", azimuth::@azimuth.greet("世界"))
}
test "greet_emoji" {
  @azimuth.assert_eq_string("Hello, 🌍!", azimuth::@azimuth.greet("🌍"))
}
test "business_calculation" {
  let price = 100
  let quantity = 3
  let tax = 10
  let subtotal = azimuth::@azimuth.multiply(price, quantity)
  let total = azimuth::@azimuth.add(subtotal, azimuth::@azimuth.multiply(subtotal, tax) / 100)
  @azimuth.assert_eq(330, total)
}
test "temperature_conversion" {
  let celsius = 20
  let fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
}
test "geometric_area" {
  let length = 10
  let width = 5
  let area = azimuth::@azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
}
test "statistical_average" {
  let sum = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(10, 20), 30), 40)
  let average = sum / 4
  @azimuth.assert_eq(25, average)
}