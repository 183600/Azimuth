// Azimuth 项目额外测试用例
// 使用标准 MoonBit 测试语法

test "subtract_basic" {
  @azimuth.assert_eq(3, @azimuth.subtract(5, 2))
  @azimuth.assert_eq(-3, @azimuth.subtract(2, 5))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
}

test "divide_basic" {
  @azimuth.assert_eq(4, @azimuth.divide(8, 2))
  @azimuth.assert_eq(0, @azimuth.divide(5, 10))
  @azimuth.assert_eq(-3, @azimuth.divide(-9, 3))
}

test "power_function" {
  @azimuth.assert_eq(8, @azimuth.power(2, 3))
  @azimuth.assert_eq(1, @azimuth.power(5, 0))
  @azimuth.assert_eq(16, @azimuth.power(4, 2))
}

test "factorial_function" {
  @azimuth.assert_eq(1, @azimuth.factorial(0))
  @azimuth.assert_eq(1, @azimuth.factorial(1))
  @azimuth.assert_eq(6, @azimuth.factorial(3))
  @azimuth.assert_eq(24, @azimuth.factorial(4))
}

test "string_concatenation" {
  @azimuth.assert_eq_string("HelloWorld", @azimuth.concat("Hello", "World"))
  @azimuth.assert_eq_string("Test", @azimuth.concat("", "Test"))
  @azimuth.assert_eq_string("Test", @azimuth.concat("Test", ""))
}

test "string_length" {
  @azimuth.assert_eq(5, @azimuth.string_length("Hello"))
  @azimuth.assert_eq(0, @azimuth.string_length(""))
  @azimuth.assert_eq(10, @azimuth.string_length("1234567890"))
}

test "array_operations" {
  let arr1 = [1, 2, 3, 4, 5]
  @azimuth.assert_eq(15, @azimuth.array_sum(arr1))
  @azimuth.assert_eq(3, @azimuth.array_length(arr1))
  
  let arr2 = [10, 20, 30]
  @azimuth.assert_eq(60, @azimuth.array_sum(arr2))
  @azimuth.assert_eq(30, @azimuth.array_max(arr2))
}

test "boolean_operations" {
  @azimuth.assert_eq(true, @azimuth.and(true, true))
  @azimuth.assert_eq(false, @azimuth.and(true, false))
  @azimuth.assert_eq(true, @azimuth.or(true, false))
  @azimuth.assert_eq(false, @azimuth.or(false, false))
  @azimuth.assert_eq(false, @azimuth.not(true))
  @azimuth.assert_eq(true, @azimuth.not(false))
}

test "conditional_logic" {
  @azimuth.assert_eq("positive", @azimuth.check_number(10))
  @azimuth.assert_eq("zero", @azimuth.check_number(0))
  @azimuth.assert_eq("negative", @azimuth.check_number(-5))
}