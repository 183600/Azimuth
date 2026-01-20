// 标准 MoonBit 测试套件
// 使用标准的 MoonBit 测试语法

test "add_function_basic_addition" {
  inspect(add(1, 2), "3")?
  inspect(add(0, 5), "5")?
  inspect(add(-3, 7), "4")?
}

test "add_function_overflow_handling" {
  inspect(add(2147483647, 1), "2147483647")?  // 正溢出
  inspect(add(-2147483648, -1), "-2147483648")?  // 负溢出
  inspect(add(-2147483648, 1), "-2147483647")?  // 最小值加正数
}

test "multiply_function_basic_cases" {
  inspect(multiply(3, 4), "12")?
  inspect(multiply(-2, 5), "-10")?
  inspect(multiply(0, 100), "0")?
}

test "multiply_function_overflow_handling" {
  inspect(multiply(2147483647, 2), "2147483647")?  // 正溢出
  inspect(multiply(-2147483648, 2), "-2147483648")?  // 负溢出
  inspect(multiply(-2147483648, -1), "-2147483648")?  // 最小值特殊情况
}

test "greet_function" {
  inspect(greet("World"), "Hello, World!")?
  inspect(greet(""), "Hello, !")?
  inspect(greet("MoonBit"), "Hello, MoonBit!")?
}

test "divide_with_ceil_function" {
  inspect(divide_with_ceil(10, 3), "4")?      // 10/3 = 3.33... 向上取整为 4
  inspect(divide_with_ceil(9, 3), "3")?       // 9/3 = 3，无需向上取整
  inspect(divide_with_ceil(7, 2), "4")?       // 7/2 = 3.5 向上取整为 4
  inspect(divide_with_ceil(-5, 2), "-2")?     // -5/2 = -2.5 向上取整为 -2
}

test "assert_functions_basic" {
  assert_eq(5, 5)
  assert_eq_string("hello", "hello")
  assert_true(true)
  assert_false(false)
}

test "edge_cases_zero_and_one" {
  inspect(add(0, 0), "0")?
  inspect(multiply(1, 42), "42")?
  inspect(multiply(-1, 42), "-42")?
  inspect(multiply(42, 0), "0")?
}

test "negative_number_operations" {
  inspect(add(-5, -3), "-8")?
  inspect(multiply(-4, -3), "12")?
  inspect(multiply(-4, 3), "-12")?
  inspect(divide_with_ceil(-10, 3), "-3")?
}

test "boundary_values" {
  inspect(add(2147483647, 0), "2147483647")?
  inspect(add(-2147483648, 0), "-2147483648")?
  inspect(multiply(1, 2147483647), "2147483647")?
  inspect(multiply(1, -2147483648), "-2147483648")?
}