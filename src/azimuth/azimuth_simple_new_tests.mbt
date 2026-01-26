// Azimuth 简单新测试用例 - 10个测试
// 测试 azimuth 库的核心功能

test "simple_addition" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "simple_multiplication" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_greet" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "simple_divide_with_ceil" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "factorial_simulation" {
  // 模拟 4! = 24
  let factorial_4 = multiply(4, multiply(3, multiply(2, 1)))
  assert_eq(24, factorial_4)
}

test "temperature_conversion" {
  // 摄氏度到华氏度：F = C × 9/5 + 32
  let celsius = 25
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  assert_eq(77, fahrenheit)
}

test "batch_processing" {
  // 计算需要的批次数
  let total_items = 180
  let batch_size = 25
  let batches_needed = divide_with_ceil(total_items, batch_size)
  assert_eq(8, batches_needed)
}

test "array_index" {
  // 二维数组转一维数组索引
  let row = 3
  let col = 4
  let cols_per_row = 6
  let one_d_index = add(multiply(row, cols_per_row), col)
  assert_eq(22, one_d_index)
}

test "circular_buffer" {
  // 循环缓冲区位置计算
  let buffer_size = 10
  let current_position = 8
  let steps_forward = 5
  let new_position = add(current_position, steps_forward) % buffer_size
  assert_eq(3, new_position)
}

test "binary_search_mid" {
  // 二分查找中间值计算
  let low = 0
  let high = 15
  let mid = divide_with_ceil(add(low, high), 2)
  assert_eq(8, mid)
}