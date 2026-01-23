// 简单的增强测试用例

test "simple_add_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "simple_multiply_test" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "simple_greet_test" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "simple_divide_test" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
}

test "boundary_add_test" {
  @azimuth.assert_eq(0, @azimuth.add(1000, -1000))
  @azimuth.assert_eq(-1, @azimuth.add(-100, 99))
}

test "boundary_multiply_test" {
  @azimuth.assert_eq(10000, @azimuth.multiply(100, 100))
  @azimuth.assert_eq(-10000, @azimuth.multiply(-100, 100))
}

test "complex_scenario_test" {
  let total_tasks = 67
  let tasks_per_week = 10
  let weeks_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_week)
  @azimuth.assert_eq(7, weeks_needed)
}

test "unicode_greet_test" {
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "math_composition_test" {
  let a = 12
  let b = 8
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), 4), @azimuth.add(a, @azimuth.add(b, 4)))
}

test "error_handling_test" {
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
}