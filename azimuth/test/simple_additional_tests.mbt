// 简单的 Azimuth 测试用例
// 包含8个基本测试用例

test "simple_add_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "simple_multiply_test" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "simple_divide_test" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "simple_greet_test" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "simple_subtract_test" {
  @azimuth.assert_eq(2, @azimuth.subtract(5, 3))
  @azimuth.assert_eq(-2, @azimuth.subtract(3, 5))
}

test "simple_commutative_test" {
  let a = 7
  let b = 13
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "simple_packaging_test" {
  let items = 25
  let capacity = 6
  let packages = @azimuth.divide_with_ceil(items, capacity)
  @azimuth.assert_eq(5, packages)
  
  let total_capacity = @azimuth.multiply(packages, capacity)
  let remaining = @azimuth.subtract(total_capacity, items)
  @azimuth.assert_eq(5, remaining)
}

test "simple_complex_test" {
  let team = 4
  let tasks_per_person = 8
  let total_tasks = @azimuth.multiply(team, tasks_per_person)
  
  let sprint_capacity = 15
  let sprints = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(32, total_tasks)
  @azimuth.assert_eq(3, sprints)
}