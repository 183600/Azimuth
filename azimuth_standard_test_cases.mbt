// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含 8 个高质量测试用例，覆盖核心功能

test "basic_addition_functionality" {
  // 基本加法功能测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "divide_with_ceil_basic" {
  // 向上取整除法基本测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "greet_function_standard" {
  // 问候函数标准测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "subtract_functionality" {
  // 减法功能测试
  @azimuth.assert_eq(3, @azimuth.subtract(8, 5))
  @azimuth.assert_eq(-3, @azimuth.subtract(5, 8))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
  @azimuth.assert_eq(15, @azimuth.subtract(5, -10))
}

test "mathematical_commutative_property" {
  // 数学交换律性质测试
  let a = 7
  let b = 13
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let items = 25
  let capacity = 6
  let packages = @azimuth.divide_with_ceil(items, capacity)
  @azimuth.assert_eq(5, packages)
  
  let total_capacity = @azimuth.multiply(packages, capacity)
  let remaining = @azimuth.subtract(total_capacity, items)
  @azimuth.assert_eq(5, remaining)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  let team = 4
  let tasks_per_person = 8
  let total_tasks = @azimuth.multiply(team, tasks_per_person)
  
  let sprint_capacity = 15
  let sprints = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(32, total_tasks)  // 4 * 8 = 32
  @azimuth.assert_eq(3, sprints)       // ceil(32/15) = 3
}