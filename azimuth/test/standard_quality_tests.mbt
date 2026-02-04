// Azimuth 项目标准质量测试用例
// 包含 10 个高质量的 MoonBit 测试用例，使用标准测试语法

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "subtract_basic_operations" {
  // 基本减法运算测试
  @azimuth.assert_eq(3, @azimuth.subtract(10, 7))
  @azimuth.assert_eq(-3, @azimuth.subtract(7, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(15, @azimuth.subtract(10, -5))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质验证
  let a = 8
  let b = 12
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  @azimuth.assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(5, remaining_space)
}

test "mathematical_identity_properties" {
  // 数学恒等式验证
  let x = 42
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(48, total_tasks)  // 6 * 8 = 48
  @azimuth.assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}