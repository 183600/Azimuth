// MoonBit 标准测试用例
// 为 Azimuth 项目添加的高质量测试覆盖

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(7, @azimuth.add(3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(2, @azimuth.subtract(10, 8))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  @azimuth.assert_eq(@azimuth.add(15, 25), @azimuth.add(25, 15))
  @azimuth.assert_eq(@azimuth.multiply(8, 12), @azimuth.multiply(12, 8))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(5, @azimuth.subtract(2, -3))
  @azimuth.assert_eq(-5, @azimuth.subtract(-2, 3))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  @azimuth.assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(1, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源计算
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(48, total_tasks)  // 8 * 6 = 48
  @azimuth.assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-5, -5))
}

test "mathematical_identity_properties" {
  // 数学恒等式测试
  let x = 42
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(-x, @azimuth.multiply(x, -1))
  @azimuth.assert_eq(0, @azimuth.subtract(x, x))
}

test "unicode_and_special_characters" {
  // Unicode 和特殊字符测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello,Москва!", @azimuth.greet("Москва"))
}