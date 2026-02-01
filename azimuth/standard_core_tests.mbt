// 标准 MoonBit 测试用例 - Azimuth 核心功能测试
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_addition_functionality" {
  // 基本加法功能测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-3, 2))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "divide_with_ceil_positive_numbers" {
  // 向上取整除法正数测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))  // 除零处理
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-10, 3))
}

test "greet_function_basic_tests" {
  // 基本问候功能测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "subtract_function_tests" {
  // 减法函数测试
  @azimuth.assert_eq(2, @azimuth.subtract(5, 3))
  @azimuth.assert_eq(-2, @azimuth.subtract(3, 5))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(0, -10))
}

test "mathematical_commutative_properties" {
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

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let team = 4
  let tasks_per_person = 8
  let total_tasks = @azimuth.multiply(team, tasks_per_person)
  
  let sprint_capacity = 15
  let sprints = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(32, total_tasks)  // 4 * 8 = 32
  @azimuth.assert_eq(3, sprints)       // ceil(32/15) = 3
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
}