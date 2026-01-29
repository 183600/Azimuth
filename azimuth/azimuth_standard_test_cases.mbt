// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法编写的测试用例

// 导入被测试的模块
use azimuth

test "add_function_positive_numbers" {
  // 测试正数加法
  azimuth.assert_eq(10, azimuth.add(3, 7))
  azimuth.assert_eq(100, azimuth.add(50, 50))
  azimuth.assert_eq(0, azimuth.add(0, 0))
}

test "add_function_negative_numbers" {
  // 测试负数加法
  azimuth.assert_eq(-10, azimuth.add(-3, -7))
  azimuth.assert_eq(0, azimuth.add(-5, 5))
  azimuth.assert_eq(-2, azimuth.add(-7, 5))
}

test "multiply_function_various_cases" {
  // 测试乘法函数的各种情况
  azimuth.assert_eq(15, azimuth.multiply(3, 5))
  azimuth.assert_eq(0, azimuth.multiply(0, 100))
  azimuth.assert_eq(-15, azimuth.multiply(-3, 5))
  azimuth.assert_eq(15, azimuth.multiply(-3, -5))
}

test "divide_with_ceil_function_basic" {
  // 测试向上取整除法的基本功能
  azimuth.assert_eq(4, azimuth.divide_with_ceil(10, 3))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(9, 3))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 1))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  azimuth.assert_eq(4, azimuth.divide_with_ceil(-10, -3))
  azimuth.assert_eq(-4, azimuth.divide_with_ceil(10, -3))
}

test "subtract_function_basic" {
  // 测试减法函数（使用加法和负数模拟）
  azimuth.assert_eq(5, azimuth.add(10, -5))
  azimuth.assert_eq(-5, azimuth.add(5, -10))
  azimuth.assert_eq(0, azimuth.add(5, -5))
  azimuth.assert_eq(15, azimuth.add(10, 5))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  azimuth.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  azimuth.assert_eq_string("Hello, World!", azimuth.greet("World"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  azimuth.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  azimuth.assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  azimuth.assert_eq_string("Hello, @#$%!", azimuth.greet("@#$%"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 场景：计算项目资源需求
  let team_members = 8
  let tasks_per_member = 5
  let total_tasks = azimuth.multiply(team_members, tasks_per_member)
  let tasks_per_sprint = 15
  let sprints_needed = azimuth.divide_with_ceil(total_tasks, tasks_per_sprint)
  
  azimuth.assert_eq(40, total_tasks)
  azimuth.assert_eq(3, sprints_needed)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  azimuth.assert_eq(azimuth.add(x, y), azimuth.add(y, x))
  
  // 乘法交换律
  azimuth.assert_eq(azimuth.multiply(x, y), azimuth.multiply(y, x))
  
  // 减法验证（使用加法和负数模拟）
  azimuth.assert_eq(0, azimuth.add(x, -x))
  azimuth.assert_eq(x, azimuth.add(azimuth.add(x, y), -y))
}