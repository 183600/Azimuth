// Azimuth 综合单元测试
// 包含10个高质量的标准 MoonBit 测试用例

test "add_function_performance_validation" {
  // 加法函数性能验证测试
  // 测试连续加法运算的正确性
  let result = add(add(add(1, 2), add(3, 4)), add(5, 6))
  assert_eq(21, result)  // (1+2)+(3+4)+(5+6) = 3+7+11 = 21
  
  // 测试大数加法
  let large_result = add(500000, 500000)
  assert_eq(1000000, large_result)
}

test "multiply_function_zero_handling" {
  // 乘法函数零处理测试
  // 验证零元素的各种情况
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 123))
  assert_eq(0, multiply(123, 0))
  assert_eq(0, multiply(-123, 0))
  assert_eq(0, multiply(0, -123))
}

test "greet_function_empty_and_special_inputs" {
  // 问候函数空值和特殊输入测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \n!", greet("\n"))
  assert_eq_string("Hello, \t!", greet("\t"))
}

test "divide_with_ceil_negative_scenarios" {
  // 向上取整除法的负数场景测试
  // 测试被除数为负的情况
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(-3, divide_with_ceil(-8, 3))
  
  // 测试除数为负的情况
  assert_eq(-2, divide_with_ceil(5, -3))
  assert_eq(-1, divide_with_ceil(2, -3))
  assert_eq(-3, divide_with_ceil(8, -3))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 模拟实际业务场景：计算项目资源需求
  let team_size = 12
  let tasks_per_member = 8
  let total_tasks = multiply(team_size, tasks_per_member)
  
  let daily_capacity = 15
  let days_needed = divide_with_ceil(total_tasks, daily_capacity)
  
  assert_eq(96, total_tasks)  // 12 * 8 = 96
  assert_eq(7, days_needed)   // ceil(96/15) = 7
  
  // 验证总处理能力
  let total_capacity = multiply(days_needed, daily_capacity)
  assert_eq(105, total_capacity)
}

test "mathematical_identity_verification" {
  // 数学恒等式验证测试
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  
  // 负数恒等式
  assert_eq(-x, multiply(x, -1))
  assert_eq(-x, multiply(-1, x))
}

test "resource_allocation_boundary_test" {
  // 资源分配边界测试
  // 场景：计算会议室需求
  let attendees = 95
  let room_capacity = 20
  let rooms_needed = divide_with_ceil(attendees, room_capacity)
  
  assert_eq(5, rooms_needed)  // ceil(95/20) = 5
  
  // 验证总容量
  let total_capacity = multiply(rooms_needed, room_capacity)
  assert_eq(100, total_capacity)
  
  // 验证剩余容量
  let remaining_capacity = subtract(total_capacity, attendees)
  assert_eq(5, remaining_capacity)
}

test "string_processing_unicode_support" {
  // 字符串处理Unicode支持测试
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Français!", greet("Français"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🎉!", greet("🎉"))
  assert_eq_string("Hello, 💻!", greet("💻"))
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  assert_eq(0, divide_with_ceil(-1000000, 0))
}

test "algorithm_efficiency_validation" {
  // 算法效率验证测试
  // 模拟不同算法复杂度的场景
  let input_size = 1000
  
  // O(1) 算法模拟
  let constant_time = multiply(1, input_size)
  assert_eq(1000, constant_time)
  
  // O(n) 算法模拟
  let linear_time = multiply(input_size, 2)
  assert_eq(2000, linear_time)
  
  // O(log n) 算法模拟（使用向上取整）
  let logarithmic_time = divide_with_ceil(input_size, 100)
  assert_eq(10, logarithmic_time)
  
  // 验证效率差异
  let efficiency_ratio = divide_with_ceil(linear_time, logarithmic_time)
  assert_eq(200, efficiency_ratio)
}

// 辅助函数
fn subtract(a : Int, b : Int) -> Int {
  a - b
}