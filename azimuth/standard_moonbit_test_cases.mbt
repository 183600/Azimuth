// 标准 MoonBit 测试用例 - 10个高质量测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_addition" {
  // 基础算术加法测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(5, add(-2, 7))
}

test "basic_arithmetic_multiplication" {
  // 基础算术乘法测试
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(7, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "greeting_function_basic" {
  // 问候函数基础测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 2)), add(multiply(x, y), multiply(x, 2)))
}

test "edge_case_handling" {
  // 边界情况处理测试
  // 零值处理
  assert_eq(7, add(7, 0))
  assert_eq(0, multiply(7, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 负数处理
  assert_eq(-2, add(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(3, divide_with_ceil(-10, -4))
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "real_world_scenarios" {
  // 真实世界场景测试
  // 场景：计算需要的运输车辆
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(50, total_capacity)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源规划
  let team_size = 8
  let tasks_per_person = 5
  let sprint_capacity = 15
  
  // 计算总任务数
  let total_tasks = multiply(team_size, tasks_per_person)
  assert_eq(40, total_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(3, sprints_needed)
}

test "performance_and_large_numbers" {
  // 性能和大数测试
  // 大数运算
  assert_eq(20000, add(10000, 10000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  
  // 连续运算性能测试
  let base = 100
  let result = divide_with_ceil(multiply(add(base, 50), 3), 25)
  assert_eq(18, result)  // ceil((100+50)*3/25) = ceil(450/25) = 18
}