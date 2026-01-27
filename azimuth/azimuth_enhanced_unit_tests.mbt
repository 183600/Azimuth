// 新增的高质量 MoonBit 测试用例
// 包含10个全面的测试用例，覆盖各种场景和边缘情况

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
}

test "divide_with_ceil_fractional_precision" {
  // 测试向上取整除法的小数精度处理
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(3, divide_with_ceil(5, 2))  // 2.5 -> 3
  assert_eq(4, divide_with_ceil(7, 2))  // 3.5 -> 4
  assert_eq(1, divide_with_ceil(1, 2))  // 0.5 -> 1
  assert_eq(0, divide_with_ceil(0, 2))  // 0 -> 0
}

test "complex_calculation_workflow" {
  // 测试复杂计算工作流
  // 场景：计算项目资源分配
  let team_size = 12
  let projects = 5
  let hours_per_project = 8
  let days_needed = divide_with_ceil(
    multiply(team_size, projects),
    multiply(hours_per_project, 3)
  )
  assert_eq(2, days_needed)  // ceil(12*5/(8*3)) = ceil(60/24) = 3
}

test "mathematical_identity_verification" {
  // 验证数学恒等式
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(0, subtract(x, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
  assert_eq(-x, multiply(x, -1))
}

test "string_input_validation" {
  // 测试字符串输入验证
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, space!", greet(" "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
  assert_eq_string("Hello, 123456!", greet("123456"))
  assert_eq_string("Hello, MixedCase!", greet("MixedCase"))
}

test "negative_number_arithmetic" {
  // 负数算术运算测试
  let a = -10
  let b = -5
  
  assert_eq(-15, add(a, b))
  assert_eq(50, multiply(a, b))
  assert_eq(-5, subtract(a, b))
  assert_eq(2, divide_with_ceil(a, b))
  assert_eq(-2, divide_with_ceil(a, 5))
}

test "resource_planning_scenario" {
  // 资源规划场景测试
  // 场景：计算活动所需的房间数量
  let participants = 150
  let rooms_available = 7
  let capacity_per_room = 25
  
  // 计算需要的房间数量
  let rooms_needed = divide_with_ceil(participants, capacity_per_room)
  assert_eq(6, rooms_needed)  // ceil(150/25) = 6
  
  // 验证房间足够
  assert_true(rooms_needed <= rooms_available)
  
  // 计算剩余容量
  let total_capacity = multiply(rooms_needed, capacity_per_room)
  let remaining_capacity = subtract(total_capacity, participants)
  assert_eq(0, remaining_capacity)  // 6*25 - 150 = 0
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试极值和边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
}

test "sequential_operation_chain" {
  // 连续操作链测试
  let initial = 100
  
  // 操作链：除法 -> 加法 -> 乘法 -> 减法
  let step1 = divide_with_ceil(initial, 3)      // ceil(100/3) = 34
  let step2 = add(step1, 10)                    // 34 + 10 = 44
  let step3 = multiply(step2, 2)                // 44 * 2 = 88
  let result = subtract(step3, 25)              // 88 - 25 = 63
  
  assert_eq(63, result)
}

test "error_handling_resilience" {
  // 错误处理和恢复能力测试
  // 测试各种可能导致错误的情况
  
  // 除零错误的多种变体
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, x!", greet("x"))
}