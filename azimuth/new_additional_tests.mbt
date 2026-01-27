// 额外的 MoonBit 测试用例
// 为 azimuth 库添加更多测试覆盖

// 测试数学运算的结合律
test "mathematical_associativity" {
  let a = 5
  let b = 3
  let c = 2
  
  // 加法结合律：(a + b) + c = a + (b + c)
  assert_eq(
    add(add(a, b), c),
    add(a, add(b, c))
  )
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  assert_eq(
    multiply(multiply(a, b), c),
    multiply(a, multiply(b, c))
  )
}

// 测试除法的边界情况
test "division_boundary_cases" {
  // 测试除数为1的情况
  assert_eq(10, divide_with_ceil(10, 1))
  assert_eq(-10, divide_with_ceil(-10, 1))
  
  // 测试除数为-1的情况
  assert_eq(-10, divide_with_ceil(10, -1))
  assert_eq(10, divide_with_ceil(-10, -1))
  
  // 测试被除数为0的情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
}

// 测试字符串处理的边缘情况
test "string_edge_cases" {
  // 测试包含空格的字符串
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello,  leading space!", greet(" leading space"))
  assert_eq_string("Hello, trailing space !", greet("trailing space "))
  
  // 测试特殊字符
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
}

// 测试复杂业务逻辑计算
test "business_logic_calculation" {
  // 场景：计算项目资源需求
  let team_members = 8
  let hours_per_day = 8
  let working_days = 5
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  
  // 计算需要的周数（每周40小时）
  let hours_per_week = 40
  let weeks_needed = divide_with_ceil(total_hours, hours_per_week)
  
  assert_eq(8, weeks_needed)  // ceil(320/40) = 8
}

// 测试数学恒等式
test "mathematical_identities" {
  let x = 15
  
  // 加法恒等式：x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式：x * 1 = x
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零律：x * 0 = 0
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

// 测试负数运算的全面性
test "negative_numbers_comprehensive" {
  // 负数加法
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(5, add(-10, 15))
  
  // 负数乘法
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(-25, multiply(5, -5))
  
  // 负数除法
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(2, divide_with_ceil(-7, -3))
}

// 测试大数运算
test "large_number_operations" {
  // 大数加法
  assert_eq(2000000, add(1000000, 1000000))
  
  // 大数乘法
  assert_eq(1000000, multiply(1000, 1000))
  
  // 大数除法
  assert_eq(1001, divide_with_ceil(1000000, 999))
  assert_eq(1000, divide_with_ceil(1000000, 1000))
}

// 测试连续运算的准确性
test "sequential_operations" {
  let initial = 10
  
  // 连续加法
  let result1 = add(add(initial, 5), 3)
  assert_eq(18, result1)
  
  // 连续乘法
  let result2 = multiply(multiply(initial, 2), 3)
  assert_eq(60, result2)
  
  // 混合运算
  let result3 = divide_with_ceil(
    multiply(add(initial, 5), 2),
    3
  )
  assert_eq(10, result3)  // ceil((10+5)*2/3) = ceil(30/3) = 10
}

// 测试Unicode字符处理
test "unicode_handling" {
  // 测试各种Unicode字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

// 测试实际应用场景
test "real_world_scenario" {
  // 场景：计算运输需求
  let total_weight = 1250  // kg
  let truck_capacity = 300  // kg
  let trucks_needed = divide_with_ceil(total_weight, truck_capacity)
  
  assert_eq(5, trucks_needed)  // ceil(1250/300) = 5
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(1500, total_capacity)
  
  // 计算剩余运力
  let remaining_capacity = subtract(total_capacity, total_weight)
  assert_eq(250, remaining_capacity)
}