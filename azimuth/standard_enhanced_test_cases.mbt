// 标准 MoonBit 增强测试用例
// 测试 azimuth 库的核心功能

// 测试加法函数的数学性质
test add_mathematical_properties {
  // 测试加法交换律
  let x = 42
  let y = 17
  assert_eq(add(x, y), add(y, x))
  
  // 测试加法结合律
  let z = 23
  assert_eq(
    add(add(x, y), z),
    add(x, add(y, z))
  )
  
  // 测试加法单位元
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
}

// 测试乘法函数的数学性质
test multiply_mathematical_properties {
  // 测试乘法交换律
  let x = 12
  let y = 8
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 测试乘法结合律
  let z = 5
  assert_eq(
    multiply(multiply(x, y), z),
    multiply(x, multiply(y, z))
  )
  
  // 测试乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 测试乘法零元
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

// 测试分配律
test distributive_property {
  let a = 6
  let b = 4
  let c = 3
  
  // 测试乘法对加法的分配律
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

// 测试向上取整除法的边界情况
test divide_with_ceil_boundary_cases {
  // 测试除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极小值情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 测试负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-2, divide_with_ceil(5, -3))
}

// 测试字符串处理和国际化
test greet_internationalization {
  // 测试基本英文字符
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 测试中文字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 中国!", greet("中国"))
  
  // 测试欧洲字符
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, naïve!", greet("naïve"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  
  // 测试表情符号
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  
  // 测试特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

// 测试复杂业务逻辑场景
test business_logic_packaging {
  // 场景：计算装盒需求
  let total_items = 157
  let box_capacity = 24
  
  // 计算需要的盒子数量
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(11, remaining_space)
}

// 测试复杂计算流水线
test complex_calculation_pipeline {
  // 场景：项目资源计算
  let team_members = 8
  let hourly_rate = 50
  let hours_per_day = 8
  let project_days = 15
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_days))
  assert_eq(960, total_hours)
  
  // 计算总预算
  let total_budget = multiply(total_hours, hourly_rate)
  assert_eq(48000, total_budget)
  
  // 计算支付周期（每周期不超过10000）
  let payment_cycle_limit = 10000
  let payment_cycles = divide_with_ceil(total_budget, payment_cycle_limit)
  assert_eq(5, payment_cycles)
}

// 测试数学恒等式和性质
test mathematical_identities {
  let x = 42
  
  // 测试加法逆元
  assert_eq(0, add(x, multiply(x, -1)))
  
  // 测试减法性质
  assert_eq(x, subtract(x, 0))
  assert_eq(multiply(2, x), subtract(add(x, x), 0))
  
  // 测试乘法性质
  assert_eq(x, multiply(x, 1))
  assert_eq(multiply(x, -1), subtract(0, x))
}

// 测试错误处理和边界条件
test error_handling_boundary_conditions {
  // 测试极值运算
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试加法边界
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
  
  // 测试乘法边界
  assert_eq(max_int, multiply(max_int, 1))
  assert_eq(min_int, multiply(min_int, 1))
  assert_eq(0, multiply(max_int, 0))
  assert_eq(0, multiply(min_int, 0))
  
  // 测试除法边界
  assert_eq(1, divide_with_ceil(max_int, max_int))
  assert_eq(1, divide_with_ceil(min_int, min_int))
  assert_eq(0, divide_with_ceil(0, max_int))
}

// 测试真实世界应用场景
test real_world_activity_planning {
  // 场景：活动座位安排
  let participants = 87
  let tables_available = 8
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 验证桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, total_seats)
  
  // 验证座位充足
  assert_true(total_seats >= participants)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(3, remaining_seats)
}