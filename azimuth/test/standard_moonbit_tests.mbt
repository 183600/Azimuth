// 标准 MoonBit 测试用例集
// 为 azimuth 库提供全面的功能测试

// 测试1: 加法函数的综合测试
test "add_function_comprehensive" {
  // 基本加法测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  
  // 混合符号测试
  @azimuth.assert_eq(4, @azimuth.add(-7, 11))
  @azimuth.assert_eq(-4, @azimuth.add(7, -11))
  
  // 大数测试
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  
  // 边界值测试
  @azimuth.assert_eq(1, @azimuth.add(2147483647, -2147483646))
  @azimuth.assert_eq(-1, @azimuth.add(-2147483648, 2147483647))
}

// 测试2: 乘法函数的边界情况测试
test "multiply_function_edge_cases" {
  // 基本乘法测试
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(-21, @azimuth.multiply(7, -3))
  @azimuth.assert_eq(21, @azimuth.multiply(-7, -3))
  
  // 单位元测试
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-1, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(-1, -12345))
  
  // 大数测试
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

// 测试3: 问候函数的国际化字符测试
test "greet_function_international" {
  // 基本英文测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 中文字符测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  
  // Unicode 字符测试
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  
  // 空字符串和特殊字符测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, !@#$%^!", @azimuth.greet("!@#$%^"))
}

// 测试4: 向上取整除法的业务逻辑测试
test "divide_with_ceil_business_logic" {
  // 基本除法测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  
  // 除零安全测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 边界值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}

// 测试5: 复杂计算流水线测试
test "complex_calculation_pipeline" {
  // 场景：计算项目完成时间和资源需求
  let tasks = 25
  let people = 4
  let tasks_per_person_per_day = 2
  
  // 计算每日处理能力
  let daily_capacity = @azimuth.multiply(people, tasks_per_person_per_day)
  @azimuth.assert_eq(8, daily_capacity)
  
  // 计算需要的天数
  let days_needed = @azimuth.divide_with_ceil(tasks, daily_capacity)
  @azimuth.assert_eq(4, days_needed)  // ceil(25/8) = 4
  
  // 验证总处理能力
  let total_capacity = @azimuth.multiply(days_needed, daily_capacity)
  @azimuth.assert_eq(32, total_capacity)
  
  // 计算剩余处理能力
  let remaining_capacity = @azimuth.add(total_capacity, -tasks)
  @azimuth.assert_eq(7, remaining_capacity)
}

// 测试6: 数学性质验证测试
test "mathematical_properties_verification" {
  let x = 42
  let y = 17
  let z = 5
  
  // 加法交换律验证
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  @azimuth.assert_eq(@azimuth.add(x, z), @azimuth.add(z, x))
  
  // 乘法交换律验证
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  @azimuth.assert_eq(@azimuth.multiply(x, z), @azimuth.multiply(z, x))
  
  // 分配律验证: a * (b + c) = a * b + a * c
  let distributive_left = @azimuth.multiply(x, @azimuth.add(y, z))
  let distributive_right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(distributive_left, distributive_right)
  
  // 结合律验证: (a + b) + c = a + (b + c)
  let associative_left = @azimuth.add(@azimuth.add(x, y), z)
  let associative_right = @azimuth.add(x, @azimuth.add(y, z))
  @azimuth.assert_eq(associative_left, associative_right)
}

// 测试7: 资源分配优化测试
test "resource_allocation_optimization" {
  // 场景：预算分配问题
  let total_budget = 10000
  let team_members = 6
  let emergency_reserve = 500
  
  // 计算可分配预算
  let available_budget = @azimuth.add(total_budget, -emergency_reserve)
  @azimuth.assert_eq(9500, available_budget)
  
  // 计算每人最大份额（向上取整确保覆盖所有情况）
  let max_share = @azimuth.divide_with_ceil(available_budget, team_members)
  @azimuth.assert_eq(1584, max_share)  // ceil(9500/6) = 1584
  
  // 验证总分配金额
  let total_allocated = @azimuth.multiply(max_share, team_members)
  @azimuth.assert_eq(9504, total_allocated)
  
  // 验证是否超出预算
  let budget_exceeded = total_allocated > available_budget
  @azimuth.assert_eq(1, if budget_exceeded { 1 } else { 0 })
  
  // 计算超出金额
  let excess_amount = @azimuth.add(total_allocated, -available_budget)
  @azimuth.assert_eq(4, excess_amount)
}

// 测试8: 错误处理和边界条件测试
test "error_handling_boundary_conditions" {
  // 极值测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 加法极值测试
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
  
  // 乘法极值测试
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
  @azimuth.assert_eq(max_int, @azimuth.multiply(max_int, 1))
  @azimuth.assert_eq(min_int, @azimuth.multiply(min_int, 1))
  
  // 除法极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(max_int, max_int))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(min_int, min_int))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, max_int))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, min_int))
  
  // 除零安全测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(min_int, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

// 测试9: 真实世界应用场景测试
test "real_world_application_scenarios" {
  // 场景1：活动座位安排
  let participants = 87
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.add(total_seats, -participants)
  @azimuth.assert_eq(1, remaining_seats)
  
  // 场景2：运输车辆计算
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = @azimuth.divide_with_ceil(packages, capacity_per_truck)
  @azimuth.assert_eq(5, trucks_needed)  // ceil(47/10) = 5
  
  // 验证总运力
  let capacity = @azimuth.multiply(trucks_needed, capacity_per_truck)
  @azimuth.assert_eq(50, capacity)
}

// 测试10: 字符串处理和断言函数测试
test "string_processing_assertions" {
  // 测试各种字符串输入
  @azimuth.assert_eq_string("Hello, single!", @azimuth.greet("single"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, mixedCASE!", @azimuth.greet("mixedCASE"))
  @azimuth.assert_eq_string("Hello, camelCase!", @azimuth.greet("camelCase"))
  @azimuth.assert_eq_string("Hello, snake_case!", @azimuth.greet("snake_case"))
  
  // 测试特殊字符和符号
  @azimuth.assert_eq_string("Hello, parentheses()!", @azimuth.greet("parentheses()"))
  @azimuth.assert_eq_string("Hello, brackets[]!", @azimuth.greet("brackets[]"))
  @azimuth.assert_eq_string("Hello, braces{}!", @azimuth.greet("braces{}"))
  @azimuth.assert_eq_string("Hello, quotes\"\"!", @azimuth.greet("quotes\"\""))
  
  // 测试断言函数
  @azimuth.assert_eq(1, if true { 1 } else { 0 })
  @azimuth.assert_eq(0, if false { 1 } else { 0 })
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq_string("Hello, test!", @azimuth.greet("test"))
}