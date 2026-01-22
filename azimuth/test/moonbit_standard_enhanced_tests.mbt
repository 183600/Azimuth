// 标准 MoonBit 测试用例 - 增强版
// 测试 azimuth 库的核心功能和边界情况

test "divide_with_ceil_zero_divisor" {
  // 测试除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2147483648, 0))
}

test "large_number_operations_stability" {
  // 测试大数运算的稳定性
  let large_num1 = 100000
  let large_num2 = 200000
  let large_num3 = -150000
  
  // 大数加法
  @azimuth.assert_eq(300000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(-50000, @azimuth.add(large_num1, large_num3))
  
  // 大数乘法
  @azimuth.assert_eq(20000000000, @azimuth.multiply(large_num1, large_num2))
  @azimuth.assert_eq(-15000000000, @azimuth.multiply(large_num1, large_num3))
  
  // 大数除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(large_num2, large_num1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_num3, -large_num1))
}

test "string_special_characters_comprehensive" {
  // 测试字符串中的特殊字符处理
  @azimuth.assert_eq_string("Hello, \n\t\r!", @azimuth.greet("\n\t\r"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  @azimuth.assert_eq_string("Hello, \\backslash!", @azimuth.greet("\\backslash"))
  @azimuth.assert_eq_string("Hello, mixed123!@#!", @azimuth.greet("mixed123!@#"))
  @azimuth.assert_eq_string("Hello, 🌙🚀⭐!", @azimuth.greet("🌙🚀⭐"))
}

test "mathematical_associative_laws" {
  // 测试数学运算的结合律
  let a = 15
  let b = 25
  let c = 35
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let mult_left = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let mult_right = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(mult_left, mult_right)
}

test "real_world_business_calculations" {
  // 测试实际业务场景计算
  
  // 场景1: 计算含税价格
  let base_price = 1000
  let tax_rate = 8  // 8%税率，以整数形式存储
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(base_price, tax_rate), 100)
  let total_price = @azimuth.add(base_price, tax_amount)
  @azimuth.assert_eq(1080, total_price)
  
  // 场景2: 计算折扣价格
  let original_price = 2500
  let discount_percentage = 15  // 15%折扣
  let discount_amount = @azimuth.divide_with_ceil(@azimuth.multiply(original_price, discount_percentage), 100)
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(2125, final_price)
  
  // 场景3: 计算批量采购所需的包装数量
  let total_items = 987
  let items_per_package = 50
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  @azimuth.assert_eq(20, packages_needed)
}

test "boundary_value_combinations" {
  // 测试边界值的组合运算
  let max_val = 2147483647
  let min_val = -2147483648
  let mid_val = 0
  
  // 边界值加法组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, mid_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, mid_val))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法组合
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, mid_val))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, mid_val))
  @azimuth.assert_eq(0, @azimuth.multiply(mid_val, mid_val))
  
  // 边界值除法组合
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(max_val, max_val))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(min_val, min_val))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(mid_val, max_val))
}

test "negative_number_operations_comprehensive" {
  // 测试负数运算的各种组合
  let neg1 = -15
  let neg2 = -25
  let pos1 = 20
  let pos2 = 30
  
  // 负数加法
  @azimuth.assert_eq(-40, @azimuth.add(neg1, neg2))
  @azimuth.assert_eq(5, @azimuth.add(neg1, pos1))
  @azimuth.assert_eq(-5, @azimuth.add(neg2, pos1))
  
  // 负数乘法
  @azimuth.assert_eq(375, @azimuth.multiply(neg1, neg2))
  @azimuth.assert_eq(-300, @azimuth.multiply(neg1, pos2))
  @azimuth.assert_eq(-500, @azimuth.multiply(neg2, pos2))
  
  // 负数除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(neg1, neg2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(neg1, pos1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(pos1, neg2))
}

test "function_chaining_operations" {
  // 测试函数链式调用
  let initial_value = 10
  
  // 链式加法操作
  let chain_add = @azimuth.add(@azimuth.add(@azimuth.add(initial_value, 5), 10), 15)
  @azimuth.assert_eq(40, chain_add)
  
  // 链式乘法操作
  let chain_multiply = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(initial_value, 2), 3), 4)
  @azimuth.assert_eq(240, chain_multiply)
  
  // 混合链式操作
  let mixed_chain = @azimuth.multiply(@azimuth.add(initial_value, 5), @azimuth.divide_with_ceil(20, 4))
  @azimuth.assert_eq(30, mixed_chain)
  
  // 字符串链式处理
  let greet_chain1 = @azimuth.greet("User")
  let greet_chain2 = @azimuth.greet(@azimuth.greet("Admin"))
  @azimuth.assert_eq_string("Hello, User!", greet_chain1)
  @azimuth.assert_eq_string("Hello, Hello, Admin!!", greet_chain2)
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  
  // 场景1: 计算复合利率
  let principal = 10000
  let rate = 5  // 5%利率
  let years = 3
  let year1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate), 100)
  let year1_total = @azimuth.add(principal, year1_interest)
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year1_total, rate), 100)
  let year2_total = @azimuth.add(year1_total, year2_interest)
  let year3_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year2_total, rate), 100)
  let final_amount = @azimuth.add(year2_total, year3_interest)
  @azimuth.assert_eq(11576, final_amount)
  
  // 场景2: 计算资源分配
  let total_resources = 1000
  let team_a_size = 3
  let team_b_size = 5
  let team_c_size = 2
  let total_team_size = @azimuth.add(@azimuth.add(team_a_size, team_b_size), team_c_size)
  let resources_per_person = @azimuth.divide_with_ceil(total_resources, total_team_size)
  let team_a_resources = @azimuth.multiply(resources_per_person, team_a_size)
  let team_b_resources = @azimuth.multiply(resources_per_person, team_b_size)
  let team_c_resources = @azimuth.multiply(resources_per_person, team_c_size)
  @azimuth.assert_eq(100, resources_per_person)
  @azimuth.assert_eq(300, team_a_resources)
  @azimuth.assert_eq(500, team_b_resources)
  @azimuth.assert_eq(200, team_c_resources)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  
  // 测试极值运算
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 极值加法溢出保护
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, max_int))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, min_int))
  
  // 极值乘法溢出保护
  @azimuth.assert_eq(max_int, @azimuth.multiply(max_int, 2))
  @azimuth.assert_eq(min_int, @azimuth.multiply(min_int, 2))
  
  // 特殊除法情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, max_int))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, min_int))
  
  // 空字符串和特殊字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, null!", @azimuth.greet("null"))
  @azimuth.assert_eq_string("Hello, undefined!", @azimuth.greet("undefined"))
}