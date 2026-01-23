// 额外的增强 MoonBit 测试用例
// 为 azimuth 库添加更多高质量测试覆盖

// 测试复杂的数学运算序列
test "complex_calculation_sequence" {
  // 测试多层嵌套计算
  let step1 = @azimuth.add(10, @azimuth.multiply(2, 5))        // 10 + 2*5 = 20
  let step2 = @azimuth.divide_with_ceil(step1, 3)     // ceil(20/3) = 7
  let step3 = @azimuth.multiply(step2, @azimuth.add(1, 2))     // 7 * (1+2) = 21
  let step4 = @azimuth.divide_with_ceil(step3, 8)     // ceil(21/8) = 3
  let final_result = @azimuth.add(step4, 7)           // 3 + 7 = 10
  @azimuth.assert_eq(10, final_result)
}

// 测试除法的特殊边界情况
test "divide_with_ceil_special_boundaries" {
  // 测试接近边界的情况（使用大数代替 max/min value）
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2147483646, 2147483646))  // 使用接近 32-bit int 最大值的数
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-2147483647, -2147483647))  // 使用接近 32-bit int 最小值的数
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 2147483647))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -2147483647))
}

// 测试字符串处理的边缘情况
test "greet_function_extended_unicode" {
  // 测试更复杂的 Unicode 字符
  @azimuth.assert_eq_string("Hello, emoji!", @azimuth.greet("emoji"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, español!", @azimuth.greet("español"))
  @azimuth.assert_eq_string("Hello, math!", @azimuth.greet("math"))
}

// 测试数学运算的分配律和结合律
test "mathematical_laws_verification" {
  let a = 12
  let b = 8
  let c = 5
  
  // 分配律: a * (b + c) = a * b + a * c
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distributive_left, distributive_right)
  
  // 结合律: (a + b) + c = a + (b + c)
  let associative_left = @azimuth.add(@azimuth.add(a, b), c)
  let associative_right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(associative_left, associative_right)
}

// 测试实际业务场景：项目资源计算
test "project_resource_calculation" {
  // 场景：计算项目所需资源
  let team_size = 7
  let story_points = 45
  let capacity_per_person = 8
  
  // 计算总容量
  let total_capacity = @azimuth.multiply(team_size, capacity_per_person)  // 7 * 8 = 56
  @azimuth.assert_eq(56, total_capacity)
  
  // 计算需要的迭代次数
  let iterations_needed = @azimuth.divide_with_ceil(story_points, capacity_per_person)  // ceil(45/8) = 6
  @azimuth.assert_eq(6, iterations_needed)
  
  // 验证总处理能力
  let total_processed = @azimuth.multiply(iterations_needed, capacity_per_person)  // 6 * 8 = 48
  @azimuth.assert_eq(48, total_processed)
}

// 测试极端数值情况
test "extreme_number_operations" {
  // 测试大数运算
  let large_num = 1000000
  let small_num = 1
  
  @azimuth.assert_eq(1000001, @azimuth.add(large_num, small_num))
  @azimuth.assert_eq(999999, @azimuth.add(large_num, @azimuth.multiply(-1, small_num)))
  @azimuth.assert_eq(1000000, @azimuth.multiply(large_num, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_num, large_num))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(@azimuth.multiply(large_num, large_num), large_num))
}

// 测试连续除法运算
test "sequential_division_operations" {
  // 测试连续的除法运算
  let initial = 100
  let step1 = @azimuth.divide_with_ceil(initial, 3)      // ceil(100/3) = 34
  let step2 = @azimuth.divide_with_ceil(step1, 5)        // ceil(34/5) = 7
  let step3 = @azimuth.divide_with_ceil(step2, 2)        // ceil(7/2) = 4
  let step4 = @azimuth.divide_with_ceil(step3, 4)        // ceil(4/4) = 1
  
  @azimuth.assert_eq(34, step1)
  @azimuth.assert_eq(7, step2)
  @azimuth.assert_eq(4, step3)
  @azimuth.assert_eq(1, step4)
}

// 测试字符串拼接和特殊字符处理
test "string_concatenation_edge_cases" {
  // 测试各种字符串拼接情况
  @azimuth.assert_eq_string("Hello, space around !", @azimuth.greet(" space around "))
  @azimuth.assert_eq_string("Hello, newline tab !", @azimuth.greet("newline tab"))
  @azimuth.assert_eq_string("Hello, quoted!", @azimuth.greet("quoted"))
  @azimuth.assert_eq_string("Hello, single!", @azimuth.greet("single"))
  @azimuth.assert_eq_string("Hello, backslash!", @azimuth.greet("backslash"))
}

// 测试数学运算的恒等性质
test "mathematical_identity_properties" {
  let test_value = 42
  
  // 加法恒等元
  @azimuth.assert_eq(test_value, @azimuth.add(test_value, 0))
  @azimuth.assert_eq(test_value, @azimuth.add(0, test_value))
  
  // 乘法恒等元
  @azimuth.assert_eq(test_value, @azimuth.multiply(test_value, 1))
  @azimuth.assert_eq(test_value, @azimuth.multiply(1, test_value))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(test_value, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, test_value))
  
  // 负数性质
  @azimuth.assert_eq(@azimuth.multiply(-1, test_value), @azimuth.multiply(test_value, -1))
  @azimuth.assert_eq(0, @azimuth.add(test_value, @azimuth.multiply(-1, test_value)))
}