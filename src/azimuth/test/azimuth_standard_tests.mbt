// Azimuth 项目标准测试用例
// 包含10个高质量的 MoonBit 测试用例

// 测试1: 基本算术运算功能
test "basic_arithmetic_functions" {
  // 测试加法运算
  assert_eq(8, add(5, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  
  // 测试乘法运算
  assert_eq(15, multiply(5, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-5, 3))
  
  // 测试减法运算
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

// 测试2: 向上取整除法的核心功能
test "divide_with_ceil_core_functionality" {
  // 基本除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  
  // 负数除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

// 测试3: 字符串问候功能
test "greet_function_functionality" {
  // 基本问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 特殊字符和Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  
  // 边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, space!", greet("space"))
}

// 测试4: 数学交换律验证
test "mathematical_commutative_laws" {
  // 加法交换律验证
  assert_eq(add(12, 8), add(8, 12))
  assert_eq(add(-5, 15), add(15, -5))
  
  // 乘法交换律验证
  assert_eq(multiply(12, 8), multiply(8, 12))
  assert_eq(multiply(-5, 15), multiply(15, -5))
}

// 测试5: 零元素性质测试
test "zero_element_properties" {
  // 加法零元素性质
  assert_eq(7, add(7, 0))
  assert_eq(0, add(0, 0))
  assert_eq(-7, add(-7, 0))
  
  // 乘法零元素性质
  assert_eq(0, multiply(7, 0))
  assert_eq(0, multiply(0, 7))
  assert_eq(0, multiply(0, 0))
}

// 测试6: 复合运算表达式
test "compound_arithmetic_expressions" {
  // 组合运算测试
  let result1 = add(multiply(3, 4), multiply(5, 2))
  assert_eq(22, result1)  // 3*4 + 5*2 = 12 + 10 = 22
  
  // 嵌套运算测试
  let result2 = multiply(add(6, 4), subtract(15, 5))
  assert_eq(100, result2)  // (6+4)*(15-5) = 10*10 = 100
  
  // 混合运算测试
  let result3 = divide_with_ceil(multiply(8, 3), add(5, 4))
  assert_eq(3, result3)  // ceil(24/9) = 3
}

// 测试7: 业务场景 - 库存包装计算
test "business_inventory_packaging" {
  // 包装计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

// 测试8: 边界值和极值测试
test "boundary_and_extreme_values" {
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1, divide_with_ceil(99, 100))
  
  // 大数运算测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
  
  // 极值除法测试
  assert_eq(1000, divide_with_ceil(10000, 10))
  assert_eq(1001, divide_with_ceil(10001, 10))
}

// 测试9: 连续计算工作流
test "sequential_calculation_workflow" {
  // 模拟实际业务计算流程
  let initial_value = 20
  
  // 步骤1：增加50%
  let step1 = add(initial_value, divide_with_ceil(initial_value, 2))
  
  // 步骤2：翻倍
  let step2 = multiply(step1, 2)
  
  // 步骤3：分配到团队
  let team_size = 7
  let step3 = divide_with_ceil(step2, team_size)
  
  // 步骤4：加上缓冲
  let buffer = 3
  let final_result = add(step3, buffer)
  
  // 验证最终结果：((20 + 10) * 2) / 7 + 3 = ceil(60/7) + 3 = 9 + 3 = 12
  assert_eq(12, final_result)
}

// 测试10: 错误处理和健壮性验证
test "error_handling_and_robustness" {
  // 除零错误全面测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(99999, 0))
  
  // 极值运算健壮性测试
  let large_number = 1000000
  assert_eq(0, multiply(large_number, 0))
  assert_eq(large_number, add(large_number, 0))
  
  // 字符串边界情况健壮性测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, x!", greet("x"))
  assert_eq_string("Hello, 123!", greet("123"))
}